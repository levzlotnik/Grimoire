from textwrap import dedent
from typing import (
    Generic,
    List,
    Dict,
    Any,
    Callable,
    Optional,
    Type,
    TypeVar,
    Union,
    Literal,
)
from pydantic import BaseModel, Field, ValidationError
import json
import re

from llm_provider import LLMProvider
from common import ToolError


class LLMFormatError(Exception):
    """Raised when LLM output doesn't match expected format"""

    pass


class Tool(BaseModel):
    """Definition of a tool the LLM can use"""

    name: str = Field(..., description="Name of the tool")
    description: str = Field(..., description="Description of the tool")
    parameters: Dict[str, str] = Field(..., description="Parameters of the tool")
    return_schema: Union[Dict[str, Any], str, None] = Field(
        None, description="Schema of the return value of the tool"
    )
    function: Callable[..., BaseModel] = Field(..., description="Function to call")

    def _sys_prompt(self) -> str:
        return_schema = (
            json.dumps(self.return_schema, indent=2) if self.return_schema else "None"
        )
        return dedent(
            f"""
            Tool: {self.name}
            Description: {self.description}
            Parameters: {json.dumps(self.parameters, indent=2)}
            Return schema: {return_schema}
        """
        ).strip()


def make_tool(fn: Callable[..., BaseModel]) -> Tool:
    """Create a tool from a function"""
    annotations = {k: str(v) for k, v in fn.__annotations__.items() if k != "return"}
    if "return" in fn.__annotations__:
        return_type = fn.__annotations__["return"]
        if issubclass(return_type, BaseModel):
            return_schema = return_type.model_json_schema(mode="serialization")
        else:
            return_schema = str(return_type)
    else:
        return_schema = None
    return Tool(
        name=fn.__name__,
        description=fn.__doc__,
        parameters=annotations,
        return_schema=return_schema,
        function=fn,
    )


class NaturalLanguage(BaseModel):
    """A natural language thought"""

    tag: Literal["natural_language"] = "natural_language"
    natural_language: str = Field(..., description="The natural language thought")


class ToolCall(BaseModel):
    """A request to use a specific tool"""

    tag: Literal["tool_call"] = "tool_call"
    tool_name: str = Field(..., description="Name of the tool to use")
    parameters: Dict[str, Any] = Field(
        ..., description="Parameters to pass to the tool"
    )
    reason: str = Field(..., description="Explanation of why this tool is being used")


class ToolCallResult(BaseModel):
    """Result from a tool call"""

    tag: Literal["tool_call_result"] = "tool_call_result"
    result: Any = Field(..., description="The result returned by the tool")


class RetVal(BaseModel):
    """A conclusion from the LLM"""

    tag: Literal["return"] = "return"  # Changed from "conclusion"
    retval: str = Field(..., description="The return value from this chain of thought")


class Thought(BaseModel):
    """A single thought in the chain of reasoning"""

    data: Union[NaturalLanguage, ToolCall, ToolCallResult, RetVal] = Field(
        ..., description="The data of the thought", discriminator="tag"
    )
    step_number: int = Field(..., description="Step number in the reasoning chain")


class ChainOfThought(BaseModel):
    """A chain of thought for the assistant"""

    thoughts: List[Thought] = Field(
        ..., description="A list of thoughts and tool interactions"
    )


def _try_parse_tool_call(content: str) -> Optional[ToolCall]:
    """Try to parse a tool call from the content"""
    try:
        data = ToolCall.model_validate_json(content)
        return ToolCall(
            tool_name=data.tool_name,
            parameters=data.parameters,
            reason=data.reason,
        )
    except (json.JSONDecodeError, ValidationError):
        return None


def parse_next_steps(text: str) -> List[Thought]:
    """Parse steps from the LLM response until a tool call or return is found.
    Returns all steps including the final tool call or return.

    Args:
        text: Text containing one or more steps

    Returns:
        List of Thoughts, where the last thought is either a tool call or return

    Raises:
        LLMFormatError: If no valid steps are found or if steps are not properly formatted
    """
    steps: List[Thought] = []
    # Split on STEP markers, capturing the step number
    pattern = re.compile(r"STEP (\d+):(.*?)(?=STEP \d+:|$)", re.DOTALL)
    matches = pattern.findall(text)

    if not matches:
        raise LLMFormatError("No valid steps found in response")

    # Process all steps except the last one as natural language
    for step_num, content in matches[:-1]:
        content = content.strip()
        if content:
            maybe_tool_call = _try_parse_tool_call(content)
            if maybe_tool_call:
                thought = Thought(
                    data=maybe_tool_call,
                    step_number=int(step_num),
                )
            else:
                thought = Thought(
                    data=NaturalLanguage(natural_language=content),
                    step_number=int(step_num),
                )
            steps.append(thought)

    # Process the final step - must be either a tool call or lead to a return
    final_step_num, final_content = matches[-1]
    final_content = final_content.strip()
    if not final_content:
        raise LLMFormatError("Empty final step")

    # First try to parse as tool call
    maybe_tool_call = _try_parse_tool_call(final_content)
    if maybe_tool_call:
        steps.append(
            Thought(
                data=maybe_tool_call,
                step_number=int(final_step_num),
            )
        )
        return steps
    else:
        return_pattern = re.compile(r"return:(.*?)$", re.DOTALL)
        return_match = return_pattern.search(text)
        if return_match:
            conclusion = return_match.group(1).strip()
            steps.append(
                Thought(
                    data=RetVal(retval=conclusion),
                    step_number=int(final_step_num),
                )
            )
            return steps

    # If we get here, the final step was neither a tool call nor a return
    raise LLMFormatError("Final step must be a tool call or return statement")


class AssistantResponse(BaseModel):
    """Structured response from the assistant"""

    tool_calls: List[ToolCall] = Field(
        default_factory=list, description="List of tools to call"
    )
    message: str = Field(..., description="Natural language response to the user")


RetvalModel = TypeVar("RetvalModel", bound=Union[str, BaseModel])


class ProcessResult(BaseModel, Generic[RetvalModel]):
    """Result of processing a message through the LLM"""

    thoughts: List[Thought] = Field(
        ..., description="Chain of thoughts leading to conclusion"
    )
    retval: RetvalModel = Field(..., description="Final conclusion/answer")


class Toolset(BaseModel):
    """A set of tools available to the LLM"""

    tools: Dict[str, Tool] = Field(..., description="Dictionary of available tools")

    def to_prompt(self) -> str:
        """Convert tools to a prompt format"""
        return "\n\n".join(tool._sys_prompt() for tool in self.tools.values())


SystemPromptGen = Callable[[Type[RetvalModel], Optional[Toolset]], str]


class ToolCallingLLM:
    """LLM wrapper with tool-calling capabilities"""

    def __init__(
        self,
        llm_provider: LLMProvider,
        model: str = "gpt-4",
        n_retries: int = 5,
    ):
        self.llm = llm_provider
        self.model = model
        self.n_retries = n_retries

    def process_message(
        self,
        message: str,
        system_prompt_gen: SystemPromptGen[RetvalModel],
        max_messages: int = 10,
        toolset: Optional[Toolset] = None,
        retval_model: Type[RetvalModel] = str,
    ):
        """Process a message using available tools.

        Args:
            message: The message to process
            system_prompt_gen: Function to generate the system prompt
            max_messages: Maximum number of messages to send to the LLM

        Returns:
            ProcessResult containing both the chain of thoughts and final conclusion

        Raises:
            LLMFormatError: If the LLM doesn't finish thinking within max_messages
        """
        if issubclass(retval_model, BaseModel):
            retval_format = retval_model.model_json_schema(mode="serialization")
        elif retval_model is str:
            retval_format = "string"

        messages = [
            {
                "role": "system",
                "content": system_prompt_gen(retval_model, toolset),
            },
            {"role": "user", "content": message},
        ]

        all_thoughts: List[Thought] = []

        for _ in range(max_messages):
            response = self.llm.chat_completion(
                messages=messages,
                temperature=0.7,
                model=self.model,
            )

            try:
                thoughts = parse_next_steps(response.content)
            except LLMFormatError:
                if self.n_retries > 0:
                    self.n_retries -= 1
                    continue
                else:
                    raise LLMFormatError(
                        "I tried so hard, and got so far, but in the end, it doesn't even matter."
                    )
            # truncate the thoughts if we have a tool call or return in the middle
            n = len(thoughts)
            for i, thought in enumerate(thoughts, start=1):
                if thought.data.tag in ["tool_call", "return"]:
                    n = i
                    break
            thoughts = thoughts[:n]

            all_thoughts.extend(thoughts)

            # Add thoughts as assistant message
            concatenated_thoughts = "\n".join(
                f"STEP {thought.step_number}: {thought.data.natural_language}"
                for thought in thoughts[:-1]
            )
            if thoughts[-1].data.tag == "tool_call":
                tool_call_content = json.dumps(thoughts[-1].data.model_dump(), indent=2)
                last_thought_content = (
                    f"STEP {thoughts[-1].step_number}: {tool_call_content}"
                )
                concatenated_thoughts += f"\n{last_thought_content}"
            messages.append({"role": "assistant", "content": concatenated_thoughts})

            # Handle the final thought
            final_thought = thoughts[-1]
            if final_thought.data.tag == "return":
                if retval_model is str:
                    return ProcessResult(
                        thoughts=all_thoughts, retval=final_thought.data.retval
                    )
                else:
                    try:
                        process_retval: RetvalModel = retval_model.model_validate_json(
                            final_thought.data.retval
                        )
                        return ProcessResult(
                            thoughts=all_thoughts, retval=process_retval
                        )
                    except ValidationError as e:
                        feedback = f"The return value didn't match the expected format: {retval_format}"
                        messages.append(
                            {
                                "role": "user",
                                "content": feedback,
                            }
                        )
            elif final_thought.data.tag == "tool_call":
                tool_call = final_thought.data
                try:
                    if tool_call.tool_name not in toolset.tools:
                        raise ToolError(f"Tool {tool_call.tool_name} not found")
                    tool = toolset.tools[tool_call.tool_name]
                    result = tool.function(**tool_call.parameters)
                    content = json.dumps(result.model_dump(), indent=2)
                    messages.append(
                        {
                            "role": "user",
                            "content": content,
                        }
                    )
                except ToolError as e:
                    messages.append(
                        {
                            "role": "user",
                            "content": json.dumps({"tool_error": str(e)}),
                        }
                    )
                # In any case, we need to update the system prompt given that
                # the tools might have changed some hidden state that affects it:
                messages[0]["content"] = system_prompt_gen(retval_model, toolset)

        raise LLMFormatError(
            f"LLM didn't finish thinking within {max_messages} messages"
        )
