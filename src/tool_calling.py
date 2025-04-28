from textwrap import dedent, indent
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
    Tuple,
)
from pydantic import BaseModel, Field, ValidationError
import json
import re

from llm_provider import LLMProvider
from common import ToolError, dump_val

RetvalModel = TypeVar("RetvalModel", bound=Union[str, BaseModel])
SystemPromptGen = Callable[[], str]
"""Function that generates system prompts for the LLM.
Returns:
    A string containing the system prompt
"""


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
        signature = f"{self.name}({', '.join(self.parameters.keys())})"
        return_schema = dump_val(self.return_schema)
        description = indent(self.description, "    ")
        return dedent(
            f"""
            Tool: {signature}
            Returns: {return_schema}
            Description:
            {description}
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


class Feedback(BaseModel):
    tag: Literal["feedback"] = "feedback"
    message: str = Field(..., description="The feedback message")
    source: str = Field(
        ..., description="Source of the feedback (tool/user/validation)"
    )
    success: bool = Field(
        ..., description="Whether this represents a successful outcome"
    )


class RetVal(BaseModel):
    """A conclusion from the LLM"""

    tag: Literal["return"] = "return"  # Changed from "conclusion"
    retval: str = Field(..., description="The return value from this chain of thought")


class Thought(BaseModel):
    """A single thought in the chain of reasoning"""

    data: Union[NaturalLanguage, ToolCall, Feedback, RetVal] = Field(
        ..., description="The data of the thought", discriminator="tag"
    )
    step_number: int = Field(..., description="Step number in the reasoning chain")
    agent_id: str = Field(..., description="ID of the agent generating the thought")

    @classmethod
    def with_data(
        cls,
        data: Union[NaturalLanguage, ToolCall, Feedback, RetVal],
        step_number: int = 1,
        agent_id: str = "assistant",
    ) -> "Thought":
        """Create a new thought with the given data and step number"""
        return cls(data=data, step_number=step_number, agent_id=agent_id)

    def to_text(self) -> str:
        """Convert the thought to a text format"""
        if self.data.tag == "natural_language":
            return self.data.natural_language
        elif self.data.tag == "tool_call":
            return dump_val(self.data)
        elif self.data.tag == "return":
            retval = dump_val(self.data.retval)
            return f"return: {retval}"
        elif self.data.tag == "feedback":
            if self.data.success:
                return f"Success: {self.data.message}"
            else:
                return f"Error: {self.data.message}"
        else:
            raise TypeError(f"Unknown thought type: {self.data.tag}")


class ChainOfThought(BaseModel):
    """A chain of thought for the assistant"""

    thoughts: List[Thought] = Field(
        ..., description="A list of thoughts and tool interactions"
    )


class ValidationWarning(BaseModel):
    """Warning for validation issues"""

    original_content: str = Field(
        ..., description="Original content that caused the warning"
    )
    error_message: str = Field(..., description="Error message from validation")


class ParseResult(BaseModel):
    """Result of parsing steps"""

    thoughts: List[Thought] = Field(..., description="Parsed thoughts")
    warnings: List[ValidationWarning] = Field(..., description="Validation warnings")


def _try_parse_tool_call(
    content: str,
) -> Tuple[Optional[ToolCall], Optional[ValidationWarning]]:
    """Try to parse a tool call from the content, returning both the tool call and any validation warning"""
    try:
        json_content = json.loads(content)
        if isinstance(json_content, dict):
            try:
                data = ToolCall.model_validate(json_content)
                return (
                    ToolCall(
                        tool_name=data.tool_name,
                        parameters=data.parameters,
                        reason=data.reason,
                    ),
                    None,
                )
            except ValidationError as e:
                return None, ValidationWarning(
                    original_content=content, error_message=str(e)
                )
    except json.JSONDecodeError:
        return None, None


def parse_next_steps(text: str) -> ParseResult:
    """Parse steps from the LLM response until a tool call or return is found.
    Returns all steps including the final tool call or return, along with any validation warnings.

    Args:
        text: Text containing one or more steps

    Returns:
        ParseResult containing thoughts and any validation warnings

    Raises:
        LLMFormatError: If no valid steps are found or if steps are not properly formatted
    """
    steps: List[Thought] = []
    warnings: List[ValidationWarning] = []
    # Split on STEP markers, capturing the step number
    pattern = re.compile(r"STEP (\d+):(.*?)(?=STEP \d+:|$)", re.DOTALL)
    matches = pattern.findall(text)

    if not matches:
        raise LLMFormatError("No valid steps found in response")

    # Process all steps except the last one
    for step_num, content in matches[:-1]:
        content = content.strip()
        if content:
            tool_call, warning = _try_parse_tool_call(content)
            if warning:
                warnings.append(warning)
            if tool_call:
                steps.append(Thought(data=tool_call, step_number=int(step_num)))
            else:
                steps.append(
                    Thought(
                        data=NaturalLanguage(natural_language=content),
                        step_number=int(step_num),
                    )
                )

    # Process the final step - must be either a tool call or lead to a return
    final_step_num, final_content = matches[-1]
    final_content = final_content.strip()
    if not final_content:
        raise LLMFormatError("Empty final step")

    # First try to parse as tool call
    tool_call, warning = _try_parse_tool_call(final_content)
    if warning:
        warnings.append(warning)
    if tool_call:
        steps.append(Thought(data=tool_call, step_number=int(final_step_num)))
        return ParseResult(thoughts=steps, warnings=warnings)

    # Try to parse as return statement
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
        return ParseResult(thoughts=steps, warnings=warnings)

    # If we get here, the final step was neither a tool call nor a return
    raise LLMFormatError("Final step must be a tool call or return statement")


def parse_single_thought(text: str, agent_id: str, step_number: int) -> Thought:
    content = text.strip()
    if not content:
        raise LLMFormatError("Empty thought")
    try:
        json_content = json.loads(content)
        if isinstance(json_content, dict):
            return Thought(
                data=ToolCall.model_validate(json_content),
                step_number=step_number,
                agent_id=agent_id,
            )
    except (json.JSONDecodeError, ValidationError):
        pass
    return_pattern = re.compile(r"return:(.*?)$", re.DOTALL)
    return_match = return_pattern.search(text)
    if return_match:
        return Thought(
            data=RetVal(retval=return_match.group(1).strip()),
            step_number=step_number,
            agent_id=agent_id,
        )
    return Thought(
        data=NaturalLanguage(natural_language=content),
        step_number=step_number,
        agent_id=agent_id,
    )


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

    def add_tool(self, tool: Tool):
        """Add a tool to the toolset."""
        if tool.name in self.tools:
            raise ToolError(f"Tool {tool.name} already exists")
        self.tools[tool.name] = tool

    def to_prompt(self) -> str:
        """Convert tools to a prompt format"""
        return "\n\n".join(tool._sys_prompt() for tool in self.tools.values())


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

    def _generate_next_thought(
        self, messages: List[Dict[str, str]], agent_id: str, step_number: int
    ) -> Thought:
        for _ in range(self.n_retries):
            try:
                response = self.llm.chat_completion(
                    messages=messages,
                    temperature=0.7,
                    model=self.model,
                )
                return parse_single_thought(response.content, agent_id, step_number)
            except LLMFormatError:
                continue
        raise LLMFormatError("Failed to generate valid thought after retries")

    def _execute_thought(
        self, thought: Thought, toolset: Optional[Toolset]
    ) -> Feedback:
        if thought.data.tag == "tool_call":
            try:
                if not toolset or thought.data.tool_name not in toolset.tools:
                    raise ToolError(f"Tool {thought.data.tool_name} not found")
                tool = toolset.tools[thought.data.tool_name]
                result = tool.function(**thought.data.parameters)
                return Feedback(
                    message=dump_val(result),
                    source="tool",
                    success=True,
                )
            except (KeyError, ToolError) as e:
                return Feedback(message=str(e), source="tool", success=False)
        return Feedback(message="continue", source="system", success=True)

    def process_message(
        self,
        message: str,
        system_prompt_gen: SystemPromptGen,
        max_messages: int = 10,
        toolset: Optional[Toolset] = None,
        retval_model: Type[RetvalModel] = str,
        agent_id: str = "assistant",
    ):
        """Process a message using available tools.

        Args:
            message: The message to process
            system_prompt_gen: Function to generate the system prompt
            max_messages: Maximum number of messages to send to the LLM
            toolset: Optional set of tools to use
            retval_model: Type to validate the return value against
            agent_id: ID of the agent processing the message

        Returns:
            ProcessResult containing both the chain of thoughts and final conclusion

        Raises:
            LLMFormatError: If the LLM doesn't finish thinking within max_messages
        """
        messages = [
            {
                "role": "system",
                "content": system_prompt_gen(),
            },
            {"role": "user", "content": message},
        ]

        all_thoughts: List[Thought] = []
        step_number = 1

        for _ in range(max_messages):
            thought = self._generate_next_thought(messages, agent_id, step_number)
            all_thoughts.append(thought)
            step_number += 1

            if thought.data.tag == "return":
                try:
                    if retval_model is str:
                        return ProcessResult(
                            thoughts=all_thoughts, retval=thought.data.retval
                        )
                    retval = retval_model.model_validate_json(thought.data.retval)
                    return ProcessResult(thoughts=all_thoughts, retval=retval)
                except ValidationError as e:
                    feedback = Feedback(
                        message=f"Return value validation failed: {str(e)}",
                        source="validation",
                        success=False,
                    )
            else:
                feedback = self._execute_thought(thought, toolset)

            feedback_thought = Thought(
                data=feedback, step_number=step_number, agent_id=agent_id
            )
            all_thoughts.append(feedback_thought)
            step_number += 1

            messages.append({"role": "assistant", "content": thought.to_text()})
            messages.append({"role": "user", "content": feedback_thought.to_text()})
            messages[0]["content"] = system_prompt_gen()

        raise LLMFormatError(
            f"LLM didn't finish thinking within {max_messages} messages"
        )
