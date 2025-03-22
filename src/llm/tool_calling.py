from typing import List, Dict, Any, Callable, Optional, Type, Union, Literal
from pydantic import BaseModel, Field, ValidationError
import json
import re

from .llm_provider import LLMProvider
from ..common import ToolError


class LLMFormatError(Exception):
    """Raised when LLM output doesn't match expected format"""

    pass


class Tool(BaseModel):
    """Definition of a tool the LLM can use"""

    name: str = Field(..., description="Name of the tool")
    description: str = Field(..., description="Description of the tool")
    parameters: Dict[str, str] = Field(..., description="Parameters of the tool")
    return_schema: Optional[Dict[str, Any]] = Field(
        None, description="Schema of the return value of the tool"
    )
    function: Callable[..., BaseModel] = Field(..., description="Function to call")


def make_tool(fn: Callable[..., BaseModel]) -> Tool:
    """Create a tool from a function"""
    annotations = {k: str(v) for k, v in fn.__annotations__.items() if k != "return"}
    if "return" in fn.__annotations__:
        return_model: Type[BaseModel] = fn.__annotations__["return"]
        return_schema = return_model.model_json_schema(mode="serialization")
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


class Conclusion(BaseModel):
    """A conclusion from the LLM"""

    tag: Literal["conclusion"] = "conclusion"
    conclusion: str = Field(..., description="The conclusion from the LLM")


class Thought(BaseModel):
    """A single thought in the chain of reasoning"""

    data: Union[NaturalLanguage, ToolCall, ToolCallResult, Conclusion] = Field(
        ..., description="The data of the thought", discriminator="tag"
    )
    step_number: int = Field(..., description="Step number in the reasoning chain")


class ChainOfThought(BaseModel):
    """A chain of thought for the assistant"""

    thoughts: List[Thought] = Field(
        ..., description="A list of thoughts and tool interactions"
    )


def parse_next_steps(text: str) -> List[Thought]:
    """Parse steps from the LLM response until a tool call or conclusion is found.
    Returns all steps including the final tool call or conclusion.

    Args:
        text: Text containing one or more steps

    Returns:
        List of Thoughts, where the last thought is either a tool call or conclusion

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
            steps.append(
                Thought(
                    data=NaturalLanguage(natural_language=content),
                    step_number=int(step_num),
                )
            )

    # Process the final step - must be either a tool call or lead to a conclusion
    final_step_num, final_content = matches[-1]
    final_content = final_content.strip()
    if not final_content:
        raise LLMFormatError("Empty final step")

    # First try to parse as tool call
    try:
        data = ToolCall.model_validate_json(final_content)
        steps.append(
            Thought(
                data=ToolCall(
                    tool_name=data.tool_name,
                    parameters=data.parameters,
                    reason=data.reason,
                ),
                step_number=int(final_step_num),
            )
        )
        return steps
    except (json.JSONDecodeError, ValidationError):
        # Not a tool call, check for conclusion
        conclusion_pattern = re.compile(r"Conclusion:(.*?)$", re.DOTALL)
        conclusion_match = conclusion_pattern.search(text)
        if conclusion_match:
            conclusion = conclusion_match.group(1).strip()
            steps.append(
                Thought(
                    data=Conclusion(conclusion=conclusion),
                    step_number=int(final_step_num),
                )
            )
            return steps

        # If we get here, the final step was neither a tool call nor led to a conclusion
        raise LLMFormatError("Final step must be a tool call or lead to a conclusion")


class AssistantResponse(BaseModel):
    """Structured response from the assistant"""

    tool_calls: List[ToolCall] = Field(
        default_factory=list, description="List of tools to call"
    )
    message: str = Field(..., description="Natural language response to the user")


class ToolCallingLLM:
    """LLM wrapper with tool-calling capabilities"""

    def __init__(
        self,
        system_prompt: str,
        llm_provider: LLMProvider,
        model: str = "gpt-4",
        n_retries: int = 5,
    ):
        self.system_prompt = system_prompt
        self.llm = llm_provider
        self.model = model
        self.tools: Dict[str, Tool] = {}
        self.n_retries = n_retries

    def add_tools(self, tools: List[Callable]) -> None:
        """Add tools that the LLM can use.

        Args:
            tools: List of functions that implement the tool interface
        """
        for tool in tools:
            if not isinstance(tool, Tool):
                tool = make_tool(tool)
            self.tools[tool.name] = tool

    def process_message(self, message: str, max_messages: int = 10) -> str:
        """Process a message using available tools.

        Args:
            message: The message to process
            max_messages: Maximum number of messages to send to the LLM

        Returns:
            The response from the LLM

        Raises:
            LLMFormatError: If the LLM doesn't finish thinking within max_messages
        """
        messages = [
            {
                "role": "system",
                "content": self.system_prompt,
            },
            {"role": "user", "content": message},
        ]

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

            # Add all thoughts except the last one as assistant message
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

            # Handle the final thought (either tool call or conclusion)
            final_thought = thoughts[-1]
            if final_thought.data.tag == "conclusion":
                messages.append(
                    {
                        "role": "assistant",
                        "content": f"Conclusion: {final_thought.data.conclusion}",
                    }
                )
                return final_thought.data.conclusion
            elif final_thought.data.tag == "tool_call":
                tool_call = final_thought.data
                if tool_call.tool_name not in self.tools:
                    raise ToolError(f"Tool {tool_call.tool_name} not found")
                tool = self.tools[tool_call.tool_name]
                try:
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

        # If we got here, the LLM didn't finish thinking within max_messages
        raise LLMFormatError(
            f"LLM didn't finish thinking within {max_messages} messages"
        )
