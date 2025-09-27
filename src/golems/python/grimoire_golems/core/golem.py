"""
Core golem class using Pydantic AI for unified LLM interface.

This class integrates with GrimoireInterface to auto-discover available tools
and uses Pydantic AI's agent framework for all LLM interactions.
"""

import os
import sys
import inspect
from typing import Dict, List, Any, Optional, Type, Union, TypeVar, Generic
from pydantic import BaseModel, Field
from pydantic_ai import Agent
from pydantic_ai.models.openai import OpenAIChatModel
from pydantic_ai.result import AgentRunResult
from pydantic_ai.messages import ModelMessage
from pydantic_ai.toolsets import FunctionToolset

from grimoire_interface import GrimoireInterface

# Type variable for generic GolemResponse
OutputT = TypeVar("OutputT")


class GolemResponse(BaseModel, Generic[OutputT]):
    """Response structure for golem task execution."""

    output: OutputT
    messages: List[ModelMessage]
    golem_id: str
    session_id: str


class Config(BaseModel):
    """Configuration for Golem instances."""

    model: str
    temperature: float = 0.5
    max_tokens: int = 4096
    system_prompt: str
    output_type: Optional[Type[BaseModel]] = None
    base_url: Optional[str] = None


class Golem:
    """
    Core golem class using Pydantic AI agents.

    Configuration is passed as a dict from Prolog:
    - model: Model string like "anthropic:sonnet-4" or "openai:gpt-4o"
    - base_url: Optional custom endpoint for Ollama or OpenAI-compatible APIs
    - temperature: Optional temperature setting
    - max_tokens: Optional max tokens setting
    """

    def __init__(self, golem_id: str, config: Union[Config, Dict], session_id: str):
        self.golem_id = golem_id
        self.session_id = session_id

        # Convert config to Pydantic Config model if needed
        if isinstance(config, Config):
            self.config = config
        else:
            self.config = Config.model_validate(config)

        # Initialize Grimoire interface for tools, skipping Prolog init since janus-swi shares the session
        self.grimoire_interface = GrimoireInterface(skip_prolog_init=False)

        # Create Pydantic AI agent based on config
        self.agent = self._create_agent(self.config)

    def _create_agent(self, config: Config) -> Agent:
        """Create a Pydantic AI agent from configuration model."""
        model = config.model

        # Check if we need custom endpoint (Ollama or OpenAI-compatible)
        if config.base_url:
            # Use OpenAIChatModel with custom base URL
            model_name = model  # Model name without provider prefix
            if ":" in model:
                # Strip provider prefix if present
                model_name = model.split(":", 1)[1]

            if config.base_url:
                from pydantic_ai.providers.openai import OpenAIProvider
                provider = OpenAIProvider(base_url=config.base_url)
                model_instance = OpenAIChatModel(model_name, provider=provider)
            else:
                model_instance = OpenAIChatModel(model_name)
        else:
            # Use standard model string
            model_instance = model

        # Get output type from config (now a direct class reference)
        output_type = config.output_type if config.output_type else dict

        # Get the toolset from GrimoireInterface
        grimoire_toolset = self.grimoire_interface.get_toolset()
        
        # Create FunctionToolset from the Grimoire tools
        function_toolset = FunctionToolset(grimoire_toolset.tools)
        
        # Create agent with configuration and toolsets
        agent_kwargs = {
            "model": model_instance, 
            "output_type": output_type,
            "toolsets": [function_toolset]  # Pass as a list of toolsets
        }

        # Combine system prompts - config prompt + Grimoire system prompt
        if config.system_prompt:
            combined_prompt = f"{grimoire_toolset.system_prompt}\n\n{config.system_prompt}"
            agent_kwargs["system_prompt"] = combined_prompt
        else:
            agent_kwargs["system_prompt"] = grimoire_toolset.system_prompt

        agent = Agent(**agent_kwargs)

        return agent


    async def execute_task(self, inputs: Dict) -> GolemResponse[Any]:
        """
        Execute a task with the agent.

        Args:
            inputs: Dict of inputs from Prolog

        Returns:
            GolemResponse containing the execution result
        """
        # Build prompt from inputs
        prompt = self._build_prompt(inputs)

        # Run the agent
        result = await self.agent.run(prompt)

        # Convert result to GolemResponse with both output and messages
        if isinstance(result, AgentRunResult):
            output_data = result.output
            all_messages = result.new_messages()
            
            # Filter out system prompt and user message to avoid bloat
            filtered_messages = []
            for msg in all_messages:
                # Only include model responses and tool interactions, skip system/user prompts
                if msg.kind == 'response':
                    filtered_messages.append(msg)

            return GolemResponse(
                output=output_data,
                messages=filtered_messages,
                golem_id=self.golem_id,
                session_id=self.session_id,
            )
        else:
            # Non-AgentRunResult case (fallback)
            return GolemResponse(
                output=result,
                messages=[],
                golem_id=self.golem_id,
                session_id=self.session_id,
            )

    def execute_task_sync(self, inputs: Dict) -> GolemResponse[Any]:
        """
        Synchronous version of execute_task for Prolog bridge compatibility.

        Args:
            inputs: Dict of inputs from Prolog

        Returns:
            GolemResponse containing the execution result
        """
        print(f"DEBUG: PYTHON: Executing task synchronously with inputs: {inputs}")
        import asyncio

        # Get or create event loop
        try:
            loop = asyncio.get_event_loop()
        except RuntimeError:
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)

        # Run async task
        return loop.run_until_complete(self.execute_task(inputs))

    def _build_prompt(self, inputs: Dict) -> str:
        """Build prompt string from input dict."""
        # Handle different input formats
        if "prompt" in inputs:
            return inputs["prompt"]
        elif "task" in inputs:
            return inputs["task"]
        elif "message" in inputs:
            return inputs["message"]
        else:
            # Build prompt from all inputs
            parts = []
            for key, value in inputs.items():
                parts.append(f"{key}: {value}")
            return "\n".join(parts)

    def get_tools(self) -> List[Dict]:
        """
        Return list of available tools as dicts.

        Returns:
            List of tool definitions as dicts
        """
        tools = []

        for name, method in inspect.getmembers(
            self.grimoire_interface, predicate=inspect.ismethod
        ):
            if name.startswith("_"):
                continue

            sig = inspect.signature(method)
            doc = inspect.getdoc(method) or f"Execute {name}"

            # Create tool dict
            tool = {
                "name": f"grimoire_{name}",
                "description": doc.split("\n")[0],
                "parameters": self._sig_to_dict(sig),
            }
            tools.append(tool)

        return tools

    def _sig_to_dict(self, sig: inspect.Signature) -> Dict:
        """Convert Python signature to dict schema."""
        properties = {}
        required = []

        for param_name, param in sig.parameters.items():
            if param_name == "self":
                continue

            # Infer type from annotation
            param_type = "string"  # default
            if param.annotation != inspect.Parameter.empty:
                if param.annotation == int:
                    param_type = "integer"
                elif param.annotation == bool:
                    param_type = "boolean"
                elif param.annotation == float:
                    param_type = "number"
                elif hasattr(param.annotation, "__origin__"):
                    if param.annotation.__origin__ == list:
                        param_type = "array"
                    elif param.annotation.__origin__ == dict:
                        param_type = "object"

            properties[param_name] = {"type": param_type}

            if param.default == inspect.Parameter.empty:
                required.append(param_name)

        return {"type": "object", "properties": properties, "required": required}
