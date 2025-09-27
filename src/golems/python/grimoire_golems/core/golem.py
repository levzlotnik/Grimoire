"""
Core golem class using Pydantic AI for unified LLM interface.

This class integrates with GrimoireInterface to auto-discover available tools
and uses Pydantic AI's agent framework for all LLM interactions.
"""

import os
import sys
import inspect
from typing import Dict, List, Any, Optional
from pydantic import BaseModel
from pydantic_ai import Agent
from pydantic_ai.models.openai import OpenAIModel

# # Add the Grimoire interface path to sys.path
# grimoire_root = os.getenv('GRIMOIRE_ROOT', '/home/levz/Projects/Grimoire')
# interface_path = os.path.join(grimoire_root, 'src', 'interface', 'api')
# if interface_path not in sys.path:
#     sys.path.insert(0, interface_path)

from grimoire_interface import GrimoireInterface
from ..types import get_type_by_name


class Golem:
    """
    Core golem class using Pydantic AI agents.

    Configuration is passed as a dict from Prolog:
    - model: Model string like "anthropic:sonnet-4" or "openai:gpt-4o"
    - base_url: Optional custom endpoint for Ollama or OpenAI-compatible APIs
    - temperature: Optional temperature setting
    - max_tokens: Optional max tokens setting
    """

    def __init__(self, golem_id: str, config: Dict, session_id: str):
        self.golem_id = golem_id
        self.session_id = session_id
        self.config = config

        # Initialize Grimoire interface for tools
        self.grimoire_interface = GrimoireInterface()

        # Create Pydantic AI agent based on config
        self.agent = self._create_agent(config)

    def _create_agent(self, config: Dict) -> Agent:
        """Create a Pydantic AI agent from configuration dict."""
        model = config.get("model", "mock")

        # Check if we need custom endpoint (Ollama or OpenAI-compatible)
        if "base_url" in config:
            # Use OpenAIModel with custom base URL
            model_name = model  # Model name without provider prefix
            if ":" in model:
                # Strip provider prefix if present
                model_name = model.split(":", 1)[1]

            model_instance = OpenAIModel(
                model_name=model_name, base_url=config["base_url"]
            )
        else:
            # Use standard model string
            model_instance = model

        # Get output type from config if specified
        output_type = dict  # Default to dict
        if "output_type" in config:
            type_class = get_type_by_name(config["output_type"])
            if type_class:
                output_type = type_class

        # Get system prompt if specified
        system_prompt = config.get("system_prompt", None)

        # Create agent with configuration
        agent_kwargs = {"model": model_instance, "output_type": output_type}

        if system_prompt:
            agent_kwargs["system_prompt"] = system_prompt

        agent = Agent(**agent_kwargs)

        # Register Grimoire tools with the agent
        self._register_tools(agent)

        return agent

    def _register_tools(self, agent: Agent):
        """Register GrimoireInterface methods as tools for the agent."""
        for name, method in inspect.getmembers(
            self.grimoire_interface, predicate=inspect.ismethod
        ):
            if name.startswith("_"):
                continue

            # Create tool wrapper that calls the GrimoireInterface method
            def make_tool_wrapper(method_name, method_obj):
                async def tool_wrapper(**kwargs):
                    """Dynamically generated tool wrapper."""
                    return method_obj(**kwargs)

                # Set proper name and docstring
                tool_wrapper.__name__ = f"grimoire_{method_name}"
                tool_wrapper.__doc__ = (
                    inspect.getdoc(method_obj) or f"Execute {method_name}"
                )

                return tool_wrapper

            # Register the tool with the agent
            tool_fn = make_tool_wrapper(name, method)
            agent.tool(tool_fn)

    async def execute_task(self, inputs: Dict) -> Dict:
        """
        Execute a task with the agent.

        Args:
            inputs: Dict of inputs from Prolog

        Returns:
            Dict containing the execution result
        """
        # Build prompt from inputs
        prompt = self._build_prompt(inputs)

        # Run the agent
        result = await self.agent.run(prompt)

        # Convert result to dict
        output_data = None

        # Handle PydanticAI RunResult
        if hasattr(result, "data"):
            output_data = result.data
        else:
            output_data = result

        # Convert Pydantic BaseModel instances to dict
        if isinstance(output_data, BaseModel):
            return output_data.dict()
        elif isinstance(output_data, dict):
            return output_data
        else:
            # Wrap simple types in a dict
            return {
                "output": output_data,
                "golem_id": self.golem_id,
                "session_id": self.session_id,
            }

    def execute_task_sync(self, inputs: Dict) -> Dict:
        """
        Synchronous version of execute_task for Prolog bridge compatibility.

        Args:
            inputs: Dict of inputs from Prolog

        Returns:
            Dict containing the execution result
        """
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
