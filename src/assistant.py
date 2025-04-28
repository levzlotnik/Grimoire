from abc import ABC, abstractmethod
import json
import os
from pathlib import Path
from typing import Callable, Generic, List, Dict, Any, Optional, Tuple, Type, TypeVar
from llm_provider import get_llm_provider
from tool_calling import ProcessResult, Tool, ToolCallingLLM, Toolset, make_tool
from prolog_tool import PrologToolset
from textwrap import dedent
from pydantic import BaseModel, Field, ValidationError, model_validator
from common import ToolError, PrimType
from logger import InteractionLogger, SessionContext


class KnowledgeTree(BaseModel):
    """Knowledge accumulated during exploration phase"""

    data: dict = Field(
        default_factory=dict,
        description="Knowledge tree containing relevant entities and components",
    )
    summary: Dict[str, str] = Field(
        default_factory=dict, description="Summary of the exploration results"
    )

    def to_prompt(self) -> str:
        """Convert knowledge tree to a prompt-friendly format"""
        return json.dumps(self.model_dump(), indent=2)

    def remember_component(
        self,
        entity: str,
        component_name: str,
        component_val: str,
        is_entity: bool = False,
        docstring: Optional[str] = None,
    ):
        """Remember a component in the knowledge tree"""
        # Access or create the entity in the knowledge tree
        if entity not in self.data:
            self.data[entity] = {}

        entity_data = self.data[entity]

        # Handle the component value
        if is_entity:
            # If the value is an entity, add it as a reference and ensure it exists as a top-level key
            ref = f"${component_val}"
            if component_name not in entity_data:
                entity_data[component_name] = []
            if ref not in entity_data[component_name]:
                entity_data[component_name].append(ref)

            if component_val not in self.data:
                assert (
                    docstring is not None
                ), "Docstring must be provided for new entities"
                self.data[component_val] = {"docstring": docstring}
        else:
            # If the value is immutable, add it directly
            if component_name not in entity_data:
                entity_data[component_name] = []
            if component_val not in entity_data[component_name]:
                entity_data[component_name].append(component_val)


class CommandExplanation(BaseModel):
    """Command with its explanation"""

    command: str
    rationale: str


class Plan(BaseModel):
    """Plan developed from exploration results"""

    commands: List[CommandExplanation] = Field(default_factory=list)
    summary: str

    def to_prompt(self) -> str:
        """Convert planning result to a prompt-friendly format"""
        return json.dumps(self.model_dump(), indent=2)

    @model_validator(mode="after")
    def validate_commands(self):
        """Ensure commands are properly formatted"""
        if len(self.commands) == 0:
            return
        for cmd in self.commands:
            if not (cmd.command.startswith("command(") and cmd.command.endswith(")")):
                raise ValidationError(f"Invalid command format: {cmd}")
        return self


PROMPTS_PATH = os.path.join(os.path.dirname(__file__), "prompts")
with open(os.path.join(PROMPTS_PATH, "base_context.md")) as f:
    BASE_CONTEXT = f.read()


def _template_format(template: str, **vars: Dict[str, Any]) -> str:
    """Format a template string with variables.

    Example:

        >>> template = "Hello, {{name}}!"
        >>> formatted = _template_format(template, name="Alice")
        >>> print(formatted)
        Hello, Alice!

    """
    for key, value in vars.items():
        template = template.replace(f"{{{{{key}}}}}", str(value))
    return template


InT = TypeVar("InT", bound=PrimType)
OutT = TypeVar("OutT", bound=PrimType)


class Agent(ABC, Generic[InT, OutT]):
    """Abstract base class for agents"""

    def __init__(self, agent_id: str):
        self.agent_id = agent_id

    @abstractmethod
    def process(self, task: InT) -> ProcessResult[OutT]:
        """Process a task and return the result."""

    def run(self, task: InT, logger: InteractionLogger) -> OutT:
        """Run the agent with a task and log the result.

        Args:
            task: The task to process.
            logger: The logger to use for logging thoughts.
        Returns:
            The result of the task processing.
        """
        result = self.process(task)
        thought_id = None
        for thought in result.thoughts:
            thought_id = logger.log_thought(thought, thought_id)
        return result.retval


class PrologExplorationAgent(Agent[str, KnowledgeTree]):
    """Agent for exploring the Prolog knowledge base"""

    def __init__(
        self,
        agent_id: str,
        llm: ToolCallingLLM,
        prolog_engine: PrologToolset,
        max_messages: int = 10,
    ):
        super().__init__(agent_id)
        self.engine = prolog_engine
        tools = [
            prolog_engine.list_entities,
            prolog_engine.list_components_types,
            prolog_engine.list_components,
            prolog_engine.load_entity_source,
            prolog_engine.get_docstring,
            self.remember_component,
        ]
        tools = [make_tool(tool) for tool in tools]
        toolset = Toolset(tools={tool.name: tool for tool in tools})
        self.tools = toolset
        self._llm = llm
        self.max_messages = max_messages

        self.state = KnowledgeTree()
        self._init_knowledge_tree()

    def _init_knowledge_tree(self):
        """Initialize the knowledge tree with system concepts"""
        system_docstring = self.engine.get_docstring("system").docstring
        self.state = KnowledgeTree(data={"system": {"docstring": system_docstring}})
        concepts = self.engine.list_components("system", "concept")
        self.state.data["system"]["concept"] = {
            concept: {"docstring": self.engine.get_docstring(concept).docstring}
            for concept in concepts.components
        }

    def remember_component(self, entity: str, component_name: str, component_val: str):
        """Remember a component in the knowledge tree, directly using entity names as keys.

        Args:
            entity: The name of the entity, e.g., "command"
            component_name: The name of the component, e.g., "ctor"
            component_val: Specific value of the component to remember

        Note:
            - If the component value is an entity, it will be added as a reference (prefixed with `$`).
            - Entities are represented as top-level keys in the knowledge tree.

        Example:
            {
                "tool_name": "remember_component",
                "parameters": {
                    "entity": "command",
                    "component_name": "ctor",
                    "component_val": "mkdir",
                },
                "reason": "I called remember_component to store the command constructor 'mkdir' as it is relevant for the task."
            }
        """
        if self.engine.is_entity(component_val).result:
            is_entity = True
            docstring = self.engine.get_docstring(component_val).docstring
        else:
            is_entity = False
            docstring = None

        self.state.remember_component(
            entity=entity,
            component_name=component_name,
            component_val=component_val,
            is_entity=is_entity,
            docstring=docstring,
        )

    def _get_system_prompt(self) -> str:
        """Get the system prompt for the agent"""
        with open(os.path.join(PROMPTS_PATH, "exploration.md")) as f:
            template = f.read()
        return _template_format(
            template,
            tools=self.tools.to_prompt(),
            base_context=BASE_CONTEXT,
            retval_format="string",
            knowledge_tree=self.state.to_prompt(),
        )

    def process(self, task: str) -> ProcessResult[KnowledgeTree]:
        """Process a task and return the result."""
        processed_message = self._llm.process_message(
            task,
            system_prompt_gen=self._get_system_prompt,
            toolset=self.tools,
            retval_model=str,
            max_messages=self.max_messages,
            agent_id=self.agent_id,
        )

        self.state = processed_message.retval

        result = ProcessResult[KnowledgeTree](
            thoughts=processed_message.thoughts, retval=self.state
        )
        return result

    def reset(self):
        """Reset the agent's state and knowledge tree"""
        self._init_knowledge_tree()


class PrologPlanningAgent(Agent[Tuple[str, KnowledgeTree], Plan]):
    """Agent for planning Prolog commands based on exploration results"""

    def __init__(self, agent_id: str, llm: ToolCallingLLM, max_messages: int = 10):
        super().__init__(agent_id)
        self._llm = llm
        self.max_messages = max_messages
        self._exploration_state: Optional[KnowledgeTree] = None

    def set_exploration(self, exploration: KnowledgeTree):
        """Set the exploration state for planning"""
        self._exploration_state = exploration

    def _get_system_prompt(self) -> str:
        """Get the system prompt for planning"""
        assert (
            self._exploration_state is not None
        ), "Exploration state must be set before planning"
        with open(os.path.join(PROMPTS_PATH, "planning_prompt.md")) as f:
            template = f.read()
        return _template_format(
            template,
            base_context=BASE_CONTEXT,
            retval_format=Plan.model_json_schema(mode="serialization"),
            knowledge_tree=self._exploration_state.to_prompt(),
        )

    def process(
        self, task_and_exploration: Tuple[str, KnowledgeTree]
    ) -> ProcessResult[Plan]:
        """Process a planning task and return the result."""
        task, exploration = task_and_exploration
        self.set_exploration(exploration)
        return self._llm.process_message(
            task,
            system_prompt_gen=self._get_system_prompt,
            retval_model=Plan,
            max_messages=self.max_messages,
            agent_id=self.agent_id,
        )


class PrologExecutionAgent(Agent[Tuple[str, Plan], str]):
    """Agent for executing and interpreting Prolog commands"""

    def __init__(
        self,
        agent_id: str,
        llm: ToolCallingLLM,
        prolog_engine: PrologToolset,
        max_messages: int = 10,
    ):
        super().__init__(agent_id)
        self._llm = llm
        self.engine = prolog_engine
        self.max_messages = max_messages
        self._plan_state: Optional[Plan] = None

    def set_plan(self, plan: Plan):
        """Set the plan state for execution"""
        self._plan_state = plan

    def _get_system_prompt(self) -> str:
        """Get the system prompt for execution"""
        assert self._plan_state is not None, "Plan state must be set before execution"
        with open(os.path.join(PROMPTS_PATH, "execution_prompt.md")) as f:
            template = f.read()
        return _template_format(
            template,
            base_context=BASE_CONTEXT,
            retval_format="string",
            transaction_result=self._plan_state.to_prompt(),
        )

    def process(self, task_and_plan: Tuple[str, Plan]) -> ProcessResult[str]:
        """Process an execution task and return the result."""
        task, plan = task_and_plan
        self.set_plan(plan)
        commands = [cmd.command for cmd in plan.commands]
        transaction_result = self.engine.commit_transaction(commands)

        return self._llm.process_message(
            task,
            system_prompt_gen=self._get_system_prompt,
            retval_model=str,
            max_messages=self.max_messages,
            agent_id=self.agent_id,
        )


class MyPAOSAssistant:
    """Assistant that uses LLM to interact with the operating system."""

    def __init__(
        self,
        project_path: Path | str,
        agent_id: str = "assistant",
        provider: str = "claude",
        model: str = "claude-3-5-sonnet-20240620",
    ):
        self.project_path = Path(project_path)
        self.engine = PrologToolset()
        self.logger = InteractionLogger(self.project_path)
        self.agent_id = agent_id

        self.llm = ToolCallingLLM(
            llm_provider=get_llm_provider(provider),
            model=model,
        )

        # Initialize agents
        self.exploration_agent = PrologExplorationAgent(
            f"{self.agent_id}/exploration_agent",
            llm=self.llm,
            prolog_engine=self.engine,
            max_messages=10,
        )

        self.planning_agent = PrologPlanningAgent(
            f"{self.agent_id}/planning_agent",
            llm=self.llm,
            max_messages=10,
        )

        self.execution_agent = PrologExecutionAgent(
            f"{self.agent_id}/execution_agent",
            llm=self.llm,
            prolog_engine=self.engine,
            max_messages=10,
        )

        # Start session immediately
        session_context = SessionContext(self.project_path)
        self.logger.start_session(session_context)

    def __del__(self):
        """Ensure session is ended when assistant is destroyed"""
        self.logger.end_session()

    def start_session(self):
        """Start a new interaction session"""
        session_context = SessionContext(self.project_path)
        self.logger.start_session(session_context)
        self.init_knowledge_tree()
        self.logger.info(
            f"Starting new session at {self.project_path}, with tools: {self.tools}"
        )

    def end_session(self):
        """End the current interaction session"""
        self.logger.end_session()
        self.logger.info(f"Ending session at {self.project_path}")

    def process_task(self, task: str, max_messages: int = 10):
        """Process message through all phases with validation"""
        # Exploration Phase
        exploration_result = self.exploration_agent.run(task, self.logger)

        # Planning Phase
        plan = self.planning_agent.run((task, exploration_result), self.logger)

        # Execution Phase (if needed)
        if len(plan.commands) > 0:
            execution_result = self.execution_agent.run((task, plan), self.logger)
            self.logger.info(f"Execution result: {execution_result}")
            return execution_result

        return "No commands to execute."


if __name__ == "__main__":
    assistant = MyPAOSAssistant(
        provider="openai",
        model="gpt-4.1-2025-04-14",
        project_path=Path.cwd() / "test_project",
    )
    # Example interaction
    print(
        assistant.process_task(
            "Create a full stack application project for me. "
            "What are the possible frameworks I can use for this?",
            max_messages=30,
        )
    )
    print(assistant.process_task('What is the "project_development" rule?'))
    print(assistant.process_task("What is the 'component' rule?"))
