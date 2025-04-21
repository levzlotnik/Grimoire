import json
import os
from pathlib import Path
from typing import Callable, List, Dict, Any, Optional, Type
from llm_provider import get_llm_provider
from tool_calling import ProcessResult, Tool, ToolCallingLLM, Toolset, make_tool
from prolog_tool import PrologToolset
from textwrap import dedent
from pydantic import BaseModel, Field, ValidationError, model_validator
from common import ToolError
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


class MyPAOSAssistant:
    """Assistant that uses LLM to interact with the operating system."""

    def __init__(
        self,
        project_path: Path | str,
        provider: str = "claude",
        model: str = "claude-3-5-sonnet-20240620",
    ):
        self.project_path = Path(project_path)
        self.engine = PrologToolset()
        self.logger = InteractionLogger(self.project_path)

        # Initialize base tools
        tools = [
            self.engine.list_entities,
            self.engine.list_components_types,
            self.engine.list_components,
            self.engine.load_entity_source,
            self.engine.get_docstring,
            # self.engine.mount_semantic,
        ]

        self.tools = [make_tool(tool) for tool in tools]
        self.init_knowledge_tree()
        self.llm = ToolCallingLLM(
            llm_provider=get_llm_provider(provider),
            model=model,
        )

        # Start session immediately
        session_context = SessionContext(self.project_path)
        self.logger.start_session(session_context)

    def __del__(self):
        """Ensure session is ended when assistant is destroyed"""
        self.logger.end_session()

    def init_knowledge_tree(self):
        """Initialize the knowledge tree with system concepts"""
        system_docstring = self.engine.get_docstring("system").docstring
        self.knowledge_tree = KnowledgeTree(
            data={"system": {"docstring": system_docstring}}
        )
        concepts = self.engine.list_components("system", "concept")
        self.knowledge_tree.data["system"]["concept"] = {
            concept: {"docstring": self.engine.get_docstring(concept).docstring}
            for concept in concepts.components
        }

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

    def base_context(self) -> str:
        base_context_path = os.path.join(
            os.path.dirname(__file__), "prompts/base_context.md"
        )
        with open(base_context_path) as f:
            base_context = f.read()
        return base_context

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
        is_entity = False
        docstring = None
        if self.engine.is_entity(component_val).result:
            is_entity = True
            docstring = self.engine.get_docstring(component_val).docstring

        self.knowledge_tree.remember_component(
            entity=entity,
            component_name=component_name,
            component_val=component_val,
            is_entity=is_entity,
            docstring=docstring,
        )

    def run_exploration(self, task: str, max_messages: int = 10):
        """Run exploration phase and validate results"""

        EXPLORATION_PROMPT = dedent(
            r"""
        Core Premise:
        {base_context}

        You have access to the MyPAOS Prolog knowledge base through these tools.
        Your task is exploring what's relevant for the current request.

        IMPORTANT: Remember relevant components using `remember_component` tool, e.g.
        {{ "tool_name": "remember_component", "parameters": {{ "entity": "command", "component_name": "ctor", "component_val": "mkdir" }}, "reason": "The user asked for creating a new project directory." }}
        THIS IS YOUR SOLE RESPONSIBILITY TO REMEMBER RELEVANT COMPONENTS.
        PRIORITIZE calling `remember_component` for components that are relevant to the task.
        THIS IS EXTREMELY IMPORTANT FOR THE PLANNING PHASE.
        THE MOMENT YOU FIND SOMETHING RELEVANT, IMMEDIATELY CALL `remember_component`.

        Example exploration:

            User: TASK: Create a new project directory and initialize a flake.nix file

            AI:
                STEP 1: Check command entity constructors
                STEP 2: {{
                    "tool_name": "list_constructors",
                    "parameters": {{"entity": "command"}},
                    "reason": "List all available commands."
                }}
                USER: ["mkdir", "mkfile", "mkproject", "nix(flake(init))", ...]

                STEP 3: Get documentation for relevant command constructor mkproject
                STEP 4: {{
                    "tool_name": "get_docstring",
                    "parameters": {{ "entity": "mkproject" }},
                    "reason": "Understand what the 'mkproject' command does."
                }}
                USER: "Creates a new project directory with full initialization..."

                STEP 5: This command is relevant for creating a new project directory, remember it.
                STEP 6: {{
                    "tool_name": "remember_component",
                    "parameters": {{
                        "entity": "command",
                        "component_name": "ctor",
                        "component_val": "mkproject"
                    }},
                    "reason": "Remember the 'mkproject' for project creation transaction."
                }}

                STEP 7: Get documentation for relevant command constructor `nix(flake(init))`
                STEP 8: {{
                    "tool_name": "get_docstring",
                    "parameters": {{ "entity": "nix(flake(init))" }},
                    "reason": "Understand what the 'nix(flake(init))' command does."
                }}
                USER: "Initialize a flake.nix file in the project directory..."
                STEP 9: This command is relevant for initializing a flake.nix file, remember it.
                STEP 10: {{
                    "tool_name": "remember_component",
                    "parameters": {{
                        "entity": "command",
                        "component_name": "ctor",
                        "component_val": "nix(flake(init))"
                    }},
                    "reason": "The user asked for initializing a flake.nix file."
                }}

                Step 11: return: mkproject and nix(flake(init)) are relevant for creating a new project directory and initializing a flake.nix file.

        Available tools:
        {tools}

        AGAIN, REMEMBER RELEVANT COMPONENTS USING `remember_component` TOOL.
        THIS IS CRUCIAL FOR THE PLANNING PHASE.
        CALL `remember_component` IMMEDIATELY WHEN YOU FIND SOMETHING RELEVANT.
        IF YOU DON'T CALL `remember_component` FOR A RELEVANT COMPONENT,
        IT WILL NOT BE AVAILABLE FOR PLANNING - AND I WILL BE FORCED TO FINE TUNE YOUR WEIGHTS.
        YOUR CONCLUSION SHOULD BE A SUMMARY OF THE EXPLORATION, NOT A LISTING OF COMPONENTS.


        Response format:
        {retval_format}

        Current Knowledge Tree State, you SHOULD use `remember_component` to add more relevant components:
        {knowledge_tree}
        """
        )

        tools_kb = [make_tool(self.remember_component)]
        tools = self.tools + tools_kb
        toolset = Toolset(tools={tool.name: tool for tool in tools})

        def system_prompt_gen(retval_model: Type[str], toolset: Toolset) -> str:
            return EXPLORATION_PROMPT.format(
                base_context=self.base_context(),
                tools=toolset.to_prompt(),
                retval_format="string",
                knowledge_tree=self.knowledge_tree.to_prompt(),
            )

        processed_message = self.llm.process_message(
            task,
            system_prompt_gen=system_prompt_gen,
            max_messages=max_messages,
            toolset=toolset,
            retval_model=str,
        )

        thought_id = None
        # Log the exploration phase thoughts
        for thought in processed_message.thoughts:
            thought_id = self.logger.log_thought(thought, thought_id)

        result = ProcessResult[KnowledgeTree](
            thoughts=processed_message.thoughts, retval=self.knowledge_tree.model_copy()
        )
        return result

    def run_planning(
        self, task: str, exploration: KnowledgeTree, max_messages: int = 10
    ):
        """Run planning phase and validate results"""

        PLANNING_PROMPT = dedent(
            """
        Core Premise:
        {base_context}

        Your task is to use the exploration results to plan the necessary commands for solving the task.
        Create a transaction plan that will accomplish this task.
        Explain your reasoning for each command.

        Note:
            - Based on exploration, identify needed commands
            - Commands are constructors of the command entity
            - Each command variant has specific parameters
            - Build a transaction list of commands

        Example planning:
        Assuming the following knowledge tree:
        {{
        "system": {{
            "concepts": ["command", "git", "nix", ...],
            "docstring": "...",
            "command": {{
            "constructors": ["mkdir", "mkfile", "mkproject", "nix(flake(init))", ...],
            "docstring": "...",
            "mkproject": {{
                "parameters": ["path", "options"]
                "docstring": "Creates a new project directory with full initialization.\nFormat: mkproject(Path, Options)\nAvailable Options:\n  - git: Initialize a git repository\n  - template: Use a specific template\n  - nix: Use Nix package manager\n",
                "options": {{
                "git": {{
                    "parameters": ["true", "false"]
                    "docstring": "Initialize a git repository",
                }},
                "template": {{
                    "parameters": ["basic", "full"]
                    "docstring": "Use a specific template",
                }},
                }},
                "git": {{
                "parameters": ["init"]
                "docstring": "Git version control system",
                }}
            }},
            "nix(flake(init))": {{
                "parameters": ["path"]
                "docstring": "Initialize a flake.nix file in the project directory\nFormat: nix(flake(init(Path)))",
            }}
            }}
        }}
        }}
        TASK: Create a new project directory and initialize a flake.nix file
        AI: return: {{
        "summary": "To create a new project directory and initialize a flake.nix file, we will use the mkproject and nix(flake(init)) commands.",
        "commands": [
            {{
            "command": "mkproject('/path/to/project', [git(true), template(basic)])",
            "rationale": "The mkproject command is used to create a new project directory with git initialization and a basic template."
            }},
            {{
            "command": "nix(flake(init('/path/to/project')))",
            "rationale": "Initialize a flake.nix file in the project directory."
            }}
        ],
        }}

        Relevant Knowledge:
        {knowledge_tree}

        Response format:
        {retval_format}
        """
        )

        def system_prompt_gen(retval_model: Type[Plan], toolset: Toolset) -> str:
            return PLANNING_PROMPT.format(
                base_context=self.base_context(),
                retval_format=Plan.model_json_schema(mode="serialization"),
                knowledge_tree=exploration.to_prompt(),
            )

        result: ProcessResult[Plan] = self.llm.process_message(
            task,
            system_prompt_gen=system_prompt_gen,
            retval_model=Plan,
            max_messages=max_messages,
        )

        thought_id = None
        # Log the planning phase thoughts
        for thought in result.thoughts:
            thought_id = self.logger.log_thought(thought, thought_id)

        return result

    def run_execution(self, task: str, plan: Plan, max_messages: int = 10):
        """Execute transaction and get interpretation"""

        EXECUTION_PROMPT = dedent(
            """
        Core Premise:
        {base_context}

        You are tasked with interpreting the transaction's execution result, given the task.

        Executed transaction:
        {transaction_result}

        Response format:
        {retval_format}
        """
        )

        def system_prompt_gen(retval_model: Type[str], toolset: Toolset) -> str:
            return EXECUTION_PROMPT.format(
                base_context=self.base_context(),
                transaction_result=plan.to_prompt(),
                retval_format="string",
            )

        commands = [cmd.command for cmd in plan.commands]
        transaction_result = self.engine.commit_transaction(commands)

        exec_result = self.llm.process_message(
            task,
            system_prompt_gen=system_prompt_gen,
            max_messages=max_messages,
            retval_model=str,
        )

        # Log execution phase thoughts and transaction
        last_thought = None
        for thought in exec_result.thoughts:
            thought_id = self.logger.log_thought(thought, last_thought)
            last_thought = thought_id

        if last_thought is not None:
            self.logger.log_transaction(last_thought, commands, transaction_result)

        return exec_result

    def process_task(self, task: str, max_messages: int = 10):
        """Process message through all phases with validation"""
        # Exploration Phase
        exploration_result = self.run_exploration(task, max_messages=max_messages)

        # Planning Phase
        plan_result = self.run_planning(
            task, exploration=exploration_result.retval, max_messages=max_messages
        )
        plan = plan_result.retval
        # Execution Phase (if needed)
        if len(plan.commands) > 0:
            execution_result = self.run_execution(task, plan, max_messages=max_messages)
            return execution_result.retval

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
