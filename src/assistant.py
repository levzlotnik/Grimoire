import json
import os
from typing import Callable, List, Dict, Any, Optional, Type
from llm_provider import get_llm_provider
from tool_calling import ProcessResult, Tool, ToolCallingLLM, Toolset, make_tool
from prolog_tool import PrologToolset
from textwrap import dedent
from pydantic import BaseModel, Field, ValidationError, model_validator
from common import ToolError


class ExplorationResult(BaseModel):
    """Knowledge accumulated during exploration phase"""

    knowledge_tree: dict = Field(
        default_factory=lambda: {
            "system": {"concepts": [], "docstring": None}  # Will be filled in __init__
        },
        description="Knowledge tree containing relevant entities and components",
    )
    summary: Dict[str, str] = Field(
        default_factory=dict, description="Summary of the exploration results"
    )

    def to_prompt(self) -> str:
        """Convert knowledge tree to a prompt-friendly format"""
        return json.dumps(self.model_dump(), indent=2)

    @model_validator(mode="after")
    def validate_data(self):
        """Ensure the knowledge tree is properly structured"""
        if "system" not in self.knowledge_tree:
            raise ValidationError("Knowledge tree must contain system entity")
        if "concepts" not in self.knowledge_tree["system"]:
            raise ValidationError("System entity must contain concepts")
        return self


class CommandExplanation(BaseModel):
    """Command with its explanation"""

    command: str
    rationale: str


class PlanningResult(BaseModel):
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
        self, provider: str = "claude", model: str = "claude-3-5-sonnet-20240620"
    ):
        self.engine = PrologToolset()

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

        # Create base exploration context
        self.base_exploration = ExplorationResult()
        concepts = self.engine.list_components("system", "concept")
        self.base_exploration.knowledge_tree["system"]["concepts"] = {
            concept: {"docstring": self.engine.get_docstring(concept).docstring}
            for concept in concepts.components
        }
        self.base_exploration.knowledge_tree["system"]["docstring"] = (
            self.engine.get_docstring("system").docstring
        )

        self.llm = ToolCallingLLM(
            llm_provider=get_llm_provider(provider),
            model=model,
        )

    def base_context(self) -> str:
        base_context_path = os.path.join(
            os.path.dirname(__file__), "prompts/base_context.md"
        )
        with open(base_context_path) as f:
            base_context = f.read()
        return base_context

    def run_exploration(self, task: str, max_messages: int = 10):
        """Run exploration phase and validate results"""

        EXPLORATION_PROMPT = dedent(
            r"""
        Core Premise:
        {base_context}

        You have access to the MyPAOS Prolog knowledge base through these tools.
        Your task is exploring what's relevant for the current request.

        IMPORTANT: Remember relevant components using `remember_component` tool, e.g.
        `{{ "tool_name": "remember_component", "parameters": {{"entity": "system.command", "component_name": "mkdir", "component_json_value": "{{\"docstring\": \"Creates a new directory\nFormat: mkdir(Path)\"}}", "summary": "The user asked for creating a new directory."}}}}`
        THIS IS YOUR SOLE RESPONSIBILITY TO REMEMBER RELEVANT COMPONENTS.
        PRIORITIZE calling `remember_component` for components that are relevant to the task.
        THIS IS EXTREMELY IMPORTANT FOR THE PLANNING PHASE.
        THE MOMENT YOU FIND SOMETHING RELEVANT, IMMEDIATELY CALL `remember_component`.

        Example exploration:

            User: TASK: Create a new project directory and initialize a flake.nix file

            AI:
                STEP 1: Check command entity constructors
                STEP 2: {{"tool_name": "list_constructors", "parameters": {{"entity": "command"}}}}
                USER: ["mkdir", "mkfile", "mkproject", "nix(flake(init))", ...]

                STEP 3: Get documentation for relevant command constructor mkproject
                STEP 4: {{"tool_name": "get_docstring", "parameters": {{"entity": "mkproject"}}}}
                -> "Creates a new project directory with full initialization..."

                STEP 5: This command is relevant for creating a new project directory, remember it.
                STEP 6: {{ "tool_name": "remember_component", "parameters": {{"entity": "system.command", "component_name": "mkproject", "component_json_value": "{{\"docstring\": \"Creates a new project directory with full initialization.\\nFormat: mkproject(Path, Options)\\nAvailable Options:\\n  - git: Initialize a git repository\\n  - template: Use a specific template\\n  - nix: Use Nix package manager\\n\"}}, "summary": "The user asked for creating a new project directory."}}}}

                STEP 7: Get documentation for relevant command constructor `nix(flake(init))`
                STEP 8: {{"tool_name": "get_docstring", "parameters": {{"entity": "nix(flake(init))"}}}}
                -> "Initialize a flake.nix file in the project directory..."
                STEP 9: This command is relevant for initializing a flake.nix file, remember it.
                STEP 10: {{ "tool_name": "remember_component", "parameters": {{"entity": "system.command", "component_name": "nix(flake(init))", "component_json_value": "{{\"docstring\": \"Initialize a flake.nix file in the project directory\\nFormat: nix(flake(init(Path))\\n\"}}", "summary": "The user asked for initializing a flake.nix file."}}}}

                Step 11: return: mkproject and nix(flake(init)) are relevant for creating a new project directory and initializing a flake.nix file.

        Available tools:
        {tools}

        AGAIN, REMEMBER RELEVANT COMPONENTS USING `remember_component` TOOL. THIS IS CRUCIAL FOR THE PLANNING PHASE.

        Response format:
        {retval_format}

        Current Knowledge Tree State, you SHOULD use `remember_component` to add more relevant components:
        {knowledge_tree}
        """
        )

        exploration_result = self.base_exploration.model_copy()

        def remember_component(entity_id: str, component_name: str, summary: str):
            """Remember a component in the knowledge tree.

            Args:
                entity_id: the `.` delimited entity ID, e.g. "system.command"
                component_name: the name of the component, e.g. "mkdir"
                summary: the reasoning for remembering the component

            Note:
                - If the component is not an entity, it will be added to the `values` list
                - If the component is an entity, it will be added as a key in the knowledge tree
                  along with its docstring
            """
            components_list = self.engine.list_components(
                entity_id, component_name
            ).components
            # Reach the entity in the knowledge tree
            entity = exploration_result.knowledge_tree
            for entity_part in entity_id.split("."):
                if isinstance(entity, dict):
                    if entity_part not in entity:
                        entity[entity_part] = {}
                    entity = entity[entity_part]
                else:
                    raise ToolError(
                        f"Invalid entity ID '{entity_id}': '{entity_part}' is not a dict"
                    )
            # Add the components to the entity
            for component in components_list:
                if not self.engine.is_entity(component):
                    if "values" not in entity:
                        entity["values"] = []
                    entity["values"].append(component)
                elif component not in entity.keys():
                    entity[component] = {
                        "docstring": self.engine.get_docstring(component).docstring
                    }
            # Update the summary
            exploration_result.summary[f"{entity_id}.{component_name}"] = summary

        tools_kb = [make_tool(remember_component)]
        tools = self.tools + tools_kb
        toolset = Toolset(tools={tool.name: tool for tool in tools})

        def system_prompt_gen(retval_model: Type[str], toolset: Toolset) -> str:
            return EXPLORATION_PROMPT.format(
                base_context=self.base_context(),
                tools=toolset.to_prompt(),
                retval_format="string",
                knowledge_tree=exploration_result.to_prompt(),
            )

        processed_message = self.llm.process_message(
            task,
            system_prompt_gen=system_prompt_gen,
            max_messages=max_messages,
            toolset=toolset,
            retval_model=str,
        )

        result = ProcessResult[ExplorationResult](
            thoughts=processed_message.thoughts, retval=exploration_result
        )
        return result

    def run_planning(
        self, task: str, exploration: ExplorationResult, max_messages: int = 10
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

        result: ProcessResult[PlanningResult] = self.llm.process_message(
            task,
            system_prompt_template=PLANNING_PROMPT,
            knowledge_tree=exploration.to_prompt(),
            retval_model=PlanningResult,
            max_messages=max_messages,
        )
        return result

    def run_execution(self, task: str, plan: PlanningResult, max_messages: int = 10):
        """Execute transaction and get interpretation"""
        # Execute transaction directly

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

        commands = [cmd.command for cmd in plan.commands]
        transaction_result = self.engine.commit_transaction(commands)

        # Have LLM interpret the results
        return self.llm.process_message(
            task,
            system_prompt_template=EXECUTION_PROMPT,
            transaction_result=transaction_result,
            max_messages=max_messages,
        )

    def process_message(self, task: str, max_messages: int = 10):
        """Process message through all phases with validation"""
        # Exploration Phase
        exploration_result = self.run_exploration(task, max_messages=max_messages)

        # Planning Phase
        plan_result = self.run_planning(
            task, exploration=exploration_result.retval, max_messages=max_messages
        )
        plan = plan_result.retval
        # Execution Phase (if needed)
        if plan.commands:
            return self.run_execution(task, plan, max_messages=max_messages)

        return "No commands to execute."


if __name__ == "__main__":
    assistant = MyPAOSAssistant(provider="openai", model="gpt-4o")

    # Example interaction
    print(
        assistant.process_message(
            "Create a full stack application project for me. "
            "What are the possible frameworks I can use for this?",
            max_messages=30,
        )
    )
    print(assistant.process_message('What is the "project_development" rule?'))
    print(assistant.process_message("What is the 'component' rule?"))
