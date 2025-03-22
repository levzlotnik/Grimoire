import json
from typing import Callable, List
from .llm.llm_provider import get_llm_provider
from .llm.tool_calling import Tool, ToolCallingLLM, make_tool
from .prolog.prolog_tool import PrologToolset
from textwrap import dedent


class MyPAOSAssistant:
    """Assistant that uses LLM to interact with the operating system."""

    def __init__(
        self, provider: str = "claude", model: str = "claude-3-5-sonnet-20240620"
    ):
        self.engine = PrologToolset()

        tools = [
            self.engine.apply_action,
            self.engine.make_rule,
            self.engine.get_all_rules,
            self.engine.get_object,
            self.engine.query,
        ]

        tools = [make_tool(t) for t in tools]

        self.llm = ToolCallingLLM(
            system_prompt=self._make_system_prompt(tools),
            llm_provider=get_llm_provider(provider),
            model=model,
        )

        # Add Prolog engine tools
        self.llm.add_tools(tools)

    def _make_system_prompt(self, tools: List[Tool]) -> str:
        def _format_tool(t: Tool) -> str:
            return_schema = (
                json.dumps(t.return_schema, indent=2) if t.return_schema else "None"
            )
            return dedent(
                f"""
                Tool: {t.name}
                Description: {t.description}
                Parameters: {json.dumps(t.parameters, indent=2)}
                Return schema: {return_schema}
            """
            ).strip()

        tools_desc = "\n".join(_format_tool(t) for t in tools)

        system_prompt = dedent(
            f"""
            You are a helpful assistant that embodies the philosophy of MyPAOS:
            Every system operation is a knowledge-based transaction, where both human intent and system
            capabilities are expressed through formal logic. By combining LLM reasoning with Prolog's
            precise semantics, we create a self-describing, self-modifying operating system that learns
            from user interactions while maintaining strict correctness through logical rules. The system
            treats everything - from UI components to IoT devices to user preferences - as queryable
            knowledge, enabling natural language control while preserving reproducibility through NixOS
            and transactional integrity.

            You can query this knowledge base using these tools:

            {tools_desc}

            You shouldn't assume to know the answer, you should always use the tools to find the
            answer. The prolog knowledge base is not always complete, so you may need to use
            multiple tools to get the answer. In particular, you should use the `get_all_rules` tool
            to get all the rules that currently exist in the context,
            and then use the `query` tool to check if the rule applies to the current situation.

            Treat the Prolog knowledge base as a strict set of rules and facts.
            You can suggest to add facts/rules, but you should only add rules if you don't see a
            rule that covers the current situation and are explicitly told to add a rule.

            Think step by step. For each step, you must:
            1. If you need to use a tool:
               - Make the tool call in JSON format
               - Wait for the tool result before continuing
            2. If you're reasoning:
               - Explain your thought process
               - Continue to the next step

            IMPORTANT:
            - MAKE SURE YOU HAVE ALL THE INFORMATION YOU NEED BY MAKING APPROPRIATE CALLS
            TO `get_all_rules` AND `query` TOOLS. THIS IS REALLY CRITICAL, PROLOG CONTAINS ALL THE
            ACTUAL KNOWLEDGE ABOUT THE OS, AND YOU NEED TO USE IT TO MAKE THE BEST DECISIONS.
            - AFTER MAKING A TOOL CALL, YOU MUST STOP AND WAIT FOR THE RESULT BEFORE CONTINUING.
            - DO NOT SAY ANYTHING ELSE AFTER MAKING A TOOL CALL, JUST WAIT FOR THE RESULT.
            - DO NOT MAKE MULTIPLE TOOL CALLS AT ONCE.
            - DO NOT WRAP TOOL CALLS IN QUOTES, OR PUT THEM IN A CODE BLOCK.
              JUST PUT THEM IN JSON FORMAT.
              That means you should not do things like this:

              STEP 2:
              ```json
              {{
                "tool_name": "get_all_rules",
                "parameters": {{}},
                "reason": "Need to understand what rules govern movement and interactions"
              }}
              ```

              Instead, you should do this:

              STEP 2: {{
                "tool_name": "get_all_rules",
                "parameters": {{}},
                "reason": "Need to understand what rules govern movement and interactions"
              }}
            - ALWAYS INCLUDE A STEP NUMBER FOR EACH THOUGHT, EVEN IF IT'S THE CONCLUSION.

            Format your steps as:
            STEP X: <your thought process OR tool call in JSON format>

            If you made a tool call, wait for the result before continuing.

            If you have a conclusion, state it as:
            STEP X: Conclusion: <your conclusion>

            Example:

            User: Can you make a full stack application project for me? What are the possible
            frameworks I can use for this?

            AI:
            STEP 1: First, I need to check what knowledge is available in the system.
            STEP 2: {{
                "tool_name": "get_all_rules",
                "parameters": {{}},
                "reason": "Need to understand what rules govern the project creation"
            }}

            User: {{
                "result": [
                    {{
                        "rule": "project_development(Usecase, Stack, SemanticsFile)",
                        "explanation": "A relation that defines the project development process with for a Usecase, using a Stack and the SemanticsFile of the stack."
                    }},
                    {{
                        "rule": "component(Entity, Component, Value)",
                        "explanation": "Checks if Entity has Component with Value"
                    }}
                ]
            }}

            AI:
            STEP 3: I see we have a rule `project_development` that I can query for what possible
            application stacks I can use. I will use the `query` tool to get the possible stacks.
            STEP 4: {{
                "tool_name": "query",
                "parameters": {{
                    "query": "project_development(Usecase, Stack, Semantics)"
                }},
                "reason": "Need to understand what application stacks are available"
            }}

            User: {{
                "result": [
                    {{
                        "Usecase": "web_ui",
                        "Stack": "react",
                        "Semantics": "~/philosophy/react_semantics.pl"
                    }},
                    {{
                        "Usecase": "cross_platform_ui",
                        "Stack": "flutter",
                        "Semantics": "~/philosophy/flutter_semantics.pl"
                    }},
                    {{
                        "Usecase": "backend",
                        "Stack": "flask",
                        "Semantics": "~/philosophy/flask_semantics.pl"
                    }},
                    {{
                        "Usecase": "backend",
                        "Stack": "rust_actix",
                        "Semantics": "~/philosophy/rust_actix.pl"
                    }}
                ]
            }}

            AI:
            STEP 5: I see we have the following stacks available:
            - react
            - flutter
            - flask
            - rust_actix

            Let's say I want to use the `react` stack. I need to peek into the semantics file to
            understand what components are available for this stack.
            STEP 6: {{
                "tool_name": "query",
                "parameters": {{
                    "query": "peek_semantics("~/philosophy/react_semantics.pl", Semantics), component(Semantics, Component, Value)"
                }},
                "reason": "Need to understand what components are available for the react stack"
            }}

            User: {{
                "result": [
                    {{
                        "Component": "source_code_files_path",
                        "Value": "path(\"./src\")"
                    }},
                    {{
                        "Component": "generate_project",
                        "Value": "[command(npx(create-react-app, [\"my_app\"]))]"
                    }},
                    {{
                        "Component": "build_project",
                        "Value": "[command(npx(build, [\"my_app\"]))]"
                    }}
                ]
            }}

            AI:
            STEP 7: I see we have the following components available for the `react` stack:
            - source_code_files_path
            - generate_project
            - build_project

            // And so on for the other stacks...

            Conclusion: I can use the `react` stack to create a full stack application project.
            If you want, I can use the following transaction to generate and build the project:
            [[
                command(npx(create-react-app, ["my_app"]))
                command(npx(build, ["my_app"]))
            ]]
        """
        ).strip()
        return system_prompt

    def process_message(self, message: str) -> str:
        """Process a message from the user."""
        return self.llm.process_message(message)


if __name__ == "__main__":
    assistant = MyPAOSAssistant(provider="openai", model="gpt-4o")

    # Example interaction
    responses = [
        assistant.process_message(
            "Create a full stack application project for me. "
            "What are the possible frameworks I can use for this?"
        ),
        assistant.process_message(
            'What is the "project_development" rule?'
        ),
        assistant.process_message(
            "What is the 'component' rule?"
        ),
    ]
    for r in responses:
        print(r)
        print("---")
