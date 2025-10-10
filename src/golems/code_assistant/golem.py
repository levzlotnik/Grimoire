"""
Code Assistant Golem

Expert software engineer specialized in code generation, review, and refactoring.
"""

import os
from typing import List, Optional
from pydantic import BaseModel, Field

from grimoire_golems.core import Golem, Config


class CodeResponse(BaseModel):
    """Response from code generation tasks."""
    code: str = Field(description="Generated code")
    language: str = Field(description="Programming language")
    tests: Optional[List[str]] = Field(default=None, description="Suggested test cases")
    documentation: Optional[str] = Field(default=None, description="Code documentation")
    explanation: Optional[str] = Field(default=None, description="Explanation of the code")


def get_readme() -> str:
    """Read the README.md file for system prompt."""
    readme_path = os.path.join(os.path.dirname(__file__), "README.md")
    with open(readme_path, 'r', encoding='utf-8') as f:
        return f.read()


# System prompt from README.md
system_prompt = get_readme()

# Golem configuration
config = Config(
    model="openai:gpt-5-nano",
    system_prompt=system_prompt,
    output_type=CodeResponse,
    temperature=0.1,
    max_tokens=8192
)

# Instantiate and register the golem
from grimoire_golems.core import register_golem
golem = Golem(golem_id="code_assistant", config=config, session_id="main")
register_golem("code_assistant", golem)
