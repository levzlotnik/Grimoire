"""
Semantics Verifier Golem

Verifies that all meaningful source code files are tested in semantics.plt
"""

import os
from typing import List, Optional
from pydantic import BaseModel, Field

from grimoire_golems.core import Golem, Config


class SemanticsVerification(BaseModel):
    """Response from semantics verification tasks."""
    covered_files: List[str] = Field(description="Files that are covered by tests")
    missing_files: List[str] = Field(description="Files that are missing test coverage")
    suggestions: Optional[List[str]] = Field(default=None, description="Suggestions for improving test coverage")
    coverage_percentage: float = Field(description="Percentage of files covered by tests")


def get_readme() -> str:
    """Read the README.md file for system prompt."""
    readme_path = os.path.join(os.path.dirname(__file__), "README.md")
    with open(readme_path, 'r', encoding='utf-8') as f:
        return f.read()


# System prompt from README.md
system_prompt = get_readme()

# Golem configuration
config = Config(
    model="anthropic:claude-sonnet-4-20250514",
    system_prompt=system_prompt,
    output_type=SemanticsVerification,
    temperature=0.2,
    max_tokens=4096
)

# Instantiate the golem
golem = Golem(id="semantics_verifier", config=config, session_id="main")