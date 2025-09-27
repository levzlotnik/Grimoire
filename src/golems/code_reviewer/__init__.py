"""
Code Reviewer Golem

Performs comprehensive code reviews with security and performance analysis.
"""

import os
from typing import List, Optional
from pydantic import BaseModel, Field

from grimoire_golems.core import Golem, Config


class CodeReview(BaseModel):
    """Response from code review tasks."""
    issues: List[str] = Field(description="Identified issues in the code")
    suggestions: List[str] = Field(description="Improvement suggestions")
    security_concerns: Optional[List[str]] = Field(default=None, description="Security vulnerabilities and concerns")
    performance_notes: Optional[List[str]] = Field(default=None, description="Performance optimization notes")
    overall_quality: str = Field(description="Overall code quality assessment")


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
    output_type=CodeReview,
    temperature=0.3,
    max_tokens=8192
)

# Instantiate the golem
golem = Golem(id="code_reviewer", config=config, session_id="main")