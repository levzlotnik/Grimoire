"""
Architect Golem

Software architect with expertise in design patterns, system architecture, and best practices.
"""

import os
from typing import List, Optional
from pydantic import BaseModel, Field

from grimoire_golems.core import Golem, Config


class ArchitecturalPlan(BaseModel):
    """Response from architectural analysis and design tasks."""
    patterns_used: List[str] = Field(description="Design patterns identified or recommended")
    strengths: List[str] = Field(description="Architectural strengths")
    weaknesses: List[str] = Field(description="Architectural weaknesses or concerns")
    recommendations: List[str] = Field(description="Strategic recommendations for improvement")
    diagram: Optional[str] = Field(default="", description="ASCII diagram or architectural description")


class RefactoringPlan(BaseModel):
    """Response from refactoring analysis tasks."""
    targets: List[str] = Field(description="Code targets that need refactoring")
    strategy: str = Field(description="Overall refactoring strategy")
    steps: List[str] = Field(description="Detailed refactoring steps")
    risk_assessment: str = Field(description="Risk assessment of the refactoring")
    expected_benefits: List[str] = Field(description="Expected benefits from refactoring")


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
    output_type=ArchitecturalPlan,
    temperature=0.5,
    max_tokens=8192
)

# Instantiate the golem
golem = Golem(id="architect", config=config, session_id="main")

# Refactoring specialist variant configuration
refactoring_config = Config(
    model="anthropic:claude-sonnet-4-20250514",
    system_prompt="You are a refactoring specialist. Identify code that needs refactoring and create detailed refactoring plans with risk assessments.",
    output_type=RefactoringPlan,
    temperature=0.2,
    max_tokens=6000
)

# Refactoring specialist golem
refactoring_golem = Golem(id="refactoring_specialist", config=refactoring_config, session_id="main")