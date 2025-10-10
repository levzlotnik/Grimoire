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
    output_type=ArchitecturalPlan,
    temperature=0.5,
    max_tokens=8192
)

# Instantiate the golem
golem = Golem(golem_id="architect", config=config, session_id="main")

# Register with grimoire_golems.core registry
from grimoire_golems.core import register_golem
register_golem("architect", golem)
