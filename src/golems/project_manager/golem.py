"""
Project Manager Golem

Senior project manager responsible for coordinating development tasks.
"""

import os
from typing import List, Optional, Dict, Any
from pydantic import BaseModel, Field

from grimoire_golems.core import Golem, Config


class ProjectAnalysis(BaseModel):
    """Analysis of project structure and dependencies."""
    structure: Dict[str, Any] = Field(description="Project structure information")
    dependencies: List[str] = Field(description="List of project dependencies")
    entry_points: List[str] = Field(description="Project entry points")
    configuration_files: List[str] = Field(description="Configuration files in the project")
    recommendations: Optional[List[str]] = Field(default=None, description="Recommendations for project improvement")


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
    output_type=ProjectAnalysis,
    temperature=0.4,
    max_tokens=8192
)

# Instantiate the golem
golem = Golem(golem_id="project_manager", config=config, session_id="main")

# Register with grimoire_golems.core registry
from grimoire_golems.core import register_golem
register_golem("project_manager", golem)
