"""
Documentation Golem

Technical documentation writer specialized in creating clear, comprehensive docs.
"""

import os
from typing import List, Optional
from pydantic import BaseModel, Field

from grimoire_golems.core import Golem, Config


class Documentation(BaseModel):
    """Structured documentation output."""
    summary: str = Field(description="Brief summary of the documented item")
    description: str = Field(description="Detailed description")
    parameters: Optional[List[str]] = Field(default=None, description="Parameters or arguments")
    returns: Optional[str] = Field(default=None, description="Return value or output description")
    examples: Optional[List[str]] = Field(default=None, description="Usage examples")


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
    output_type=Documentation,
    temperature=0.3,
    max_tokens=4096
)

# Instantiate the golem
golem = Golem(golem_id="documentation", config=config, session_id="main")

# Register with grimoire_golems.core registry
from grimoire_golems.core import register_golem
register_golem("documentation", golem)
