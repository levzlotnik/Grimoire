"""
Test Planner Golem

Creates comprehensive test plans for code with coverage analysis and edge case detection.
"""

import os
from typing import List
from pydantic import BaseModel, Field

from grimoire_golems.core import Golem, Config


class TestPlan(BaseModel):
    """Comprehensive test plan structure."""
    test_cases: List[str] = Field(description="List of test cases to implement")
    coverage_areas: List[str] = Field(description="Areas of code that need coverage")
    edge_cases: List[str] = Field(description="Edge cases to test")
    test_strategy: str = Field(description="Overall testing strategy")


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
    output_type=TestPlan,
    temperature=0.4,
    max_tokens=6000
)

# Instantiate the golem
golem = Golem(golem_id="test_planner", config=config, session_id="main")

# Register with grimoire_golems.core registry
from grimoire_golems.core import register_golem
register_golem("test_planner", golem)
