"""
Test Runner Golem

QA engineer focused on testing, test automation, and quality assurance.
"""

import os
from typing import List, Dict, Optional, Any
from pydantic import BaseModel, Field

from grimoire_golems.core import Golem, Config


class TestResult(BaseModel):
    """Response from test execution and analysis."""
    passed: int = Field(description="Number of tests passed")
    failed: int = Field(description="Number of tests failed")
    skipped: int = Field(description="Number of tests skipped")
    failures: Optional[List[Dict[str, Any]]] = Field(default=None, description="Details of failed tests")
    coverage: Optional[float] = Field(default=None, description="Code coverage percentage")
    test_plan: Optional[str] = Field(default=None, description="Recommended test plan")
    test_cases: Optional[List[str]] = Field(default=None, description="Generated test cases")
    recommendations: Optional[List[str]] = Field(default=None, description="Testing improvements")


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
    output_type=TestResult,
    temperature=0.0,
    max_tokens=8192
)

# Instantiate the golem
golem = Golem(golem_id="test_runner", config=config, session_id="main")