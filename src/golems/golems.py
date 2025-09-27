"""
Grimoire Golems Module

Imports all available golem instances for easy access.
"""

import sys
import os

# Add current directory to path to enable direct imports
current_dir = os.path.dirname(os.path.abspath(__file__))
if current_dir not in sys.path:
    sys.path.append(current_dir)

# Import all golem instances
from code_assistant import golem as code_assistant
from architect import golem as architect
from code_reviewer import golem as code_reviewer
from documentation import golem as documentation
from project_manager import golem as project_manager
from semantics_verifier import golem as semantics_verifier
from test_planner import golem as test_planner
from test_runner import golem as test_runner

# Make all golems available at module level
__all__ = [
    'code_assistant',
    'architect', 
    'code_reviewer',
    'documentation',
    'project_manager',
    'semantics_verifier',
    'test_planner',
    'test_runner'
]