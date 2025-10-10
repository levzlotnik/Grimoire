"""
Grimoire Golems Module

Imports all golem modules to trigger their registration with grimoire_golems.core registry.
"""

import sys
import os

# Add current directory to path to enable direct imports
current_dir = os.path.dirname(os.path.abspath(__file__))
if current_dir not in sys.path:
    sys.path.append(current_dir)

# Import all golems - they will self-register with grimoire_golems.core
from code_assistant import golem as code_assistant_golem
from architect import golem as architect_golem
from code_reviewer import golem as code_reviewer_golem
from documentation import golem as documentation_golem
from project_manager import golem as project_manager_golem
from semantics_verifier import golem as semantics_verifier_golem
from test_planner import golem as test_planner_golem
from test_runner import golem as test_runner_golem
