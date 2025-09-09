"""
Janus bridge module for Grimoire Golems.

Auto-registers Python functions when imported, providing
seamless integration between Prolog and Python layers.
"""

from typing import Dict, List, Any

# Import our manager functions
from .core.manager import execute_task


def register_functions():
    """
    Register Python functions with Janus for Prolog access.
    
    This function is called from Prolog during system initialization
    to make Python functionality available to the Prolog layer.
    
    Note: Thought logging is handled by Prolog's existing think command
    """
    try:
        # No need to register functions manually with janus-swi
        # Functions are accessible directly via py_call
        return "success"
    except Exception as e:
        return f"error: {e}"