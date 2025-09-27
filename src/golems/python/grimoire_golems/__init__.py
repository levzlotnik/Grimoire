"""
Grimoire Golems AI Agent Framework

Provides autonomous AI agents built on Entity-Component-System architecture.
Integrates with the existing GrimoireInterface for seamless tool discovery
and supports multiple LLM providers for flexible AI service integration.
"""

from .core import Golem
from .core.golem import Config

__version__ = "0.1.0"
__all__ = ["Golem", "Config"]