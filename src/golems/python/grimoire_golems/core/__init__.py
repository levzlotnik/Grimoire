"""
Core golem functionality.
"""
from .golem import Golem, Config
from typing import Dict, Any, Optional


class GolemRegistry:
    """Registry for golem instances"""

    def __init__(self):
        self.golems: Dict[str, Any] = {}

    def register_golem(self, golem_id: str, golem_instance: Any) -> None:
        """Register a golem instance"""
        self.golems[golem_id] = golem_instance

    def get_golem(self, golem_id: str) -> Optional[Any]:
        """Get a registered golem by ID"""
        return self.golems.get(golem_id)

    def list_golems(self) -> list:
        """List all registered golem IDs"""
        return list(self.golems.keys())

    def has_golem(self, golem_id: str) -> bool:
        """Check if a golem is registered"""
        return golem_id in self.golems


# Global registry instance
_registry = GolemRegistry()


# Module-level functions for external use
def register_golem(golem_id: str, golem_instance: Any) -> None:
    """Register a golem in the global registry"""
    _registry.register_golem(golem_id, golem_instance)


def get_golem(golem_id: str) -> Optional[Any]:
    """Get a golem from the global registry"""
    return _registry.get_golem(golem_id)


def list_golems() -> list:
    """List all registered golem IDs"""
    return _registry.list_golems()


def has_golem(golem_id: str) -> bool:
    """Check if a golem is registered"""
    return _registry.has_golem(golem_id)


__all__ = ['Golem', 'Config', 'register_golem', 'get_golem', 'list_golems', 'has_golem']