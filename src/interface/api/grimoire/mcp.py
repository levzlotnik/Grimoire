"""
Grimoire MCP Server

Provides Model Context Protocol interface to Grimoire Prolog system.
Uses new Grimoire client with typed interface operations.
"""

import argparse
import yaml
from mcp.server import FastMCP
from typing import Dict, List, Any, Optional

from pydantic import BaseModel
from grimoire.client import Grimoire, GrimoireError

# Create global instance of Grimoire interface
grimoire = Grimoire()

# Get system instructions for MCP
try:
    system_instructions = grimoire.system_instructions().instructions
except GrimoireError:
    system_instructions = "Grimoire - Knowledge-Based Operating System"

# Create FastMCP server with proper instructions
mcp = FastMCP("Grimoire Interface", instructions=system_instructions)


def _model_to_string(model: Any) -> str:
    """Convert a model to YAML string representation for better nested structure readability"""
    if not isinstance(model, BaseModel):
        return str(model)
    # Convert Pydantic model to dict and then to YAML
    model_dict = model.model_dump()
    return yaml.dump(model_dict, default_flow_style=False, sort_keys=False)


@mcp.tool()
def component_types(entity: str = "system") -> str:
    """List all component types for an entity"""
    try:
        result = grimoire.component_types(entity)
        return _model_to_string(result)
    except GrimoireError as e:
        return f"Error: {e}"


@mcp.tool()
def components(entity: str, component_type: str) -> str:
    """Get verified components with smart singleton/set detection"""
    try:
        result = grimoire.components(entity, component_type)
        return _model_to_string(result)
    except GrimoireError as e:
        return f"Error: {e}"


@mcp.tool()
def docstring(entity: str = "system") -> str:
    """Get entity docstring"""
    try:
        result = grimoire.docstring(entity)
        return result.docstring
    except GrimoireError as e:
        return f"Error: {e}"


@mcp.tool()
def entities() -> str:
    """List all entities in the system"""
    try:
        result = grimoire.entities()
        return _model_to_string(result)
    except GrimoireError as e:
        return f"Error: {e}"


@mcp.tool()
def list_spells() -> str:
    """List all registered spells (conjure and perceive)"""
    try:
        spells = grimoire.list_all_spells()
        return yaml.dump({"spells": spells}, default_flow_style=False)
    except GrimoireError as e:
        return f"Error: {e}"


@mcp.tool()
def perceive(query_sig: str, args: Optional[Dict[str, Any]] = None) -> str:
    """
    Execute perception query.

    Args:
        query_sig: Query signature (e.g., "git(status)")
        args: Optional dict of arguments for template filling

    Example:
        perceive("git(status)", {})
    """
    try:
        result = grimoire.perceive(query_sig, args or {})
        return result.result
    except GrimoireError as e:
        return f"Error: {e}"


@mcp.tool()
def conjure(spell_sig: str, args: Optional[Dict[str, Any]] = None) -> str:
    """
    Execute conjuration spell.

    Args:
        spell_sig: Spell signature (e.g., "git(commit)")
        args: Dict of arguments for template filling

    Example:
        conjure("git(commit)", {"Message": "Initial commit"})
    """
    try:
        result = grimoire.conjure(spell_sig, args or {})
        return result.result
    except GrimoireError as e:
        return f"Error: {e}"


@mcp.tool()
def test(args: Optional[List[str]] = None) -> str:
    """
    Run test suite with optional args.

    Args:
        args: Optional list of test names or flags (e.g., ["--list"])
    """
    try:
        result = grimoire.test(args)
        return _model_to_string(result)
    except GrimoireError as e:
        return f"Error: {e}"


@mcp.tool()
def prove_it(entity: str, component_type: str, value: Any) -> str:
    """
    Component provenance - where generated and how verified.

    Args:
        entity: Entity name
        component_type: Component type
        value: Component value to trace
    """
    try:
        result = grimoire.prove_it(entity, component_type, value)
        return result.result
    except GrimoireError as e:
        return f"Error: {e}"


@mcp.tool()
def sauce_me(spell_ctor: str) -> str:
    """
    Spell metadata - source location, implementation, formats.

    Args:
        spell_ctor: Spell constructor (e.g., "git(commit)")
    """
    try:
        result = grimoire.sauce_me(spell_ctor)
        return result.result
    except GrimoireError as e:
        return f"Error: {e}"


@mcp.tool()
def exec_query(query_str: str) -> str:
    """
    Execute arbitrary Prolog query (developer mode - unlimited power).

    CAUTION: This bypasses all safety checks and can execute any Prolog code.

    Args:
        query_str: Raw Prolog query string

    Returns:
        YAML formatted list of solution dicts
    """
    try:
        solutions = grimoire.exec(query_str)
        return yaml.dump({"solutions": solutions}, default_flow_style=False)
    except GrimoireError as e:
        return f"Error: {e}"


def main(argv: Optional[List[str]] = None) -> None:
    """Main entry point for grimoire-mcp-server

    Args:
        argv: Command line arguments. If None, uses sys.argv[1:]
    """
    import sys

    if argv is None:
        argv = sys.argv[1:]

    parser = argparse.ArgumentParser(description="Grimoire MCP Server")
    parser.add_argument(
        "--transport",
        default="stdio",
        choices=["stdio", "sse"],
        help="Transport mode for MCP server"
    )
    args = parser.parse_args(argv)
    mcp.run(transport=args.transport)


if __name__ == "__main__":
    main()
