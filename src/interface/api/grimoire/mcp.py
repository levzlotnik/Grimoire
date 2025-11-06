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
from grimoire.client import (
    Grimoire,
    GrimoireError,
    ComponentTypesResponse,
    ComponentsResponse,
    DocstringResponse,
    EntitiesResponse,
    TestResponse,
    GenericResponse,
    SessionContextResponse,
)

# Create global instance of Grimoire interface
grimoire = Grimoire()

# Get system instructions for MCP
try:
    system_instructions = grimoire.system_instructions().instructions
except GrimoireError:
    system_instructions = "Grimoire - Knowledge-Based Operating System"

# Create FastMCP server with proper instructions
mcp = FastMCP("Grimoire Interface", instructions=system_instructions)


@mcp.tool()
def component_types(entity: Optional[str] = None) -> ComponentTypesResponse:
    """List all component types for an entity (defaults to focused entity or 'system')"""
    return grimoire.component_types(entity)


@mcp.tool()
def components(component_type: str, entity: Optional[str] = None) -> ComponentsResponse:
    """Get verified components with smart singleton/set detection (defaults to focused entity or 'system')"""
    return grimoire.components(entity, component_type)


@mcp.tool()
def docstring(entity: Optional[str] = None) -> DocstringResponse:
    """Get entity docstring (defaults to focused entity or 'system')"""
    return grimoire.docstring(entity)


@mcp.tool()
def entities() -> EntitiesResponse:
    """List all entities in the system"""
    return grimoire.entities()


@mcp.tool()
def list_spells() -> Dict[str, Any]:
    """List all registered spells (conjure and perceive)"""
    try:
        spells = grimoire.list_all_spells()
        return {"spells": spells}
    except GrimoireError as e:
        return {"error": str(e)}


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
def test(args: Optional[List[str]] = None) -> TestResponse:
    """
    Run test suite with optional args.

    Args:
        args: Optional list of test names or flags (e.g., ["--list"])
    """
    return grimoire.test(args)


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


@mcp.tool()
def focus_entity(entity: str) -> GenericResponse:
    """Focus on an entity by name for subsequent operations"""
    return grimoire.session_focus_entity(entity)


@mcp.tool()
def focus_path(path: str) -> GenericResponse:
    """Focus on an entity by path (looks up entity via self component)"""
    return grimoire.session_focus_path(path)


@mcp.tool()
def get_focused() -> GenericResponse:
    """Get currently focused entity with structured information"""
    return grimoire.session_get_focused()


@mcp.tool()
def unfocus() -> GenericResponse:
    """Clear focused entity"""
    return grimoire.session_unfocus()


@mcp.tool()
def do(skill_term: str, entity: Optional[str] = None) -> GenericResponse:
    """
    Invoke a skill on an entity (defaults to focused entity or 'system').

    Skills are operations derived from entity structure (e.g., nix packages become build skills).

    Args:
        skill_term: Skill term to invoke (e.g., "nix(build(hello))")
        entity: Optional entity name (defaults to focused entity or 'system')

    Example:
        do("nix(build(hello))", "my_project")
    """
    return grimoire.do(skill_term, entity)


@mcp.tool()
def skills(entity: Optional[str] = None) -> GenericResponse:
    """
    List all available skills for an entity (defaults to focused entity or 'system').

    Skills are operations derived from entity structure (e.g., nix packages, git remotes).

    Args:
        entity: Optional entity name (defaults to focused entity or 'system')

    Example:
        skills("my_project")
    """
    return grimoire.skills(entity)


@mcp.tool()
def session_status() -> GenericResponse:
    """Get session status including focused entity"""
    return grimoire.session_status()


@mcp.tool()
def get_session_context() -> SessionContextResponse:
    """Get comprehensive session context for LLM state recovery including focused entity components and activity summary"""
    return grimoire.session_context()


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
