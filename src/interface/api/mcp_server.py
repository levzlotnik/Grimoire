"""
Grimoire MCP Server

Provides Model Context Protocol interface to Grimoire Prolog system.
Uses the same GrimoireInterface backend as the REST API.
"""

import argparse
from mcp.server import FastMCP
from typing import Dict, List, Any, Optional
from grimoire_interface import (
    GrimoireInterface,
    ComponentTypesResponse,
    ComponentsResponse,
    DocumentationResponse,
    StatusResponse,
    PerceiveResponse,
    ConjureResponse,
    SystemInfo,
    EntitiesResponse,
    TestResponse,
    SessionCommandResponse,
    LoadResponse,
)

# Create global instance of Grimoire interface
grimoire = GrimoireInterface()

# Get interface docstrings from Prolog
interface_docs = grimoire.query_interface_docstrings()

# Create FastMCP server with proper instructions
mcp = FastMCP("Grimoire Interface", instructions=grimoire.system_instructions())


@mcp.tool(description=interface_docs['compt'])
def compt(entity: str = "system") -> ComponentTypesResponse:
    return grimoire.compt(entity)


@mcp.tool(description=interface_docs['comp'])
def comp(entity: str, component_type: str) -> ComponentsResponse:
    return grimoire.comp(entity, component_type)


@mcp.tool(description=interface_docs['doc'])
def doc(entity: str = "system") -> DocumentationResponse:
    return grimoire.doc(entity)


@mcp.tool(description=interface_docs['status'])
def status() -> StatusResponse:
    return grimoire.status()


@mcp.tool(description=interface_docs['perceive'])
def perceive(query: str) -> PerceiveResponse:
    return grimoire.call_perceive_query(query)


@mcp.tool(description=interface_docs['conjure'])
def conjure(spell: str) -> ConjureResponse:
    return grimoire.call_conjure_spell(spell)


@mcp.tool()
def system_info() -> SystemInfo:
    """Get Grimoire system information including root directory, Prolog status, and version details."""
    result = grimoire.get_system_info()
    return result


@mcp.tool()
def health_check() -> Dict[str, Any]:
    """Test Prolog connection and system health. Returns diagnostic information about janus-swi and Prolog initialization."""
    result = grimoire.test_prolog_connection()
    return result


@mcp.tool(description=interface_docs['entities'])
def entities() -> EntitiesResponse:
    return grimoire.entities()


@mcp.tool(description=interface_docs['test'])
def test(args: Optional[List[str]] = None) -> TestResponse:
    return grimoire.test(args)


@mcp.tool(description=interface_docs['session'])
def session(args: List[str]) -> SessionCommandResponse:
    return grimoire.session(args)


@mcp.tool(description=interface_docs['load'])
def load(entity_spec: str) -> LoadResponse:
    return grimoire.load(entity_spec)


# Initialize dynamic docstrings with Prolog data
def _initialize_docstrings():
    """Set dynamic docstrings for MCP tools based on Prolog interface docstrings"""
    interface_docstrings = grimoire.query_interface_docstrings()

    compt.__doc__ = f"List component types. {interface_docstrings.get('compt', 'List all component types of current entity')} Returns structured component type data for MCP clients."
    comp.__doc__ = f"List components. {interface_docstrings.get('comp', 'List components of specific type for current entity')} Returns structured component data for MCP clients."
    doc.__doc__ = f"Show documentation. {interface_docstrings.get('doc', 'Show docstring of current entity')} Returns entity documentation for MCP clients."
    status.__doc__ = f"Show session status. {interface_docstrings.get('status', 'Show session/transaction status')} Returns session and git status information for MCP clients."
    perceive.__doc__ = f"Execute perception query. {interface_docstrings.get('perceive', 'Execute perception spells (query operations)')} Returns query results with variable bindings for MCP clients."
    conjure.__doc__ = f"Execute conjuration spell. {interface_docstrings.get('conjure', 'Execute conjuration spells (mutable operations)')} Returns spell execution results for MCP clients."


# Initialize docstrings at module load time
_initialize_docstrings()


def main():
    """Main entry point for grimoire-mcp-server script"""
    parser = argparse.ArgumentParser(description="Grimoire MCP Server")
    parser.add_argument(
        "--transport",
        default="stdio",
        choices=["stdio", "sse"],
    )
    args = parser.parse_args()
    mcp.run(transport=args.transport)


if __name__ == "__main__":
    main()
