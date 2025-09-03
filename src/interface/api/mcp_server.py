"""
Grimoire MCP Server

Provides Model Context Protocol interface to Grimoire Prolog system.
Uses the same GrimoireInterface backend as the REST API.
"""

import argparse
import yaml
from mcp.server import FastMCP
from typing import Dict, List, Any, Optional

from pydantic import BaseModel
from grimoire_interface import GrimoireInterface

# Create global instance of Grimoire interface
grimoire = GrimoireInterface()

# Create FastMCP server with proper instructions
mcp = FastMCP("Grimoire Interface", instructions=grimoire.system_instructions())

# Get interface docstrings from Prolog (defer until after initialization)
interface_docs = grimoire.query_interface_docstrings()


def _model_to_string(model: Any) -> str:
    """Convert a model to YAML string representation for better nested structure readability"""
    if not isinstance(model, BaseModel):
        return str(model)
    # Convert Pydantic model to dict and then to YAML
    model_dict = model.model_dump()
    return yaml.dump(model_dict, default_flow_style=False, sort_keys=False)


@mcp.tool(description=interface_docs["compt"])
def compt(entity: str = "system"):
    return _model_to_string(grimoire.compt(entity))


@mcp.tool(description=interface_docs["comp"])
def comp(entity: str, component_type: str):
    return _model_to_string(grimoire.comp(entity, component_type))


@mcp.tool(description=interface_docs["doc"])
def doc(entity: str = "system"):
    return _model_to_string(grimoire.doc(entity))


@mcp.tool(description=interface_docs["status"])
def status():
    return _model_to_string(grimoire.status())


@mcp.tool(description=interface_docs["perceive"])
def perceive(query: str):
    return _model_to_string(grimoire.call_perceive_query(query))


@mcp.tool(description=interface_docs["conjure"])
def conjure(spell: str):
    return _model_to_string(grimoire.call_conjure_spell(spell))


@mcp.tool()
def system_info():
    """Get Grimoire system information including root directory, Prolog status, and version details."""
    return _model_to_string(grimoire.get_system_info())


@mcp.tool(description=interface_docs["entities"])
def entities():
    return _model_to_string(grimoire.entities())


@mcp.tool(description=interface_docs["test"])
def test(args: Optional[List[str]] = None):
    return _model_to_string(grimoire.test(args))


@mcp.tool(description=interface_docs["session"])
def session(args: List[str]):
    return _model_to_string(grimoire.session(args))


@mcp.tool(description=interface_docs["load"])
def load(entity_spec: str):
    return _model_to_string(grimoire.load(entity_spec))


@mcp.tool(description=interface_docs["read_file"])
def read_file(file_path: str, start: int, end: int):
    """Read lines from a file using 1-based indexing."""
    result = grimoire.read_file(file_path, start, end)
    
    # Format as simple string with line numbers
    lines = []
    for line_content in result.lines:
        lines.append(f"{line_content.line_number}  {line_content.content}")
    
    return "\n".join(lines)


@mcp.tool(description=interface_docs["edit_file"])
def edit_file(file_path: str, edits: List[Dict[str, Any]]):
    """Edit file using Grimoire's edit_file conjure operation. Edits should be list of edit operations."""
    from grimoire_interface import EditInsert, EditDelete, EditReplace, EditAppend
    
    # Convert dicts to typed models
    typed_edits = []
    for edit in edits:
        op = edit.get("operation", "")
        if op == "insert":
            typed_edits.append(EditInsert(**edit))
        elif op == "delete":
            typed_edits.append(EditDelete(**edit))
        elif op == "replace":
            typed_edits.append(EditReplace(**edit))
        elif op == "append":
            typed_edits.append(EditAppend(**edit))
    
    return _model_to_string(grimoire.edit_file(file_path, typed_edits))


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
