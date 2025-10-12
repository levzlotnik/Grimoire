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
from grimoire_client import GrimoireClient

# Create global instance of Grimoire interface
grimoire = GrimoireClient()

# Create FastMCP server with proper instructions
mcp = FastMCP("Grimoire Interface", instructions=grimoire.system_instructions())

# Get interface docstrings from Prolog - these define which tools are available
# Only expose tools for subcommands that are actually registered in Prolog
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


@mcp.tool(description=interface_docs["read_file"])
def read_file(file_path: str, start: int = 1, end: int = 10):
    """Read lines from a file using 1-based indexing."""
    result = grimoire.read_file(file_path, start, end)
    
    # Format as simple string with line numbers
    lines = []
    for line_content in result.lines:
        lines.append(f"{line_content.line_number}  {line_content.content}")
    
    return "\n".join(lines)


@mcp.tool(description=interface_docs["edit_file"])
def edit_file(file_path: str, edits: List[Dict[str, Any]]):
    """
    Edit file using Grimoire's edit_file conjure operation.

    Edits should be a list of edit operations. Each operation must have an 'operation' field
    with one of: 'insert', 'delete', 'replace', 'append'.

    Note: The REPL interface command is intentionally not exposed as an MCP tool because
    REPL requires interactive stdin/stdout which is incompatible with the MCP server's
    async request/response model.
    """
    from grimoire_client import EditInsert, EditDelete, EditReplace, EditAppend

    # Validate and convert dicts to typed models
    typed_edits = []
    valid_operations = {"insert", "delete", "replace", "append"}

    for i, edit in enumerate(edits):
        op = edit.get("operation", "")

        # Validate operation type
        if not op:
            raise ValueError(f"Edit at index {i} is missing 'operation' field")
        if op not in valid_operations:
            raise ValueError(
                f"Edit at index {i} has invalid operation '{op}'. "
                f"Must be one of: {', '.join(valid_operations)}"
            )

        # Convert to appropriate typed model with Pydantic validation
        try:
            if op == "insert":
                typed_edits.append(EditInsert(**edit))
            elif op == "delete":
                typed_edits.append(EditDelete(**edit))
            elif op == "replace":
                typed_edits.append(EditReplace(**edit))
            elif op == "append":
                typed_edits.append(EditAppend(**edit))
        except Exception as e:
            raise ValueError(f"Invalid edit at index {i}: {e}") from e

    # Ensure at least one valid edit was provided
    if not typed_edits:
        raise ValueError("No valid edits provided")

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
