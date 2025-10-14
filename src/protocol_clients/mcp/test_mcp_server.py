"""Simple test MCP server for unit tests using FastMCP"""
from fastmcp import FastMCP

# Create FastMCP server
mcp = FastMCP("test-server")


@mcp.tool()
def mock_tool(input: str) -> str:
    """A mock tool for testing

    Args:
        input: Test input string

    Returns:
        Mock response string
    """
    return f"Mock response: {input}"


if __name__ == "__main__":
    # FastMCP handles the stdio transport automatically
    mcp.run()
