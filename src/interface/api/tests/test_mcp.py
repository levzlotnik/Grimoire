"""
MCP server tests for grimoire-mcp

Tests MCP server using FastMCP Client (in-memory, no subprocess).
"""
import pytest
from fastmcp import Client
from grimoire.mcp import mcp


@pytest.fixture
async def mcp_client():
    """Create in-memory MCP client"""
    async with Client(mcp) as client:
        yield client


class TestMCPServer:
    """Test suite for Grimoire MCP Server"""

    @pytest.mark.asyncio
    async def test_component_types_tool(self, mcp_client):
        """Test component_types tool"""
        result = await mcp_client.call_tool(
            "component_types",
            {"entity": "system"}
        )
        assert result is not None
        # CallToolResult has a content attribute with the actual text
        content = result.content[0].text if hasattr(result, 'content') else str(result)
        assert "types:" in content

    @pytest.mark.asyncio
    async def test_components_tool(self, mcp_client):
        """Test components tool"""
        result = await mcp_client.call_tool(
            "components",
            {"entity": "system", "component_type": "self"}
        )
        assert result is not None

    @pytest.mark.asyncio
    async def test_docstring_tool(self, mcp_client):
        """Test docstring tool"""
        result = await mcp_client.call_tool(
            "docstring",
            {"entity": "system"}
        )
        assert result is not None
        content = result.content[0].text if hasattr(result, 'content') else str(result)
        assert len(content) > 0

    @pytest.mark.asyncio
    async def test_entities_tool(self, mcp_client):
        """Test entities tool"""
        result = await mcp_client.call_tool("entities", {})
        assert result is not None

    @pytest.mark.asyncio
    async def test_list_spells_tool(self, mcp_client):
        """Test list_spells tool"""
        result = await mcp_client.call_tool("list_spells", {})
        assert result is not None
        content = result.content[0].text if hasattr(result, 'content') else str(result)
        assert "spells:" in content

    @pytest.mark.asyncio
    async def test_perceive_tool(self, mcp_client):
        """Test perceive tool"""
        result = await mcp_client.call_tool(
            "perceive",
            {"query_sig": "interface(entities)", "args": {}}
        )
        assert result is not None
