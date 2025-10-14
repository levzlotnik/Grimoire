"""MCP client implementation using FastMCP"""
import asyncio
from typing import Dict, List, Any, Optional
from pydantic import BaseModel, Field

from fastmcp import Client


class MCPTool(BaseModel):
    """MCP tool definition"""
    name: str
    description: str
    input_schema: Dict[str, Any]


class MCPServer(BaseModel):
    """MCP server connection info"""
    name: str
    transport: str  # "stdio" or "http"
    command: Optional[List[str]] = None  # For stdio transport
    url: Optional[str] = None  # For http transport
    tools: List[MCPTool] = Field(default_factory=list)


class MCPServerRegistry:
    """Registry for MCP servers using FastMCP"""

    def __init__(self):
        self.servers: Dict[str, MCPServer] = {}

    def register_server(self, name: str, transport: str, command: Optional[List[str]] = None,
                       url: Optional[str] = None) -> None:
        """Register a new MCP server and discover its tools

        Args:
            name: Server identifier
            transport: Either "stdio" or "http"
            command: Command list for stdio transport (e.g., ["python", "server.py"])
            url: URL for http transport (e.g., "http://localhost:8000/mcp")
        """
        if transport == "stdio":
            if not command or len(command) == 0:
                raise ValueError("stdio transport requires a command list")
            tools = asyncio.run(self._register_server(name, command=command))
            self.servers[name] = MCPServer(
                name=name,
                transport=transport,
                command=command,
                tools=tools
            )
        elif transport in ("http", "streamablehttp"):
            if not url:
                raise ValueError("http transport requires a URL")
            tools = asyncio.run(self._register_server(name, url=url))
            self.servers[name] = MCPServer(
                name=name,
                transport="http",
                url=url,
                tools=tools
            )
        else:
            raise ValueError(f"Unsupported transport: {transport}. Use 'stdio' or 'http'")

    async def _register_server(self, name: str, command: Optional[List[str]] = None,
                              url: Optional[str] = None) -> List[MCPTool]:
        """Register MCP server and discover tools using FastMCP"""
        try:
            # FastMCP Client can accept either a script path or URL
            if command:
                # For stdio: FastMCP expects just the script path, not "python script.py"
                # Extract the script path (last element of command list)
                client_source = command[-1]
            elif url:
                # For HTTP: use URL directly
                client_source = url
            else:
                raise ValueError("Either command or url must be provided")

            client = Client(client_source)

            async with client:
                tools = []
                tool_list = await client.list_tools()

                for tool in tool_list:
                    tools.append(MCPTool(
                        name=tool.name,
                        description=tool.description if hasattr(tool, 'description') else "",
                        input_schema=tool.inputSchema if hasattr(tool, 'inputSchema') else {}
                    ))

                return tools
        except Exception as e:
            raise RuntimeError(f"Failed to register MCP server {name}: {e}")

    def call_tool(self, server_name: str, tool_name: str, args: Dict) -> Dict[str, Any]:
        """Call a tool on registered MCP server"""
        if server_name not in self.servers:
            raise ValueError(f"Server {server_name} not registered")

        server = self.servers[server_name]

        try:
            result = asyncio.run(self._call_tool_async(server, tool_name, args))
            return result
        except Exception as e:
            return {
                'content': [{'type': 'text', 'text': str(e)}],
                'isError': True
            }

    async def _call_tool_async(self, server: MCPServer, tool_name: str, args: Dict) -> Dict[str, Any]:
        """Call a tool asynchronously using FastMCP"""
        try:
            # Get client source based on transport
            if server.transport == "stdio":
                # FastMCP expects just the script path
                client_source = server.command[-1]
            elif server.transport == "http":
                client_source = server.url
            else:
                raise ValueError(f"Unsupported transport: {server.transport}")

            client = Client(client_source)

            async with client:
                result = await client.call_tool(tool_name, args)

                # FastMCP returns result with content - convert to our format
                content_list = []
                if hasattr(result, 'content'):
                    for item in result.content:
                        if hasattr(item, 'text'):
                            content_list.append({'type': 'text', 'text': item.text})
                        else:
                            content_list.append({'type': 'text', 'text': str(item)})
                else:
                    content_list.append({'type': 'text', 'text': str(result)})

                return {
                    'content': content_list,
                    'isError': result.isError if hasattr(result, 'isError') else False
                }
        except Exception as e:
            return {
                'content': [{'type': 'text', 'text': str(e)}],
                'isError': True
            }

    def list_tools(self, server_name: str) -> List[Dict[str, Any]]:
        """List all tools from a server"""
        if server_name not in self.servers:
            raise ValueError(f"Server {server_name} not registered")

        return [t.model_dump() for t in self.servers[server_name].tools]

    def list_servers(self) -> List[Dict[str, Any]]:
        """List all registered servers"""
        return [s.model_dump() for s in self.servers.values()]

    def ensure_registered(self, name: str, transport: str, command: Optional[List[str]] = None,
                         url: Optional[str] = None) -> None:
        """Ensure server is registered (for verification)"""
        if name not in self.servers:
            self.register_server(name, transport, command, url)


# Global registry instance
_registry = MCPServerRegistry()


# Module-level functions for janus interface
def register_server(name: str, transport: str, command: Optional[List[str]] = None,
                   url: Optional[str] = None) -> None:
    _registry.register_server(name, transport, command, url)


def call_tool(server: str, tool: str, args: Dict) -> Dict[str, Any]:
    return _registry.call_tool(server, tool, args)


def list_tools(server: str) -> List[Dict[str, Any]]:
    return _registry.list_tools(server)


def list_servers() -> List[Dict[str, Any]]:
    return _registry.list_servers()


def ensure_registered(name: str, transport: str, command: Optional[List[str]] = None,
                     url: Optional[str] = None) -> None:
    _registry.ensure_registered(name, transport, command, url)
