"""MCP client implementation using FastMCP with auto-reflection"""
import asyncio
from typing import Dict, List, Any, Optional
from dataclasses import dataclass, field

try:
    from fastmcp import Client
    FASTMCP_AVAILABLE = True
except ImportError:
    FASTMCP_AVAILABLE = False


@dataclass
class MCPTool:
    """MCP tool definition"""
    name: str
    description: str
    input_schema: Dict[str, Any]


@dataclass
class MCPServer:
    """MCP server with client and tools"""
    name: str
    transport: str
    command: List[str]
    tools: List[MCPTool] = field(default_factory=list)
    _client: Optional[Any] = field(default=None, repr=False)
    _event_loop: Optional[asyncio.AbstractEventLoop] = field(default=None, repr=False)


class MCPServerRegistry:
    """Registry for MCP servers with FastMCP auto-reflection"""

    def __init__(self):
        self.servers: Dict[str, MCPServer] = {}

    def register_server(self, name: str, transport: str, command: List[str]) -> None:
        """Register a new MCP server and auto-reflect its tools

        Uses FastMCP client to connect via stdio and discover available tools.
        Falls back to mock implementation if FastMCP is not available.
        """
        if not FASTMCP_AVAILABLE:
            # Fallback: create mock server with empty tools
            self.servers[name] = MCPServer(
                name=name,
                transport=transport,
                command=command,
                tools=[]
            )
            return

        # Real FastMCP implementation
        if transport != "stdio":
            raise ValueError(f"Only stdio transport is currently supported, got: {transport}")

        # For stdio, command should be a list like ["python", "server.py"]
        # or just ["server.py"] if it's executable
        if len(command) == 0:
            raise ValueError("Command list cannot be empty")

        # Build the server path/command
        # If command is like ["python", "server.py"], we pass "server.py"
        # If command is like ["./server.py"], we pass "./server.py"
        if len(command) == 1:
            server_source = command[0]
        elif command[0] in ["python", "python3"]:
            server_source = command[1] if len(command) > 1 else None
            if not server_source:
                raise ValueError("Python command requires a script path")
        else:
            # For other commands, use the full command as-is
            server_source = " ".join(command)

        try:
            # Create FastMCP client - it will auto-detect stdio transport
            client = Client(server_source)

            # We need to run async operations in a sync context
            # Use asyncio.run to execute the tool discovery
            tools = asyncio.run(self._discover_tools(client))

            self.servers[name] = MCPServer(
                name=name,
                transport=transport,
                command=command,
                tools=tools,
                _client=client
            )
        except Exception as e:
            raise RuntimeError(f"Failed to register MCP server {name}: {e}")

    async def _discover_tools(self, client: Any) -> List[MCPTool]:
        """Discover tools from MCP server using FastMCP client"""
        tools = []
        try:
            async with client:
                # List available tools from the server
                tool_list = await client.list_tools()

                for tool_info in tool_list:
                    # FastMCP returns tool objects with name, description, and input_schema
                    tools.append(MCPTool(
                        name=tool_info.name if hasattr(tool_info, 'name') else str(tool_info),
                        description=tool_info.description if hasattr(tool_info, 'description') else "",
                        input_schema=tool_info.inputSchema if hasattr(tool_info, 'inputSchema') else {}
                    ))
        except Exception as e:
            # If discovery fails, return empty list but don't crash
            print(f"Warning: Failed to discover tools: {e}")

        return tools

    def call_tool(self, server_name: str, tool_name: str, args: Dict) -> Dict[str, Any]:
        """Call a tool on registered MCP server"""
        if server_name not in self.servers:
            raise ValueError(f"Server {server_name} not registered")

        server = self.servers[server_name]

        if not FASTMCP_AVAILABLE or server._client is None:
            # Fallback: mock response
            return {
                'content': f"Mock result from {tool_name}",
                'isError': False
            }

        # Real FastMCP tool call
        try:
            result = asyncio.run(self._call_tool_async(server._client, tool_name, args))
            return result
        except Exception as e:
            return {
                'content': str(e),
                'isError': True
            }

    async def _call_tool_async(self, client: Any, tool_name: str, args: Dict) -> Dict[str, Any]:
        """Call a tool asynchronously using FastMCP client"""
        async with client:
            result = await client.call_tool(tool_name, args)

            # Parse the result based on FastMCP's response format
            if hasattr(result, 'content'):
                content = result.content
            else:
                content = str(result)

            is_error = hasattr(result, 'isError') and result.isError

            return {
                'content': content,
                'isError': is_error
            }

    def list_tools(self, server_name: str) -> List[MCPTool]:
        """List all tools from a server"""
        if server_name not in self.servers:
            raise ValueError(f"Server {server_name} not registered")

        return self.servers[server_name].tools

    def list_servers(self) -> List[MCPServer]:
        """List all registered servers"""
        return list(self.servers.values())

    def ensure_registered(self, name: str, transport: str, command: List[str]) -> None:
        """Ensure server is registered (for verification)"""
        if name not in self.servers:
            self.register_server(name, transport, command)


# Global registry instance
_registry = MCPServerRegistry()


# Module-level functions for janus interface
def register_server(name: str, transport: str, command: List[str]) -> None:
    _registry.register_server(name, transport, command)


def call_tool(server: str, tool: str, args: Dict) -> Dict[str, Any]:
    return _registry.call_tool(server, tool, args)


def list_tools(server: str) -> List[MCPTool]:
    return _registry.list_tools(server)


def list_servers() -> List[MCPServer]:
    return _registry.list_servers()


def ensure_registered(name: str, transport: str, command: List[str]) -> None:
    _registry.ensure_registered(name, transport, command)
