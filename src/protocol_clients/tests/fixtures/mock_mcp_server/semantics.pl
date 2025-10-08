% Mock MCP Server Test Fixture
:- self_entity(mock_mcp_server).

% Declare mock MCP server entity
component(mock_mcp_server, has(protocol_client(mcp)), protocol_client(mcp(
    server(mock_server),
    transport(stdio),
    command(["python", "-m", "mock_mcp_server"]),
    auto_reflect(true)
))).

% Mock expected tools from auto-reflection
component(mock_mcp_server, available_tools, [
    tool(test_tool, #{
        name => "test_tool",
        description => "A test tool",
        input_schema => #{type => "object"}
    })
]).

docstring(mock_mcp_server, "Mock MCP server fixture for testing").
