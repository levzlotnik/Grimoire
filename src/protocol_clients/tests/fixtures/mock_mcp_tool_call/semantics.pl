% Mock MCP Tool Call Test Fixture
:- self_entity(mock_mcp_tool_call).

% Declare expected MCP tool call result
component(mock_mcp_tool_call, expected_tool_result, mcp_result(
    "Mock tool result content",
    false  % isError = false
)).

component(mock_mcp_tool_call, test_server, mock_call_server).
component(mock_mcp_tool_call, test_tool, mock_tool).
component(mock_mcp_tool_call, test_args, #{input => "test_input"}).

docstring(mock_mcp_tool_call, "Mock MCP tool call result fixture for testing").
