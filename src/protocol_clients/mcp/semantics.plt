% PLUnit tests for MCP client subdomain
:- use_module(library(plunit)).

:- begin_tests(protocol_client_mcp).

% Test MCP server registration
test(mcp_server_registration, [
    setup(setup_mcp_test),
    cleanup(cleanup_mcp_test),
    condition(python_available)
]) :-
    grimoire_resolve_path('@/src/protocol_clients/mcp/test_mcp_server.py', TestServerPath),
    user:magic_cast(conjure(protocol_client(mcp(register(
        server(test_server),
        transport(stdio),
        command(["python", TestServerPath])
    )))), Result),
    assertion(Result = ok(server_registered(test_server))).

% Test MCP list servers
test(mcp_list_servers, [
    setup(setup_mcp_test),
    cleanup(cleanup_mcp_test),
    condition(python_available)
]) :-
    % Register a test server first
    grimoire_resolve_path('@/src/protocol_clients/mcp/test_mcp_server.py', TestServerPath),
    user:magic_cast(conjure(protocol_client(mcp(register(
        server(test_list_server),
        transport(stdio),
        command(["python", TestServerPath])
    )))), RegResult),
    assertion(RegResult = ok(server_registered(test_list_server))),
    % List servers
    user:magic_cast(perceive(protocol_client(mcp(list_servers))), Result),
    assertion(Result = ok(servers(_Servers))).

% Test MCP list tools
test(mcp_list_tools, [
    setup(setup_mcp_test),
    cleanup(cleanup_mcp_test),
    condition(python_available)
]) :-
    % Register a test server first
    grimoire_resolve_path('@/src/protocol_clients/mcp/test_mcp_server.py', TestServerPath),
    user:magic_cast(conjure(protocol_client(mcp(register(
        server(test_tools_server),
        transport(stdio),
        command(["python", TestServerPath])
    )))), RegResult),
    assertion(RegResult = ok(server_registered(test_tools_server))),
    % List tools (should have mock_tool)
    user:magic_cast(perceive(protocol_client(mcp(list_tools(server(test_tools_server))))), Result),
    assertion(Result = ok(tools(_Tools))).

% Test MCP call tool
test(mcp_call_tool, [
    setup(setup_mcp_test),
    cleanup(cleanup_mcp_test),
    condition(python_available)
]) :-
    % Register a test server first
    grimoire_resolve_path('@/src/protocol_clients/mcp/test_mcp_server.py', TestServerPath),
    user:magic_cast(conjure(protocol_client(mcp(register(
        server(test_call_server),
        transport(stdio),
        command(["python", TestServerPath])
    )))), RegResult),
    assertion(RegResult = ok(server_registered(test_call_server))),
    % Call a mock tool
    user:magic_cast(conjure(protocol_client(mcp(call(
        server(test_call_server),
        tool(mock_tool),
        args(py{input: "test"})
    )))), Result),
    assertion(Result = ok(tool_result(content(_Content), server(test_call_server)))).

:- end_tests(protocol_client_mcp).

% === SETUP/CLEANUP ===

setup_mcp_test :-
    % Create test directory and set GRIMOIRE_DATA for this test
    make_directory_path('/tmp/test_mcp'),
    setenv('GRIMOIRE_DATA', '/tmp/test_mcp').

cleanup_mcp_test :-
    % Clean up test directory
    (   exists_directory('/tmp/test_mcp')
    ->  delete_directory_and_contents('/tmp/test_mcp')
    ;   true
    ).

% Check if Python and FastMCP are available
python_available :-
    catch(
        (py_call(sys:version, _),
         py_import(fastmcp, [])),
        _,
        fail
    ).
