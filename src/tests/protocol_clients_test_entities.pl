% Test entities for protocol_clients domain tests
% This file contains declarative entity/component definitions for testing

:- self_entity(test_entity(protocol_clients)).

% === TEST ENTITIES ===

entity(test_mcp_client).
entity(test_http_client).
entity(mock_protocol_client).

% === MCP CLIENT COMPONENT (DSL PATTERN) ===

% MCP client component with DSL pattern
component(test_mcp_client, has(protocol_client(mcp)), protocol_client(mcp(
    server(file_operations),
    transport(stdio),
    command(["python", "-m", "test_mcp_server"]),
    auto_reflect(true)
))).

% === HTTP CLIENT COMPONENT (DSL PATTERN) ===

% HTTP client component with DSL pattern
component(test_http_client, has(protocol_client(http)), protocol_client(http(
    service(github_api),
    base_url("https://api.github.com"),
    auth(bearer_token("${GITHUB_TOKEN}")),
    endpoints([
        endpoint(get_repo, "/repos/:owner/:repo", get),
        endpoint(create_issue, "/repos/:owner/:repo/issues", post)
    ])
))).

% === MOCK CLIENT FOR TESTING ===

component(mock_protocol_client, protocol_client_test_path, '/tmp/test_protocol_clients').

% === DOCSTRINGS ===

docstring(test_entity(protocol_clients), "Test entity container for protocol_clients domain verification tests").
docstring(test_mcp_client, "Test MCP client entity with file operations server").
docstring(test_http_client, "Test HTTP client entity with GitHub API").
docstring(mock_protocol_client, "Mock protocol client for testing").
