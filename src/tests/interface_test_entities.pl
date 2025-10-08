% Test entities for interface domain tests
% This file contains declarative entity/component definitions for testing

:- self_entity(test_entity(interface)).

% Test client entities for different interface types
entity(test_cli_client).
entity(test_mcp_client).
entity(test_http_client).
entity(test_golem_client).

% CLI client with full capabilities
component(test_cli_client, has(interface(client)), interface(client([
    type(cli),
    capabilities([conjure, perceive, test, load, status])
]))).

% MCP client with restricted capabilities
component(test_mcp_client, has(interface(client)), interface(client([
    type(mcp),
    capabilities([conjure, perceive, session])
]))).

% HTTP client with query-only access
component(test_http_client, has(interface(client)), interface(client([
    type(http),
    capabilities([perceive])
]))).

% Golem client with spell delegation
component(test_golem_client, has(interface(client)), interface(client([
    type(golem),
    capabilities([conjure, perceive])
]))).

component(test_golem_client, has(interface(spell_access)), interface(spell_access([
    domain(git),
    spells([conjure(git(status)), conjure(git(commit(_)))])
]))).

% Docstrings for test entities
docstring(test_entity(interface), "Test entity container for interface domain tests").
docstring(test_cli_client, "CLI client test entity with full capabilities").
docstring(test_mcp_client, "MCP server client test entity with session support").
docstring(test_http_client, "HTTP client test entity with read-only access").
docstring(test_golem_client, "Golem client test entity with git spell access").
