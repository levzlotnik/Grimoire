:- self_entity(protocol_clients).

% Load child entities
:- load_entity(semantic(folder('@/src/protocol_clients/http'))).
:- load_entity(semantic(folder('@/src/protocol_clients/mcp'))).

% Declare child entities
component(protocol_clients, child_entity, protocol_client(http)).
component(protocol_clients, child_entity, protocol_client(mcp)).

% Expansion rules for protocol client properties
component(Entity, protocol_client_mcp_server, Server) :-
    component(Entity, has(protocol_client(mcp)), protocol_client(mcp(server(Server), _, _, _))).

component(Entity, protocol_client_http_service, Service) :-
    component(Entity, has(protocol_client(http)), protocol_client(http(service(Service), _, _, _))).

docstring(protocol_clients, "Level 3 domain for external HTTP/MCP clients").
