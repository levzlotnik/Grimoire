:- use_module(library(plunit)).

% Load test entities - DISABLED during protocol_clients rework
% :- load_entity(semantic(file('@/src/tests/protocol_clients_test_entities.pl'))).

% Load subdomain test files
:- grimoire_ensure_loaded('@/src/protocol_clients/http/semantics.plt').
:- grimoire_ensure_loaded('@/src/protocol_clients/mcp/semantics.plt').

% Verification rules
verify(component(Entity, has(protocol_client(mcp)), protocol_client(mcp(server(S), transport(T), command(C), auto_reflect(A))))) :-
    user:please_verify(component(Entity, protocol_client_mcp_server, S)),
    atom(S), atom(T), is_list(C).

% Top-level protocol_clients tests
:- begin_tests(protocol_clients).

test(entity_exists) :-
    entity(protocol_clients).

test(has_docstring) :-
    docstring(protocol_clients, _).

test(http_child_exists) :-
    user:please_verify(component(protocol_clients, child_entity, protocol_client(http))).

test(mcp_child_exists) :-
    user:please_verify(component(protocol_clients, child_entity, protocol_client(mcp))).

test(mcp_expansion) :-
    user:please_verify(component(test_mcp_client, protocol_client_mcp_server, file_operations)).

:- end_tests(protocol_clients).
