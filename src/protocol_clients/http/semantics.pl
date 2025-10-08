% HTTP Client Logic - Pure Prolog (NO py_call)
% All Python interaction happens in python_bridge.pl

:- self_entity(protocol_client(http)).

:- use_module('python_bridge.pl', [
    http_call_endpoint/5,
    http_register_service/3,
    http_list_services/1
]).

% === DOCSTRINGS ===

docstring(protocol_client(http), "HTTP client for consuming external HTTP APIs with user-declared endpoint schemas").

% === SPELL IMPLEMENTATIONS ===

% HTTP service registration spell
register_spell(
    conjure(protocol_client(http(register))),
    input(protocol_client(http(register(
        service('Service'),
        base_url('BaseUrl'),
        endpoints('Endpoints')
    )))),
    output(either(ok(service_registered('Service')), error(http_registration_error('Reason')))),
    docstring("Register an HTTP service with base URL and endpoint schemas for later invocation")
).

cast(conjure(protocol_client(http(register(
    service(Service),
    base_url(BaseUrl),
    endpoints(Endpoints)
)))), Result) :-
    catch(
        (
            http_register_service(Service, BaseUrl, Endpoints),
            % Write to config file for persistence
            write_http_service_config(Service, BaseUrl, Endpoints),
            Result = ok(service_registered(Service))
        ),
        Error,
        Result = error(http_registration_error(Error))
    ).

% HTTP endpoint call spell
register_spell(
    conjure(protocol_client(http(call))),
    input(protocol_client(http(call(
        service('Service'),
        endpoint('Endpoint'),
        params('Params')
    )))),
    output(either(ok(response(status('Status'), body('Body'), service('Service'))), error(http_call_error('Reason')))),
    docstring("Call a registered HTTP service endpoint with given parameters")
).

cast(conjure(protocol_client(http(call(
    service(Service),
    endpoint(Endpoint),
    params(Params)
)))), Result) :-
    catch(
        (
            http_call_endpoint(Service, Endpoint, Params, Status, DecodedResponse),
            Result = ok(response(status(Status), body(DecodedResponse), service(Service)))
        ),
        Error,
        Result = error(http_call_error(Error))
    ).

% List services spell
register_spell(
    perceive(protocol_client(http(list_services))),
    input(protocol_client(http(list_services))),
    output(either(ok(services('Services')), error(http_list_error('Reason')))),
    docstring("List all registered HTTP services")
).

cast(perceive(protocol_client(http(list_services))), Result) :-
    catch(
        (
            http_list_services(Services),
            Result = ok(services(Services))
        ),
        Error,
        Result = error(http_list_error(Error))
    ).

% === HELPER PREDICATES ===

% Write HTTP service config to file for persistence
write_http_service_config(Service, BaseUrl, Endpoints) :-
    getenv('GRIMOIRE_DATA', GrimoireData),
    format(atom(ConfigDir), '~w/protocol_clients/http', [GrimoireData]),
    % Ensure directory exists
    (   exists_directory(ConfigDir)
    ->  true
    ;   make_directory_path(ConfigDir)
    ),
    format(atom(ConfigPath), '~w/~w.json', [ConfigDir, Service]),
    % Write service config as JSON
    open(ConfigPath, write, Stream),
    format(Stream, '{', []),
    format(Stream, '  "service": "~w",~n', [Service]),
    format(Stream, '  "base_url": "~w",~n', [BaseUrl]),
    format(Stream, '  "endpoints": [', []),
    write_endpoints_json(Stream, Endpoints),
    format(Stream, '  ]~n', []),
    format(Stream, '}~n', []),
    close(Stream).

write_endpoints_json(_, []).
write_endpoints_json(Stream, [endpoint(Name, Path, Method)]) :-
    format(Stream, '~n    {"name": "~w", "path": "~w", "method": "~w"}~n', [Name, Path, Method]).
write_endpoints_json(Stream, [endpoint(Name, Path, Method)|Rest]) :-
    Rest \= [],
    format(Stream, '~n    {"name": "~w", "path": "~w", "method": "~w"},', [Name, Path, Method]),
    write_endpoints_json(Stream, Rest).
