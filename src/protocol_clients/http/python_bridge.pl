% HTTP Python Bridge - ALL py_call + decoding happens here
% Keeps semantics.pl pure Prolog

:- module(http_python_bridge, [
    http_call_endpoint/5,
    http_register_service/3,
    http_list_services/1,
    ensure_http_service_registered/3
]).

:- use_module(library(janus)).

% Ensure Python HTTP client module is imported
:- dynamic http_client_imported/0.

ensure_python_http_client :-
    (   http_client_imported
    ->  true
    ;   grimoire_resolve_path('./', HttpPath),
        py_add_lib_dir(HttpPath, first),
        py_import(client, [as(http_client)]),
        assertz(http_client_imported)
    ).

% Register HTTP service (writes to Python registry)
http_register_service(Service, BaseUrl, Endpoints) :-
    ensure_python_http_client,
    % Convert Prolog endpoints to Python format
    encode_endpoints(Endpoints, PyEndpoints),
    py_call(http_client:register_service(Service, BaseUrl, PyEndpoints)).

% Call HTTP endpoint via Python httpx
http_call_endpoint(Service, Endpoint, Params, Status, DecodedResponse) :-
    ensure_python_http_client,
    % Encode params to Python dict
    encode_params(Params, PyParams),
    % Call Python HTTP client
    py_call(http_client:call_endpoint(Service, Endpoint, PyParams), PyResponse),
    % Decode response to Prolog terms
    decode_http_response(PyResponse, Status, DecodedResponse).

% List all registered services
http_list_services(Services) :-
    ensure_python_http_client,
    py_call(http_client:list_services(), PyServices),
    decode_service_list(PyServices, Services).

% Ensure service is registered (for verification)
ensure_http_service_registered(Service, BaseUrl, Endpoints) :-
    ensure_python_http_client,
    encode_endpoints(Endpoints, PyEndpoints),
    py_call(http_client:ensure_registered(Service, BaseUrl, PyEndpoints)).

% === DECODING PREDICATES ===

% Decode HTTP response from Python to Prolog
decode_http_response(PyResponse, Status, DecodedBody) :-
    py_call(PyResponse:'status_code', Status),
    % Try to parse JSON response
    catch(
        (
            py_call(PyResponse:json(), PyJson),
            decode_json(PyJson, DecodedBody)
        ),
        _,
        % If JSON parsing fails, get text
        (
            py_call(PyResponse:text, DecodedBody)
        )
    ).

% Decode JSON (recursive for nested structures)
decode_json(PyValue, PrologValue) :-
    (   py_is_dict(PyValue)
    ->  py_iter(PyValue, Items, items),
        maplist(decode_dict_pair, Items, PrologPairs),
        dict_create(PrologValue, _, PrologPairs)
    ;   py_is_list(PyValue)
    ->  py_iter(PyValue, PyList),
        maplist(decode_json, PyList, PrologValue)
    ;   % Primitive types pass through
        PrologValue = PyValue
    ).

decode_dict_pair(Key-PyValue, Key-PrologValue) :-
    decode_json(PyValue, PrologValue).

% === ENCODING PREDICATES ===

% Encode Prolog params to Python dict (janus converts dicts automatically)
encode_params(PrologDict, PrologDict) :-
    is_dict(PrologDict).

% Encode endpoints - convert to list of dicts for Python
encode_endpoints(Endpoints, PyEndpoints) :-
    maplist(encode_endpoint, Endpoints, PyEndpoints).

encode_endpoint(endpoint(Name, Path, Method), #{name: Name, path: Path, method: Method}).

% Decode service list
decode_service_list(PyServices, Services) :-
    py_iter(PyServices, PyList),
    maplist(decode_service, PyList, Services).

decode_service(PyService, service(Name, BaseUrl, Endpoints)) :-
    py_call(PyService:name, Name),
    py_call(PyService:base_url, BaseUrl),
    py_call(PyService:endpoints, PyEndpoints),
    py_iter(PyEndpoints, PyEndpointList),
    maplist(decode_endpoint, PyEndpointList, Endpoints).

decode_endpoint(PyEndpoint, endpoint(Name, Path, Method)) :-
    py_call(PyEndpoint:name, Name),
    py_call(PyEndpoint:path, Path),
    py_call(PyEndpoint:method, Method).

% Ensure Python setup happens when this module is loaded
:- ensure_python_http_client.
