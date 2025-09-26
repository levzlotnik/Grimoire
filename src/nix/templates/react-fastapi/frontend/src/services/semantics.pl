% Frontend services directory semantics
% API client and service layer components

:- self_entity(react_fastapi_template(frontend(src(services)))).

% Services directory docstring
docstring(react_fastapi_template(frontend(src(services))), "Service layer components including API client for backend communication").

% Service files as children
component(react_fastapi_template(frontend(src(services))), child, react_fastapi_template(frontend(src(services(source(file('./apiClient.ts'))))))).

% Service file entities with docstrings
entity(react_fastapi_template(frontend(src(services(source(file('./apiClient.ts'))))))).
docstring(react_fastapi_template(frontend(src(services(source(file('./apiClient.ts')))))), "Type-safe API client with error handling for dashboard endpoints and authentication support").

% Service capabilities
component(react_fastapi_template(frontend(src(services))), capability, dashboard_api).
component(react_fastapi_template(frontend(src(services))), capability, error_handling).
component(react_fastapi_template(frontend(src(services))), capability, type_safety).
component(react_fastapi_template(frontend(src(services))), capability, auth_support).