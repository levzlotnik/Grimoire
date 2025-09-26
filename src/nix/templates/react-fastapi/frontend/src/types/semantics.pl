% Frontend types directory semantics
% TypeScript type definitions

:- self_entity(react_fastapi_template(frontend(src(types)))).

% Types directory docstring
docstring(react_fastapi_template(frontend(src(types))), "TypeScript type definitions for API responses and application data structures").

% Type files as children
component(react_fastapi_template(frontend(src(types))), child, react_fastapi_template(frontend(src(types(source(file('./api.ts'))))))).

% Type file entities with docstrings
entity(react_fastapi_template(frontend(src(types(source(file('./api.ts'))))))).
docstring(react_fastapi_template(frontend(src(types(source(file('./api.ts')))))), "API response type definitions for dashboard data, endpoints, and documentation").

% Type categories
component(react_fastapi_template(frontend(src(types))), category, dashboard_types).
component(react_fastapi_template(frontend(src(types))), category, api_types).
component(react_fastapi_template(frontend(src(types))), category, documentation_types).