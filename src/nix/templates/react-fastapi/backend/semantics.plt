% PLUnit tests for Backend semantics

:- begin_tests(backend_semantics).

% Load semantic file
:- ensure_loaded(semantics).

% Test entity declaration
test(backend_entity_declared) :-
    entity(react_fastapi_template(backend)).

% Test docstring
test(backend_has_docstring) :-
    docstring(react_fastapi_template(backend), _).

% Test source file entities
test(backend_source_files_exist) :-
    entity(react_fastapi_template(backend(source(file('./main.py'))))),
    entity(react_fastapi_template(backend(source(file('./pyproject.toml'))))),
    entity(react_fastapi_template(backend(source(file('./Dockerfile'))))).

% Test source file docstrings
test(source_files_have_docstrings) :-
    docstring(react_fastapi_template(backend(source(file('./main.py')))), _),
    docstring(react_fastapi_template(backend(source(file('./pyproject.toml')))), _),
    docstring(react_fastapi_template(backend(source(file('./Dockerfile')))), _).

% Test child relationships
test(backend_children_defined) :-
    component(react_fastapi_template(backend), child, react_fastapi_template(backend(source(file('./main.py'))))),
    component(react_fastapi_template(backend), child, react_fastapi_template(backend(source(file('./pyproject.toml'))))),
    component(react_fastapi_template(backend), child, react_fastapi_template(backend(source(file('./Dockerfile'))))).

% Test technology stack
test(technology_stack) :-
    component(react_fastapi_template(backend), web_framework, fastapi),
    component(react_fastapi_template(backend), orm, sqlalchemy),
    component(react_fastapi_template(backend), database, sqlite),
    component(react_fastapi_template(backend), validation, pydantic).

% Test endpoint categories
test(endpoint_categories) :-
    component(react_fastapi_template(backend), endpoint_category, authentication),
    component(react_fastapi_template(backend), endpoint_category, dashboard),
    component(react_fastapi_template(backend), endpoint_category, health_check).

% Test dashboard endpoints (used by frontend)
test(dashboard_endpoints) :-
    component(react_fastapi_template(backend), api_endpoint, endpoint(get, '/api/dashboard/stats', _)),
    component(react_fastapi_template(backend), api_endpoint, endpoint(get, '/api/dashboard/monthly-data', _)),
    component(react_fastapi_template(backend), api_endpoint, endpoint(get, '/api/dashboard/device-distribution', _)),
    component(react_fastapi_template(backend), api_endpoint, endpoint(get, '/api/dashboard/hourly-activity', _)),
    component(react_fastapi_template(backend), api_endpoint, endpoint(get, '/api/dashboard/transactions', _)).

% Test system endpoints
test(system_endpoints) :-
    component(react_fastapi_template(backend), api_endpoint, endpoint(get, '/api/health', _)),
    component(react_fastapi_template(backend), api_endpoint, endpoint(get, '/api/docs/openapi', _)),
    component(react_fastapi_template(backend), api_endpoint, endpoint(websocket, '/ws', _)).

% Test database models
test(database_models) :-
    component(react_fastapi_template(backend), database_model, user),
    component(react_fastapi_template(backend), database_model, task).

% Test configuration
test(configuration) :-
    component(react_fastapi_template(backend), cors_origins, _),
    component(react_fastapi_template(backend), database_file, tempfile),
    component(react_fastapi_template(backend), auto_docs, true).

% Test actual files exist
test(actual_backend_files_exist) :-
    exists_file('main.py'),
    exists_file('pyproject.toml'),
    exists_file('Dockerfile').

:- end_tests(backend_semantics).