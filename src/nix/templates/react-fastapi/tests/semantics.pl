% Tests directory semantics for React FastAPI Template
% Backend test files using pytest

:- self_entity(react_fastapi_template(tests)).

% Tests directory docstring
docstring(react_fastapi_template(tests), "Backend test files using pytest with TestClient for API testing").

% Test files as children
component(react_fastapi_template(tests), child, react_fastapi_template(tests(source(file('./test_main.py'))))).
component(react_fastapi_template(tests), child, react_fastapi_template(tests(source(file('./test_websocket.py'))))).
component(react_fastapi_template(tests), child, react_fastapi_template(tests(source(file('./__init__.py'))))).

% Test file entities with docstrings
entity(react_fastapi_template(tests(source(file('./test_main.py'))))).
docstring(react_fastapi_template(tests(source(file('./test_main.py')))), "Main API endpoint tests using FastAPI TestClient").

entity(react_fastapi_template(tests(source(file('./test_websocket.py'))))).
docstring(react_fastapi_template(tests(source(file('./test_websocket.py')))), "WebSocket connection and messaging tests").

entity(react_fastapi_template(tests(source(file('./__init__.py'))))).
docstring(react_fastapi_template(tests(source(file('./__init__.py')))), "Python package initialization for tests module").

% Test framework and patterns
component(react_fastapi_template(tests), framework, pytest).
component(react_fastapi_template(tests), test_client, fastapi_testclient).
component(react_fastapi_template(tests), test_types, [unit_tests, integration_tests]).
component(react_fastapi_template(tests), coverage, api_endpoints).