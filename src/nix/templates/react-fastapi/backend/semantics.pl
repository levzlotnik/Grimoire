% Backend semantics for React FastAPI Template
% FastAPI backend with SQLAlchemy and sample dashboard APIs

:- self_entity(react_fastapi_template(backend)).

% Backend docstring
docstring(react_fastapi_template(backend), "FastAPI backend with SQLAlchemy ORM, dashboard APIs, and authentication endpoints").

% Backend child components
component(react_fastapi_template(backend), child, react_fastapi_template(backend(source(file('./main.py'))))).
component(react_fastapi_template(backend), child, react_fastapi_template(backend(source(file('./pyproject.toml'))))).
component(react_fastapi_template(backend), child, react_fastapi_template(backend(source(file('./Dockerfile'))))).

% Source file entities with docstrings
entity(react_fastapi_template(backend(source(file('./main.py'))))).
docstring(react_fastapi_template(backend(source(file('./main.py')))), "Main FastAPI application with SQLAlchemy models, authentication endpoints, dashboard APIs, and WebSocket support").

entity(react_fastapi_template(backend(source(file('./pyproject.toml'))))).
docstring(react_fastapi_template(backend(source(file('./pyproject.toml')))), "Python package configuration with FastAPI dependencies").

entity(react_fastapi_template(backend(source(file('./Dockerfile'))))).
docstring(react_fastapi_template(backend(source(file('./Dockerfile')))), "Docker container configuration for backend service").

% Backend technology stack
component(react_fastapi_template(backend), web_framework, fastapi).
component(react_fastapi_template(backend), orm, sqlalchemy).
component(react_fastapi_template(backend), database, sqlite).
component(react_fastapi_template(backend), validation, pydantic).
component(react_fastapi_template(backend), server, uvicorn).
component(react_fastapi_template(backend), auth_method, jwt).
component(react_fastapi_template(backend), password_hashing, bcrypt).

% API endpoint categories
component(react_fastapi_template(backend), endpoint_category, authentication).
component(react_fastapi_template(backend), endpoint_category, dashboard).
component(react_fastapi_template(backend), endpoint_category, health_check).
component(react_fastapi_template(backend), endpoint_category, api_docs).
component(react_fastapi_template(backend), endpoint_category, websocket).

% Authentication endpoints
component(react_fastapi_template(backend), api_endpoint, endpoint(post, '/api/register', 'User registration')).
component(react_fastapi_template(backend), api_endpoint, endpoint(post, '/api/login', 'User login')).
component(react_fastapi_template(backend), api_endpoint, endpoint(get, '/api/me', 'Get current user')).

% Task management endpoints (unused by frontend but present)
component(react_fastapi_template(backend), api_endpoint, endpoint(get, '/api/tasks', 'Get user tasks')).
component(react_fastapi_template(backend), api_endpoint, endpoint(post, '/api/tasks', 'Create new task')).
component(react_fastapi_template(backend), api_endpoint, endpoint(get, '/api/tasks/{id}', 'Get specific task')).
component(react_fastapi_template(backend), api_endpoint, endpoint(put, '/api/tasks/{id}', 'Update task')).
component(react_fastapi_template(backend), api_endpoint, endpoint(delete, '/api/tasks/{id}', 'Delete task')).

% Dashboard endpoints (used by frontend)
component(react_fastapi_template(backend), api_endpoint, endpoint(get, '/api/dashboard/stats', 'Dashboard statistics')).
component(react_fastapi_template(backend), api_endpoint, endpoint(get, '/api/dashboard/monthly-data', 'Monthly revenue data')).
component(react_fastapi_template(backend), api_endpoint, endpoint(get, '/api/dashboard/device-distribution', 'Device usage distribution')).
component(react_fastapi_template(backend), api_endpoint, endpoint(get, '/api/dashboard/hourly-activity', 'Hourly activity data')).
component(react_fastapi_template(backend), api_endpoint, endpoint(get, '/api/dashboard/transactions', 'Recent transactions')).

% System endpoints
component(react_fastapi_template(backend), api_endpoint, endpoint(get, '/api/health', 'Health check')).
component(react_fastapi_template(backend), api_endpoint, endpoint(get, '/api/docs/openapi', 'OpenAPI schema')).
component(react_fastapi_template(backend), api_endpoint, endpoint(get, '/api/docs/endpoints', 'API endpoint documentation')).
component(react_fastapi_template(backend), api_endpoint, endpoint(websocket, '/ws', 'WebSocket connection')).

% Database models
component(react_fastapi_template(backend), database_model, user).
component(react_fastapi_template(backend), database_model, task).

% Configuration
component(react_fastapi_template(backend), cors_origins, ['http://localhost:3000', 'http://localhost:5173']).
component(react_fastapi_template(backend), database_file, tempfile).
component(react_fastapi_template(backend), auto_docs, true).