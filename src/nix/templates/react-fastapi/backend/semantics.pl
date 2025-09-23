% Backend API Semantics - FastAPI Application
% Knowledge representation of the FastAPI backend component

% Self-entity declaration pattern
:- self_entity(react_fastapi_template(backend)).

docstring(react_fastapi_template(backend), "FastAPI backend server with JWT authentication, task management, WebSocket support, and comprehensive API documentation. Built with SQLAlchemy ORM and SQLite database.").

% Technology stack
component(react_fastapi_template(backend), web_framework, fastapi).
component(react_fastapi_template(backend), language, python).
component(react_fastapi_template(backend), orm, sqlalchemy).
component(react_fastapi_template(backend), database, sqlite).
component(react_fastapi_template(backend), authentication, jwt_bearer).
component(react_fastapi_template(backend), password_hashing, bcrypt).
component(react_fastapi_template(backend), validation, pydantic).
component(react_fastapi_template(backend), testing, pytest).
component(react_fastapi_template(backend), server, uvicorn).
component(react_fastapi_template(backend), cors_middleware, enabled).

% API Endpoints as entities
entity(api_endpoint).

% Authentication endpoints
entity(auth_register).
component(auth_register, http_method, post).
component(auth_register, path, "/api/register").
component(auth_register, description, "User registration with email and password").
component(auth_register, request_body, user_registration_schema).
component(auth_register, response_body, user_response_schema).
component(auth_register, status_codes, [201, 400, 422]).

entity(auth_login).
component(auth_login, http_method, post).
component(auth_login, path, "/api/login").
component(auth_login, description, "User authentication returning JWT token").
component(auth_login, request_body, form_data).
component(auth_login, response_body, token_response_schema).
component(auth_login, status_codes, [200, 401, 422]).

entity(auth_me).
component(auth_me, http_method, get).
component(auth_me, path, "/api/me").
component(auth_me, description, "Get current authenticated user information").
component(auth_me, requires_auth, true).
component(auth_me, response_body, user_response_schema).
component(auth_me, status_codes, [200, 401]).

% Task management endpoints
entity(tasks_list).
component(tasks_list, http_method, get).
component(tasks_list, path, "/api/tasks").
component(tasks_list, description, "Get all tasks for authenticated user").
component(tasks_list, requires_auth, true).
component(tasks_list, response_body, task_list_schema).
component(tasks_list, status_codes, [200, 401]).

entity(tasks_create).
component(tasks_create, http_method, post).
component(tasks_create, path, "/api/tasks").
component(tasks_create, description, "Create new task for authenticated user").
component(tasks_create, requires_auth, true).
component(tasks_create, request_body, task_create_schema).
component(tasks_create, response_body, task_response_schema).
component(tasks_create, status_codes, [201, 400, 401, 422]).

entity(tasks_get).
component(tasks_get, http_method, get).
component(tasks_get, path, "/api/tasks/{id}").
component(tasks_get, description, "Get specific task by ID for authenticated user").
component(tasks_get, requires_auth, true).
component(tasks_get, path_parameters, [task_id]).
component(tasks_get, response_body, task_response_schema).
component(tasks_get, status_codes, [200, 401, 404]).

entity(tasks_update).
component(tasks_update, http_method, put).
component(tasks_update, path, "/api/tasks/{id}").
component(tasks_update, description, "Update specific task by ID").
component(tasks_update, requires_auth, true).
component(tasks_update, path_parameters, [task_id]).
component(tasks_update, request_body, task_update_schema).
component(tasks_update, response_body, task_response_schema).
component(tasks_update, status_codes, [200, 400, 401, 404, 422]).

entity(tasks_delete).
component(tasks_delete, http_method, delete).
component(tasks_delete, path, "/api/tasks/{id}").
component(tasks_delete, description, "Delete specific task by ID").
component(tasks_delete, requires_auth, true).
component(tasks_delete, path_parameters, [task_id]).
component(tasks_delete, status_codes, [204, 401, 404]).

% System endpoints
entity(health_check).
component(health_check, http_method, get).
component(health_check, path, "/api/health").
component(health_check, description, "Health check endpoint for monitoring").
component(health_check, requires_auth, false).
component(health_check, response_body, health_status_schema).
component(health_check, status_codes, [200]).

entity(api_docs).
component(api_docs, http_method, get).
component(api_docs, path, "/docs").
component(api_docs, description, "Interactive API documentation (Swagger UI)").
component(api_docs, requires_auth, false).
component(api_docs, response_body, html).
component(api_docs, status_codes, [200]).

entity(openapi_spec).
component(openapi_spec, http_method, get).
component(openapi_spec, path, "/openapi.json").
component(openapi_spec, description, "OpenAPI specification JSON").
component(openapi_spec, requires_auth, false).
component(openapi_spec, response_body, openapi_schema).
component(openapi_spec, status_codes, [200]).

% WebSocket endpoint
entity(websocket_connection).
component(websocket_connection, protocol, websocket).
component(websocket_connection, path, "/ws").
component(websocket_connection, description, "Real-time WebSocket connection for task updates").
component(websocket_connection, requires_auth, true).
component(websocket_connection, message_types, [task_created, task_updated, task_deleted, echo, error]).

% Database models as entities
entity(user_model).
component(user_model, table_name, "users").
component(user_model, fields, [id, email, hashed_password, created_at, updated_at]).
component(user_model, primary_key, id).
component(user_model, unique_constraints, [email]).

entity(task_model).
component(task_model, table_name, "tasks").
component(task_model, fields, [id, title, description, completed, user_id, created_at, updated_at]).
component(task_model, primary_key, id).
component(task_model, foreign_keys, [user_id]).
component(task_model, relationships, [user]).

% Pydantic schemas as entities
entity(user_registration_schema).
component(user_registration_schema, fields, [email, password]).
component(user_registration_schema, validation, [email_format, password_length]).

entity(user_response_schema).
component(user_response_schema, fields, [id, email, created_at]).

entity(token_response_schema).
component(token_response_schema, fields, [access_token, token_type, expires_in]).

entity(task_create_schema).
component(task_create_schema, fields, [title, description]).
component(task_create_schema, validation, [title_required, title_max_length]).

entity(task_update_schema).
component(task_update_schema, fields, [title, description, completed]).
component(task_update_schema, validation, [optional_fields]).

entity(task_response_schema).
component(task_response_schema, fields, [id, title, description, completed, user_id, created_at, updated_at]).

entity(health_status_schema).
component(health_status_schema, fields, [status, timestamp, version]).

% Security configuration
entity(jwt_config).
component(jwt_config, algorithm, "HS256").
component(jwt_config, expiration_time, 1800). % 30 minutes
component(jwt_config, token_type, "bearer").

entity(cors_config).
component(cors_config, allowed_origins, ["http://localhost:3000", "http://localhost:5173"]).
component(cors_config, allowed_methods, ["GET", "POST", "PUT", "DELETE"]).
component(cors_config, allowed_headers, ["*"]).
component(cors_config, allow_credentials, true).

% Middleware stack
entity(middleware_stack).
component(middleware_stack, cors_middleware, enabled).
component(middleware_stack, jwt_middleware, enabled).
component(middleware_stack, error_handling, enabled).

% Dependencies and services
entity(database_dependency).
component(database_dependency, connection_type, sqlite).
component(database_dependency, file_path, "./app.db").
component(database_dependency, connection_pool, enabled).

entity(authentication_service).
component(authentication_service, password_hashing, bcrypt).
component(authentication_service, token_generation, jwt).
component(authentication_service, token_validation, jwt_bearer).

entity(websocket_manager).
component(websocket_manager, connection_tracking, enabled).
component(websocket_manager, broadcast_capability, enabled).
component(websocket_manager, connection_cleanup, enabled).

% File structure
component(react_fastapi_template(backend), main_file, "main.py").
component(react_fastapi_template(backend), config_file, "pyproject.toml").
component(react_fastapi_template(backend), requirements_file, "requirements.txt").
component(react_fastapi_template(backend), database_file, "app.db").

% Testing configuration
entity(test_configuration).
component(test_configuration, test_framework, pytest).
component(test_configuration, test_client, fastapi_testclient).
component(test_configuration, test_database, ":memory:").
component(test_configuration, test_coverage, enabled).

% Deployment configuration
component(react_fastapi_template(backend), docker_image, "python:3.11-slim").
component(react_fastapi_template(backend), port, 8000).
component(react_fastapi_template(backend), environment, [development, production]).
component(react_fastapi_template(backend), logging_level, "INFO").