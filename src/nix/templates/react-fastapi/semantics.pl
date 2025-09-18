% React + FastAPI Fullstack Template Semantics
% Modern fullstack web application with React frontend and FastAPI backend

:- discontiguous component/3.
:- discontiguous entity/1.
:- discontiguous docstring/2.

entity(react_fastapi_template).

% Main docstring for the project
docstring(react_fastapi_template, "A modern fullstack web application template using React (TypeScript) frontend and FastAPI (Python) backend. Features JWT authentication, real-time WebSocket communication, task management, and comprehensive testing.").

% Project type identification
component(react_fastapi_template, project_type, fullstack_web).
component(react_fastapi_template, build_system, nix).
component(react_fastapi_template, frontend_language, typescript).
component(react_fastapi_template, backend_language, python).
component(react_fastapi_template, frontend_framework, react).
component(react_fastapi_template, backend_framework, fastapi).
component(react_fastapi_template, database, sqlite).
component(react_fastapi_template, auth_method, jwt).
component(react_fastapi_template, realtime, websocket).

% Make this entity available as a command
component(conjure, ctor, react_fastapi_template).

% Nix-provided subcommands (from flake apps)
component(react_fastapi_template, subcommand, run).
component(react_fastapi_template, subcommand, dev).
component(react_fastapi_template, subcommand, test).
component(react_fastapi_template, subcommand, test_backend).
component(react_fastapi_template, subcommand, test_frontend).

% Docstrings for Nix-based subcommands
docstring(react_fastapi_template(run), "Run the production fullstack application using 'nix run .#run'").
docstring(react_fastapi_template(dev), "Start development servers for both frontend and backend using 'nix run .#dev'").
docstring(react_fastapi_template(test), "Run all tests (frontend and backend) using 'nix run .#test'").
docstring(react_fastapi_template(test_backend), "Run backend tests only using 'nix run .#test-backend'").
docstring(react_fastapi_template(test_frontend), "Run frontend tests only using 'nix run .#test-frontend'").

% Make subcommands available as ctors too
component(react_fastapi_template, ctor, C) :- component(react_fastapi_template, subcommand, C).

% Command implementations using Nix
cast(conjure(react_fastapi_template(run)), RetVal) :-
    cast(conjure(nix(run(['.#run']))), RetVal).

cast(conjure(react_fastapi_template(dev)), RetVal) :-
    cast(conjure(nix(run(['.#dev']))), RetVal).

cast(conjure(react_fastapi_template(test)), RetVal) :-
    cast(conjure(nix(run(['.#test']))), RetVal).

cast(conjure(react_fastapi_template(test_backend)), RetVal) :-
    cast(conjure(nix(run(['.#test-backend']))), RetVal).

cast(conjure(react_fastapi_template(test_frontend)), RetVal) :-
    cast(conjure(nix(run(['.#test-frontend']))), RetVal).

% Define the main components of the fullstack application
entity(frontend_app).
entity(backend_app).
entity(database_layer).
entity(auth_system).
entity(websocket_service).
entity(task_domain).

% Frontend application components
component(frontend_app, technology_stack, react_typescript_vite).
component(frontend_app, state_management, zustand).
component(frontend_app, styling, tailwind_css).
component(frontend_app, routing, react_router).
component(frontend_app, form_handling, react_hook_form).
component(frontend_app, validation, zod).
component(frontend_app, http_client, axios).
component(frontend_app, testing, vitest).
component(frontend_app, ui_components, custom_components).

% Backend application components
component(backend_app, web_framework, fastapi).
component(backend_app, orm, sqlalchemy).
component(backend_app, database, sqlite).
component(backend_app, authentication, jwt_bearer).
component(backend_app, password_hashing, bcrypt).
component(backend_app, validation, pydantic).
component(backend_app, testing, pytest).
component(backend_app, server, uvicorn).

% Authentication system components
component(auth_system, token_type, jwt).
component(auth_system, password_hashing, bcrypt).
component(auth_system, token_expiry, thirty_minutes).
component(auth_system, endpoints, [register, login, current_user]).

% WebSocket service components
component(websocket_service, real_time_features, [task_notifications, status_updates]).
component(websocket_service, connection_management, connection_manager).
component(websocket_service, message_types, [task_created, task_updated, task_deleted, echo, error]).

% Task domain components
component(task_domain, crud_operations, [create, read, update, delete]).
component(task_domain, fields, [title, description, completed, timestamps]).
component(task_domain, user_isolation, true).
component(task_domain, real_time_sync, websocket).

% API endpoint definitions as entities and components
entity(api_endpoint).

% Authentication endpoints
component(react_fastapi_template, api_endpoint, endpoint(post, "/api/register", "User registration")).
component(react_fastapi_template, api_endpoint, endpoint(post, "/api/login", "User login")).
component(react_fastapi_template, api_endpoint, endpoint(get, "/api/me", "Get current user")).

% Task management endpoints
component(react_fastapi_template, api_endpoint, endpoint(get, "/api/tasks", "Get user tasks")).
component(react_fastapi_template, api_endpoint, endpoint(post, "/api/tasks", "Create new task")).
component(react_fastapi_template, api_endpoint, endpoint(get, "/api/tasks/{id}", "Get specific task")).
component(react_fastapi_template, api_endpoint, endpoint(put, "/api/tasks/{id}", "Update task")).
component(react_fastapi_template, api_endpoint, endpoint(delete, "/api/tasks/{id}", "Delete task")).

% System endpoints
component(react_fastapi_template, api_endpoint, endpoint(get, "/api/health", "Health check")).
component(react_fastapi_template, api_endpoint, endpoint(websocket, "/ws", "WebSocket connection")).

% Frontend pages and components
entity(frontend_page).
component(react_fastapi_template, frontend_page, page("LoginPage", "User authentication")).
component(react_fastapi_template, frontend_page, page("RegisterPage", "User registration")).
component(react_fastapi_template, frontend_page, page("DashboardPage", "Main application interface")).

entity(frontend_component).
component(react_fastapi_template, frontend_component, comp("Button", "Reusable button component")).
component(react_fastapi_template, frontend_component, comp("Input", "Form input component")).
component(react_fastapi_template, frontend_component, comp("Card", "Content container component")).
component(react_fastapi_template, frontend_component, comp("TaskCard", "Task display and editing")).
component(react_fastapi_template, frontend_component, comp("CreateTaskForm", "Task creation form")).
component(react_fastapi_template, frontend_component, comp("WebSocketStatus", "Connection status indicator")).

% State management
entity(frontend_store).
component(react_fastapi_template, frontend_store, store("authStore", "Authentication state management")).
component(react_fastapi_template, frontend_store, store("taskStore", "Task state management")).

% Project structure expectations
component(react_fastapi_template, expected_file, file("flake.nix")).
component(react_fastapi_template, expected_file, file("docker-compose.yml")).
component(react_fastapi_template, expected_file, file("backend/pyproject.toml")).
component(react_fastapi_template, expected_file, file("backend/main.py")).
component(react_fastapi_template, expected_file, file("frontend/package.json")).
component(react_fastapi_template, expected_file, file("frontend/vite.config.ts")).
component(react_fastapi_template, expected_file, file("shared/types.ts")).

% Main source files
component(react_fastapi_template, main_source, file("backend/main.py")).
component(react_fastapi_template, main_source, file("frontend/src/App.tsx")).
component(react_fastapi_template, main_source, file("frontend/src/main.tsx")).

% Technology patterns
component(react_fastapi_template, frontend_pattern, "React with TypeScript and Vite").
component(react_fastapi_template, backend_pattern, "FastAPI with async/await").
component(react_fastapi_template, api_pattern, "RESTful endpoints with WebSocket").
component(react_fastapi_template, auth_pattern, "JWT bearer token authentication").
component(react_fastapi_template, database_pattern, "SQLAlchemy ORM with SQLite").
component(react_fastapi_template, state_pattern, "Zustand for client state management").
component(react_fastapi_template, styling_pattern, "Tailwind CSS with custom components").

% Testing patterns
component(react_fastapi_template, backend_testing, "pytest with TestClient").
component(react_fastapi_template, frontend_testing, "vitest with React Testing Library").
component(react_fastapi_template, test_coverage, "Unit and integration tests").

% Deployment patterns
component(react_fastapi_template, deployment_nix, "Nix flake for reproducible builds").
component(react_fastapi_template, deployment_docker, "Docker Compose for containerization").
component(react_fastapi_template, deployment_production, "Multi-stage Docker build").

% Development patterns
component(react_fastapi_template, dev_pattern, "Hot reload for both frontend and backend").
component(react_fastapi_template, cors_pattern, "CORS middleware for development").
component(react_fastapi_template, proxy_pattern, "Vite proxy for API requests").

% Security patterns
component(react_fastapi_template, security_pattern, "JWT token authentication").
component(react_fastapi_template, password_security, "bcrypt password hashing").
component(react_fastapi_template, cors_security, "Configured CORS origins").
component(react_fastapi_template, validation_security, "Pydantic request validation").

% Real-time patterns
component(react_fastapi_template, websocket_pattern, "WebSocket for real-time updates").
component(react_fastapi_template, reconnection_pattern, "Automatic WebSocket reconnection").
component(react_fastapi_template, notification_pattern, "Real-time task notifications").

% Build artifacts patterns
component(react_fastapi_template, build_artifact_pattern, glob("frontend/dist/**/*")).
component(react_fastapi_template, build_artifact_pattern, glob("frontend/node_modules/**/*")).
component(react_fastapi_template, build_artifact_pattern, glob("backend/__pycache__/**/*")).
component(react_fastapi_template, build_artifact_pattern, glob("backend/*.egg-info/**/*")).
component(react_fastapi_template, build_artifact_pattern, glob("backend/.pytest_cache/**/*")).
component(react_fastapi_template, build_artifact_pattern, glob("*.db")).
component(react_fastapi_template, build_artifact_pattern, glob("result*")).
