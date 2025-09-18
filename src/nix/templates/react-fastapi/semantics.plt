% PLUnit tests for React + FastAPI Template Semantics

:- begin_tests(react_fastapi_template).

% Test basic entity declaration
test(entity_declared) :-
    entity(react_fastapi_template).

% Test project type identification
test(project_type_fullstack) :-
    component(react_fastapi_template, project_type, fullstack_web).

test(build_system_nix) :-
    component(react_fastapi_template, build_system, nix).

test(frontend_framework_react) :-
    component(react_fastapi_template, frontend_framework, react).

test(backend_framework_fastapi) :-
    component(react_fastapi_template, backend_framework, fastapi).

% Test available subcommands
test(has_run_subcommand) :-
    component(react_fastapi_template, subcommand, run).

test(has_dev_subcommand) :-
    component(react_fastapi_template, subcommand, dev).

test(has_test_subcommands) :-
    component(react_fastapi_template, subcommand, test),
    component(react_fastapi_template, subcommand, test_backend),
    component(react_fastapi_template, subcommand, test_frontend).

% Test that subcommands are available as ctors
test(subcommands_as_ctors) :-
    component(react_fastapi_template, ctor, run),
    component(react_fastapi_template, ctor, dev),
    component(react_fastapi_template, ctor, test).

% Test docstring existence
test(has_main_docstring) :-
    docstring(react_fastapi_template, _).

test(has_subcommand_docstrings) :-
    docstring(react_fastapi_template(run), _),
    docstring(react_fastapi_template(dev), _),
    docstring(react_fastapi_template(test), _).

% Test API endpoint definitions
test(has_auth_endpoints) :-
    component(react_fastapi_template, api_endpoint, endpoint(post, "/api/register", _)),
    component(react_fastapi_template, api_endpoint, endpoint(post, "/api/login", _)),
    component(react_fastapi_template, api_endpoint, endpoint(get, "/api/me", _)).

test(has_task_endpoints) :-
    component(react_fastapi_template, api_endpoint, endpoint(get, "/api/tasks", _)),
    component(react_fastapi_template, api_endpoint, endpoint(post, "/api/tasks", _)),
    component(react_fastapi_template, api_endpoint, endpoint(put, "/api/tasks/{id}", _)),
    component(react_fastapi_template, api_endpoint, endpoint(delete, "/api/tasks/{id}", _)).

test(has_websocket_endpoint) :-
    component(react_fastapi_template, api_endpoint, endpoint(websocket, "/ws", _)).

test(has_health_endpoint) :-
    component(react_fastapi_template, api_endpoint, endpoint(get, "/api/health", _)).

% Test frontend architecture
test(frontend_technology_stack) :-
    component(frontend_app, technology_stack, react_typescript_vite),
    component(frontend_app, state_management, zustand),
    component(frontend_app, styling, tailwind_css).

test(frontend_pages_defined) :-
    component(react_fastapi_template, frontend_page, page("LoginPage", _)),
    component(react_fastapi_template, frontend_page, page("RegisterPage", _)),
    component(react_fastapi_template, frontend_page, page("DashboardPage", _)).

test(frontend_components_defined) :-
    component(react_fastapi_template, frontend_component, comp("Button", _)),
    component(react_fastapi_template, frontend_component, comp("TaskCard", _)),
    component(react_fastapi_template, frontend_component, comp("CreateTaskForm", _)).

test(frontend_stores_defined) :-
    component(react_fastapi_template, frontend_store, store("authStore", _)),
    component(react_fastapi_template, frontend_store, store("taskStore", _)).

% Test backend architecture
test(backend_technology_stack) :-
    component(backend_app, web_framework, fastapi),
    component(backend_app, orm, sqlalchemy),
    component(backend_app, database, sqlite).

test(auth_system_configured) :-
    component(auth_system, token_type, jwt),
    component(auth_system, password_hashing, bcrypt).

test(websocket_service_configured) :-
    component(websocket_service, connection_management, connection_manager),
    component(websocket_service, real_time_features, _).

% Test task domain
test(task_domain_crud) :-
    component(task_domain, crud_operations, [create, read, update, delete]).

test(task_domain_isolation) :-
    component(task_domain, user_isolation, true).

test(task_domain_realtime) :-
    component(task_domain, real_time_sync, websocket).

% Test expected file structure
test(has_expected_files) :-
    component(react_fastapi_template, expected_file, file("flake.nix")),
    component(react_fastapi_template, expected_file, file("docker-compose.yml")),
    component(react_fastapi_template, expected_file, file("backend/pyproject.toml")),
    component(react_fastapi_template, expected_file, file("frontend/package.json")).

test(has_main_sources) :-
    component(react_fastapi_template, main_source, file("backend/main.py")),
    component(react_fastapi_template, main_source, file("frontend/src/App.tsx")).

% Test technology patterns
test(has_technology_patterns) :-
    component(react_fastapi_template, frontend_pattern, _),
    component(react_fastapi_template, backend_pattern, _),
    component(react_fastapi_template, api_pattern, _).

test(has_security_patterns) :-
    component(react_fastapi_template, security_pattern, _),
    component(react_fastapi_template, password_security, _),
    component(react_fastapi_template, validation_security, _).

test(has_deployment_patterns) :-
    component(react_fastapi_template, deployment_nix, _),
    component(react_fastapi_template, deployment_docker, _).

% Test testing patterns
test(has_testing_patterns) :-
    component(react_fastapi_template, backend_testing, _),
    component(react_fastapi_template, frontend_testing, _),
    component(react_fastapi_template, test_coverage, _).

% Test real-time patterns
test(has_realtime_patterns) :-
    component(react_fastapi_template, websocket_pattern, _),
    component(react_fastapi_template, reconnection_pattern, _),
    component(react_fastapi_template, notification_pattern, _).

% Test build artifact patterns
test(has_build_artifacts) :-
    component(react_fastapi_template, build_artifact_pattern, glob("frontend/dist/**/*")),
    component(react_fastapi_template, build_artifact_pattern, glob("backend/__pycache__/**/*")),
    component(react_fastapi_template, build_artifact_pattern, glob("*.db")).

% Test that the entity is available as a conjure target
test(conjure_target_available) :-
    component(conjure, ctor, react_fastapi_template).

% Test command implementations (these would require actual Nix environment to test)
test(command_implementations_defined, [true]) :-
    % We can test that the clauses are defined
    clause(cast(conjure(react_fastapi_template(run)), _), _),
    clause(cast(conjure(react_fastapi_template(dev)), _), _),
    clause(cast(conjure(react_fastapi_template(test)), _), _).

% Test completeness - ensure all major components are covered
test(architecture_completeness) :-
    % Frontend
    entity(frontend_app),
    entity(frontend_page),
    entity(frontend_component),
    entity(frontend_store),
    % Backend
    entity(backend_app),
    entity(auth_system),
    entity(websocket_service),
    entity(task_domain),
    % API
    entity(api_endpoint).

% Test consistency - ensure patterns match the technology choices
test(technology_consistency) :-
    component(react_fastapi_template, frontend_framework, react),
    component(frontend_app, technology_stack, react_typescript_vite),
    component(react_fastapi_template, backend_framework, fastapi),
    component(backend_app, web_framework, fastapi).

:- end_tests(react_fastapi_template).
