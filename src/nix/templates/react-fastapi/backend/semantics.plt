% PLUnit tests for Backend API Semantics
% Tests the completeness and consistency of backend ECS declarations

:- begin_tests(react_fastapi_backend).

% Test self-entity declaration
test(backend_entity_declared) :-
    entity(react_fastapi_template(backend)).

% Test main docstring exists
test(backend_has_docstring) :-
    docstring(react_fastapi_template(backend), _).

% Test technology stack components are declared
test(backend_technology_stack) :-
    component(react_fastapi_template(backend), web_framework, fastapi),
    component(react_fastapi_template(backend), language, python),
    component(react_fastapi_template(backend), orm, sqlalchemy),
    component(react_fastapi_template(backend), database, sqlite).

% Test authentication components
test(backend_auth_components) :-
    component(react_fastapi_template(backend), authentication, jwt_bearer),
    component(react_fastapi_template(backend), password_hashing, bcrypt).

% Test all API endpoint entities are declared
test(api_endpoint_entities_declared) :-
    entity(auth_register),
    entity(auth_login),
    entity(auth_me),
    entity(tasks_list),
    entity(tasks_create),
    entity(tasks_get),
    entity(tasks_update),
    entity(tasks_delete),
    entity(health_check),
    entity(api_docs),
    entity(openapi_spec),
    entity(websocket_connection).

% Test endpoint HTTP methods are specified
test(endpoints_have_http_methods) :-
    component(auth_register, http_method, post),
    component(auth_login, http_method, post),
    component(auth_me, http_method, get),
    component(tasks_list, http_method, get),
    component(tasks_create, http_method, post),
    component(tasks_get, http_method, get),
    component(tasks_update, http_method, put),
    component(tasks_delete, http_method, delete),
    component(health_check, http_method, get).

% Test endpoint paths are specified
test(endpoints_have_paths) :-
    component(auth_register, path, "/api/register"),
    component(auth_login, path, "/api/login"),
    component(auth_me, path, "/api/me"),
    component(tasks_list, path, "/api/tasks"),
    component(tasks_create, path, "/api/tasks"),
    component(tasks_get, path, "/api/tasks/{id}"),
    component(tasks_update, path, "/api/tasks/{id}"),
    component(tasks_delete, path, "/api/tasks/{id}"),
    component(health_check, path, "/api/health").

% Test endpoints have descriptions
test(endpoints_have_descriptions) :-
    component(auth_register, description, _),
    component(auth_login, description, _),
    component(auth_me, description, _),
    component(tasks_list, description, _),
    component(tasks_create, description, _),
    component(tasks_get, description, _),
    component(tasks_update, description, _),
    component(tasks_delete, description, _),
    component(health_check, description, _).

% Test auth requirements are specified
test(auth_requirements_specified) :-
    component(auth_me, requires_auth, true),
    component(tasks_list, requires_auth, true),
    component(tasks_create, requires_auth, true),
    component(tasks_get, requires_auth, true),
    component(tasks_update, requires_auth, true),
    component(tasks_delete, requires_auth, true),
    component(health_check, requires_auth, false).

% Test status codes are specified for endpoints
test(endpoints_have_status_codes) :-
    component(auth_register, status_codes, RegisterCodes),
    is_list(RegisterCodes),
    member(201, RegisterCodes),
    component(auth_login, status_codes, LoginCodes),
    is_list(LoginCodes),
    member(200, LoginCodes),
    component(health_check, status_codes, HealthCodes),
    is_list(HealthCodes),
    member(200, HealthCodes).

% Test database model entities are declared
test(database_models_declared) :-
    entity(user_model),
    entity(task_model).

% Test database models have table names
test(models_have_table_names) :-
    component(user_model, table_name, "users"),
    component(task_model, table_name, "tasks").

% Test database models have fields
test(models_have_fields) :-
    component(user_model, fields, UserFields),
    is_list(UserFields),
    member(id, UserFields),
    member(email, UserFields),
    component(task_model, fields, TaskFields),
    is_list(TaskFields),
    member(id, TaskFields),
    member(title, TaskFields).

% Test Pydantic schema entities are declared
test(pydantic_schemas_declared) :-
    entity(user_registration_schema),
    entity(user_response_schema),
    entity(token_response_schema),
    entity(task_create_schema),
    entity(task_update_schema),
    entity(task_response_schema),
    entity(health_status_schema).

% Test schemas have fields
test(schemas_have_fields) :-
    component(user_registration_schema, fields, RegFields),
    is_list(RegFields),
    member(email, RegFields),
    member(password, RegFields),
    component(task_create_schema, fields, CreateFields),
    is_list(CreateFields),
    member(title, CreateFields).

% Test configuration entities are declared
test(config_entities_declared) :-
    entity(jwt_config),
    entity(cors_config),
    entity(middleware_stack),
    entity(database_dependency),
    entity(authentication_service),
    entity(websocket_manager).

% Test JWT configuration components
test(jwt_config_complete) :-
    component(jwt_config, algorithm, "HS256"),
    component(jwt_config, expiration_time, 1800),
    component(jwt_config, token_type, "bearer").

% Test CORS configuration
test(cors_config_complete) :-
    component(cors_config, allowed_origins, Origins),
    is_list(Origins),
    component(cors_config, allowed_methods, Methods),
    is_list(Methods),
    member("GET", Methods),
    member("POST", Methods).

% Test WebSocket configuration
test(websocket_config_complete) :-
    entity(websocket_connection),
    component(websocket_connection, protocol, websocket),
    component(websocket_connection, path, "/ws"),
    component(websocket_connection, message_types, Types),
    is_list(Types),
    member(echo, Types).

% Test testing configuration
test(testing_config_declared) :-
    entity(test_configuration),
    component(test_configuration, test_framework, pytest),
    component(test_configuration, test_client, fastapi_testclient).

% Test deployment configuration
test(deployment_config_present) :-
    component(react_fastapi_template(backend), docker_image, _),
    component(react_fastapi_template(backend), port, 8000),
    component(react_fastapi_template(backend), environment, Environments),
    is_list(Environments),
    member(development, Environments),
    member(production, Environments).

% Test file structure components
test(file_structure_specified) :-
    component(react_fastapi_template(backend), main_file, "main.py"),
    component(react_fastapi_template(backend), config_file, "pyproject.toml").

% Test middleware stack completeness
test(middleware_stack_complete) :-
    component(middleware_stack, cors_middleware, enabled),
    component(middleware_stack, jwt_middleware, enabled),
    component(middleware_stack, error_handling, enabled).

% Test service entities completeness
test(service_entities_complete) :-
    component(authentication_service, password_hashing, bcrypt),
    component(authentication_service, token_generation, jwt),
    component(websocket_manager, connection_tracking, enabled),
    component(websocket_manager, broadcast_capability, enabled).

:- end_tests(react_fastapi_backend).