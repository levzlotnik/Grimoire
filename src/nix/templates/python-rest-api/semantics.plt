% Tests for Python REST API template semantics
:- begin_tests(python_rest_api_template).

% Test that the entity is properly declared
test(entity_exists) :-
    entity(python_rest_api_template).

% Test basic component properties
test(project_type) :-
    component(python_rest_api_template, project_type, python).

test(build_system) :-
    component(python_rest_api_template, build_system, nix).

test(framework) :-
    component(python_rest_api_template, framework, fastapi).

% Test that subcommands are defined
test(subcommands_exist) :-
    component(python_rest_api_template, subcommand, run),
    component(python_rest_api_template, subcommand, test),
    component(python_rest_api_template, subcommand, develop).

% Test that API endpoints are defined
test(api_endpoints_exist) :-
    findall(Endpoint, component(python_rest_api_template, api_endpoint, Endpoint), Endpoints),
    length(Endpoints, Count),
    Count >= 5.  % Should have at least 5 endpoints

% Test specific endpoints exist
test(hello_endpoint) :-
    component(python_rest_api_template, api_endpoint, endpoint(get, "/hello", _)).

test(perceive_endpoint) :-
    component(python_rest_api_template, api_endpoint, endpoint(get, "/perceive", _)).

test(conjure_endpoint) :-
    component(python_rest_api_template, api_endpoint, endpoint(post, "/conjure", _)).

test(data_endpoint) :-
    component(python_rest_api_template, api_endpoint, endpoint(post, "/data", _)).

test(health_endpoint) :-
    component(python_rest_api_template, api_endpoint, endpoint(get, "/health", _)).

% Test that expected files are defined
test(expected_files) :-
    component(python_rest_api_template, expected_file, file("pyproject.toml")),
    component(python_rest_api_template, expected_file, file("main.py")).

% Test that main source is identified
test(main_source) :-
    component(python_rest_api_template, main_source, file("main.py")).

% Test conjure integration
test(conjure_available) :-
    component(conjure, ctor, python_rest_api_template).

% Test command implementations (mock tests - would require actual nix in real test)
test(run_command_defined, [sto(user_error)]) :-
    % This is a mock test - in real environment would test actual command execution
    clause(cast(conjure(python_rest_api_template(run)), _), _).

test(test_command_defined, [sto(user_error)]) :-
    % This is a mock test - in real environment would test actual command execution  
    clause(cast(conjure(python_rest_api_template(test)), _), _).

% Test that documentation exists
test(has_docstring) :-
    docstring(python_rest_api_template, Doc),
    Doc \= "".

test(subcommand_docstrings) :-
    docstring(python_rest_api_template(run), _),
    docstring(python_rest_api_template(test), _),
    docstring(python_rest_api_template(develop), _).

% Test endpoint documentation
test(endpoint_docstrings) :-
    docstring(endpoint(get, "/hello", _), _),
    docstring(endpoint(post, "/conjure", _), _),
    docstring(endpoint(get, "/perceive", _), _).

% Test patterns are defined
test(framework_patterns) :-
    component(python_rest_api_template, framework_pattern, _),
    component(python_rest_api_template, server_pattern, _),
    component(python_rest_api_template, api_pattern, _).

test(session_management_patterns) :-
    component(python_rest_api_template, session_pattern, _),
    component(python_rest_api_template, response_pattern, _).

% Test build artifacts patterns
test(build_artifacts) :-
    findall(Pattern, component(python_rest_api_template, build_artifact_pattern, Pattern), Patterns),
    length(Patterns, Count),
    Count >= 3.  % Should have at least 3 build artifact patterns

:- end_tests(python_rest_api_template).