% Tests for Python REST API template semantics
:- begin_tests(interface_api).

% Test that the entity is properly declared
test(entity_exists) :-
    entity(interface_api).

% Test basic component properties
test(project_type) :-
    component(interface_api, project_type, python).

test(build_system) :-
    component(interface_api, build_system, nix).

test(framework) :-
    component(interface_api, framework, fastapi).

% Test that subcommands are defined
test(subcommands_exist) :-
    component(interface_api, subcommand, run),
    component(interface_api, subcommand, test),
    component(interface_api, subcommand, develop).

% Test that API endpoints are defined
test(api_endpoints_exist) :-
    findall(Endpoint, component(interface_api, api_endpoint, Endpoint), Endpoints),
    length(Endpoints, Count),
    Count >= 14.  % Should have at least 14 endpoints (added entities, test, session, load)

% Test specific endpoints exist
test(root_endpoint) :-
    component(interface_api, api_endpoint, endpoint(get, "/", _)).

test(compt_endpoint) :-
    component(interface_api, api_endpoint, endpoint(get, "/compt", _)).

test(comp_endpoint) :-
    component(interface_api, api_endpoint, endpoint(get, "/comp/{entity}/{comp_type}", _)).

test(perceive_endpoint) :-
    component(interface_api, api_endpoint, endpoint(get, "/perceive", _)).

test(conjure_endpoint) :-
    component(interface_api, api_endpoint, endpoint(post, "/conjure", _)).

test(doc_endpoint) :-
    component(interface_api, api_endpoint, endpoint(get, "/doc", _)).

test(status_endpoint) :-
    component(interface_api, api_endpoint, endpoint(get, "/status", _)).

% Test new endpoints
test(entities_endpoint) :-
    component(interface_api, api_endpoint, endpoint(get, "/entities", _)).

test(test_endpoint) :-
    component(interface_api, api_endpoint, endpoint(get, "/test", _)).

test(session_endpoint) :-
    component(interface_api, api_endpoint, endpoint(post, "/session", _)).

test(load_endpoint) :-
    component(interface_api, api_endpoint, endpoint(post, "/load", _)).

test(health_endpoint) :-
    component(interface_api, api_endpoint, endpoint(get, "/health", _)).

% Test that expected files are defined
test(expected_files) :-
    component(interface_api, expected_file, file("pyproject.toml")),
    component(interface_api, expected_file, file("main.py")).

% Test that main source is identified
test(main_source) :-
    component(interface_api, main_source, file("main.py")).

% Test conjure integration
test(conjure_available) :-
    component(conjure, ctor, interface_api).

% Test command implementations (mock tests - would require actual nix in real test)
test(run_command_defined, [sto(user_error)]) :-
    % This is a mock test - in real environment would test actual command execution
    clause(cast(conjure(interface_api(run)), _), _).

test(test_command_defined, [sto(user_error)]) :-
    % This is a mock test - in real environment would test actual command execution  
    clause(cast(conjure(interface_api(test)), _), _).

% Test that documentation exists
test(has_docstring) :-
    docstring(interface_api, Doc),
    Doc \= "".

test(subcommand_docstrings) :-
    docstring(interface_api(run), _),
    docstring(interface_api(test), _),
    docstring(interface_api(develop), _).

% Test endpoint documentation
test(endpoint_docstrings) :-
    docstring(endpoint(get, "/", _), _),
    docstring(endpoint(post, "/conjure", _), _),
    docstring(endpoint(get, "/perceive", _), _).

% Test patterns are defined
test(framework_patterns) :-
    component(interface_api, framework_pattern, _),
    component(interface_api, server_pattern, _),
    component(interface_api, api_pattern, _).

test(session_management_patterns) :-
    component(interface_api, session_pattern, _),
    component(interface_api, response_pattern, _).

% Test build artifacts patterns
test(build_artifacts) :-
    findall(Pattern, component(interface_api, build_artifact_pattern, Pattern), Patterns),
    length(Patterns, Count),
    Count >= 3.  % Should have at least 3 build artifact patterns

:- end_tests(interface_api).