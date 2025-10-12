% Tests for Python REST API template semantics
:- begin_tests(interface_api).

% Test that the entity is properly declared
test(entity_exists) :-
    entity(interface_api).

% Test basic component properties
test(project_type) :-
    user:please_verify(component(interface_api, project_type, python)).

test(build_system) :-
    user:please_verify(component(interface_api, build_system, nix)).

test(framework) :-
    user:please_verify(component(interface_api, framework, fastapi)).

% Test that subcommands are defined
test(subcommands_exist) :-
    user:please_verify(component(interface_api, subcommand, run)),
    user:please_verify(component(interface_api, subcommand, test)),
    user:please_verify(component(interface_api, subcommand, develop)).

% Test that API endpoints are defined
test(api_endpoints_exist) :-
    findall(Endpoint, component(interface_api, api_endpoint, Endpoint), Endpoints),
    length(Endpoints, Count),
    Count >= 10.  % Core endpoints (session removed during rework)

% Test specific endpoints exist
test(root_endpoint) :-
    user:please_verify(component(interface_api, api_endpoint, endpoint(get, "/", _))).

test(compt_endpoint) :-
    user:please_verify(component(interface_api, api_endpoint, endpoint(get, "/compt", _))).

test(comp_endpoint) :-
    user:please_verify(component(interface_api, api_endpoint, endpoint(get, "/comp/{entity}/{comp_type}", _))).

test(perceive_endpoint) :-
    user:please_verify(component(interface_api, api_endpoint, endpoint(get, "/perceive", _))).

test(conjure_endpoint) :-
    user:please_verify(component(interface_api, api_endpoint, endpoint(post, "/conjure", _))).

test(doc_endpoint) :-
    user:please_verify(component(interface_api, api_endpoint, endpoint(get, "/doc", _))).

% Test new endpoints
test(entities_endpoint) :-
    user:please_verify(component(interface_api, api_endpoint, endpoint(get, "/entities", _))).

test(test_endpoint) :-
    user:please_verify(component(interface_api, api_endpoint, endpoint(get, "/test", _))).

test(health_endpoint) :-
    user:please_verify(component(interface_api, api_endpoint, endpoint(get, "/health", _))).

% Test that expected files are defined
test(expected_files) :-
    user:please_verify(component(interface_api, expected_file, file("pyproject.toml"))),
    user:please_verify(component(interface_api, expected_file, file("main.py"))).

% Test that main source is identified
test(main_source) :-
    user:please_verify(component(interface_api, main_source, file("main.py"))).

% Test conjure integration - check subcommand spells
test(conjure_available) :-
    user:please_verify(component(conjure, ctor, interface_api(run))),
    user:please_verify(component(conjure, ctor, interface_api(test))),
    user:please_verify(component(conjure, ctor, interface_api(develop))).

% Test command implementations (mock tests - would require actual nix in real test)
test(run_command_defined) :-
    % This is a mock test - in real environment would test actual command execution
    clause(cast(conjure(interface_api(run)), _), _).

test(test_command_defined) :-
    % This is a mock test - in real environment would test actual command execution
    clause(cast(conjure(interface_api(test)), _), _).

% Test that documentation exists
test(has_docstring) :-
    docstring(interface_api, Doc),
    Doc \= "".

test(subcommand_docstrings) :-
    docstring(conjure(interface_api(run)), _),
    docstring(conjure(interface_api(test)), _),
    docstring(conjure(interface_api(develop)), _).

% Test endpoint documentation
test(endpoint_docstrings) :-
    docstring(endpoint(get, "/", _), _),
    docstring(endpoint(post, "/conjure", _), _),
    docstring(endpoint(get, "/perceive", _), _).

% Test patterns are defined
test(framework_patterns) :-
    user:please_verify(component(interface_api, framework_pattern, _)),
    user:please_verify(component(interface_api, server_pattern, _)),
    user:please_verify(component(interface_api, api_pattern, _)).

test(session_management_patterns) :-
    % component(interface_api, session_pattern, _),  % Disabled - session being reworked
    user:please_verify(component(interface_api, response_pattern, _)).

% Test build artifacts patterns
test(build_artifacts) :-
    findall(Pattern, component(interface_api, build_artifact_pattern, Pattern), Patterns),
    length(Patterns, Count),
    Count >= 3.  % Should have at least 3 build artifact patterns

:- end_tests(interface_api).