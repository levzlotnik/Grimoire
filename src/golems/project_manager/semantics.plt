% Tests for Project Manager Golem semantics
% Validates entity declarations, components, and semantic relationships

:- use_module(library(plunit)).

% Helper predicate: check if end-to-end tests should run
:- multifile e2e_tests_enabled/0.
e2e_tests_enabled :-
    getenv('GRIMOIRE_RUN_E2E_TESTS', '1').

:- begin_tests('golem(project_manager)').

% === ENTITY TESTS ===

test(project_manager_entity_exists) :-
    entity(golem(project_manager)).

% === COMPONENT TESTS ===

test(project_manager_delegation_relationships) :-
    component(golem(project_manager), can_delegate_to, golem(architect)),
    component(golem(project_manager), can_delegate_to, golem(semantics_verifier)).

test(project_manager_has_available_tools) :-
    component(golem(project_manager), available_tools, _Tools).

% === END-TO-END TESTS ===

test(project_manager_end_to_end, [condition(e2e_tests_enabled)]) :-
    Input = input{prompt: "Describe a basic Python project structure"},
    magic_cast(conjure(golem_task(golem(project_manager), Input)), Result),
    Result = ok(golem_response(ParsedOutput, _Messages, project_manager, _SessionId)),
    % Use dot notation for dict access
    assertion(ParsedOutput.type = 'ProjectAnalysis'),
    assertion(is_dict(ParsedOutput.structure)),
    assertion(is_list(ParsedOutput.dependencies)),
    assertion(is_list(ParsedOutput.entry_points)),
    assertion(is_list(ParsedOutput.configuration_files)),
    assertion(is_list(ParsedOutput.recommendations)).

:- end_tests('golem(project_manager)').
