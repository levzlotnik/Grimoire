% Tests for Test Planner Golem semantics
% Validates entity declarations, components, and semantic relationships

:- use_module(library(plunit)).

% Helper predicate: check if end-to-end tests should run
:- multifile e2e_tests_enabled/0.
e2e_tests_enabled :-
    getenv('GRIMOIRE_RUN_E2E_TESTS', '1').

:- begin_tests('golem(test_planner)').

% === ENTITY TESTS ===

test(test_planner_entity_exists) :-
    entity(golem(test_planner)).

% === COMPONENT TESTS ===

test(test_planner_delegation_relationships) :-
    user:please_verify(component(golem(test_planner), can_delegate_to, golem(code_assistant))),
    user:please_verify(component(golem(test_planner), can_delegate_to, golem(semantics_verifier))).

test(test_planner_has_available_tools) :-
    user:please_verify(component(golem(test_planner), available_tools, _Tools)).

% === END-TO-END TESTS ===

test(test_planner_end_to_end, [condition(e2e_tests_enabled)]) :-
    Input = input{prompt: "Create a test plan for: def add(a, b): return a + b"},
    magic_cast(conjure(golem_task(golem(test_planner), Input)), Result),
    Result = ok(golem_response(ParsedOutput, _Messages, test_planner, _SessionId)),
    % Use dot notation for dict access
    assertion(ParsedOutput.type = 'TestPlan'),
    assertion(is_list(ParsedOutput.test_cases)),
    assertion(is_list(ParsedOutput.coverage_areas)),
    assertion(is_list(ParsedOutput.edge_cases)),
    assertion(atom(ParsedOutput.test_strategy)).

:- end_tests('golem(test_planner)').
