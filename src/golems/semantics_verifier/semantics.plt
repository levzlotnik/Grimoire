% Tests for Semantics Verifier Golem semantics
% Validates entity declarations, components, and semantic relationships

:- use_module(library(plunit)).

% Helper predicate: check if end-to-end tests should run
:- multifile e2e_tests_enabled/0.
e2e_tests_enabled :-
    getenv('GRIMOIRE_RUN_E2E_TESTS', '1').

:- begin_tests('golem(semantics_verifier)').

% === ENTITY TESTS ===

test(semantics_verifier_entity_exists) :-
    entity(golem(semantics_verifier)).

% === COMPONENT TESTS ===

test(semantics_verifier_delegation_relationships) :-
    user:please_verify(component(golem(semantics_verifier), can_delegate_to, golem(test_planner))),
    user:please_verify(component(golem(semantics_verifier), can_delegate_to, golem(project_manager))).

test(semantics_verifier_has_available_tools) :-
    user:please_verify(component(golem(semantics_verifier), available_tools, _Tools)).

% === END-TO-END TESTS ===

test(semantics_verifier_end_to_end, [condition(e2e_tests_enabled)]) :-
    Input = input{prompt: "Check semantic coverage for: main.py"},
    magic_cast(conjure(golem_task(golem(semantics_verifier), Input)), Result),
    Result = ok(golem_response(ParsedOutput, _Messages, semantics_verifier, _SessionId)),
    % Use dot notation for dict access
    assertion(ParsedOutput.type = 'SemanticsVerification'),
    assertion(is_list(ParsedOutput.covered_files)),
    assertion(is_list(ParsedOutput.missing_files)),
    assertion(is_list(ParsedOutput.suggestions)),
    assertion(number(ParsedOutput.coverage_percentage)).

:- end_tests('golem(semantics_verifier)').
