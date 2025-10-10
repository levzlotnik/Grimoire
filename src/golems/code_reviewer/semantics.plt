% Tests for Code Reviewer Golem semantics
% Validates entity declarations, components, and semantic relationships

:- use_module(library(plunit)).

% Helper predicate: check if end-to-end tests should run
:- multifile e2e_tests_enabled/0.
e2e_tests_enabled :-
    getenv('GRIMOIRE_RUN_E2E_TESTS', '1').

:- begin_tests('golem(code_reviewer)').

% === ENTITY TESTS ===

test(code_reviewer_entity_exists) :-
    entity(golem(code_reviewer)).

% === COMPONENT TESTS ===

test(code_reviewer_delegation_relationships) :-
    component(golem(code_reviewer), can_delegate_to, golem(architect)),
    component(golem(code_reviewer), can_delegate_to, golem(test_planner)).

test(code_reviewer_has_available_tools) :-
    component(golem(code_reviewer), available_tools, _Tools).

% === END-TO-END TESTS ===

test(code_reviewer_end_to_end, [condition(e2e_tests_enabled)]) :-
    Input = input{prompt: "Review this simple code: print('hello')"},
    magic_cast(conjure(golem_task(golem(code_reviewer), Input)), Result),
    Result = ok(golem_response(ParsedOutput, _Messages, code_reviewer, _SessionId)),
    % Use dot notation for dict access
    assertion(ParsedOutput.type = 'CodeReview'),
    assertion(is_list(ParsedOutput.issues)),
    assertion(is_list(ParsedOutput.suggestions)),
    assertion(is_list(ParsedOutput.security_concerns)),
    assertion(is_list(ParsedOutput.performance_notes)),
    assertion(atom(ParsedOutput.overall_quality)).

:- end_tests('golem(code_reviewer)').
