% Test Runner Golem Tests
% Tests for the test runner golem entity and components

% Helper predicate: check if end-to-end tests should run
:- multifile e2e_tests_enabled/0.
e2e_tests_enabled :-
    getenv('GRIMOIRE_RUN_E2E_TESTS', '1').

:- begin_tests('golem(test_runner)').

% Test that test_runner golem entity exists
test(test_runner_entity_exists) :-
    entity(golem(test_runner)).

% Test that test_runner has supervisor relationship
test(test_runner_has_supervisor) :-
    component(golem(test_runner), supervisor, golem(project_manager)).

% === END-TO-END TESTS ===

test(test_runner_end_to_end, [condition(e2e_tests_enabled)]) :-
    Input = input{prompt: "Analyze these test results: 5 passed, 2 failed"},
    magic_cast(conjure(golem_task(golem(test_runner), Input)), Result),
    Result = ok(golem_response(ParsedOutput, _Messages, test_runner, _SessionId)),
    % Use dot notation for dict access
    assertion(ParsedOutput.type = 'TestResult'),
    assertion(number(ParsedOutput.passed)),
    assertion(number(ParsedOutput.failed)),
    assertion(number(ParsedOutput.skipped)).

:- end_tests('golem(test_runner)').
