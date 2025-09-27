% Test Runner Golem Tests
% Tests for the test runner golem entity and components

:- begin_tests(golem_test_runner).

% Test that test_runner golem entity exists
test(test_runner_entity_exists) :-
    entity(golem(test_runner)).

% Test that test_runner has supervisor relationship
test(test_runner_has_supervisor) :-
    component(golem(test_runner), supervisor, golem(project_manager)).

% Test that test_runner has output parser
test(test_runner_has_output_parser) :-
    component(golem(test_runner), output_parser, parse_test_result).

% Test output parser functionality
test(test_runner_parser_works) :-
    Dict = _{
        passed: 10,
        failed: 2,
        skipped: 1,
        failures: [_{test: "test_foo", error: "assertion failed"}],
        coverage: 85.5
    },
    parse_test_result(Dict, test_result(10, 2, 1, [_{test: "test_foo", error: "assertion failed"}], 85.5)).

% Test output parser with minimal data
test(test_runner_parser_minimal) :-
    Dict = _{
        passed: 5,
        failed: 0,
        skipped: 0
    },
    parse_test_result(Dict, test_result(5, 0, 0, [], 0.0)).

:- end_tests(golem_test_runner).