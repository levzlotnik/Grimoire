% Tests for Test Planner Golem semantics
% Validates entity declarations, components, and semantic relationships

:- use_module(library(plunit)).

:- begin_tests(test_planner_semantics).

% === ENTITY TESTS ===

test(test_planner_entity_exists) :-
    entity(golem(test_planner)).

% === COMPONENT TESTS ===

test(test_planner_has_output_parser) :-
    component(golem(test_planner), output_parser, parse_test_plan).

test(test_planner_delegation_relationships) :-
    component(golem(test_planner), can_delegate_to, golem(code_assistant)),
    component(golem(test_planner), can_delegate_to, golem(semantics_verifier)).

test(test_planner_has_available_tools) :-
    component(golem(test_planner), available_tools, _Tools).

% === PARSER TESTS ===

test(parse_test_plan_with_full_dict) :-
    Dict = _{
        test_cases: ["test_valid_input", "test_invalid_input"],
        coverage_areas: ["input validation", "error handling"],
        edge_cases: ["empty input", "null values"],
        test_strategy: "Unit tests with mocking"
    },
    parse_test_plan(Dict, test_plan(TestCases, CoverageAreas, EdgeCases, Strategy)),
    TestCases = ["test_valid_input", "test_invalid_input"],
    CoverageAreas = ["input validation", "error handling"],
    EdgeCases = ["empty input", "null values"],
    Strategy = "Unit tests with mocking".

test(parse_test_plan_with_minimal_dict) :-
    Dict = _{
        test_cases: [],
        coverage_areas: [],
        edge_cases: [],
        test_strategy: "Basic testing"
    },
    parse_test_plan(Dict, test_plan(TestCases, CoverageAreas, EdgeCases, Strategy)),
    TestCases = [],
    CoverageAreas = [],
    EdgeCases = [],
    Strategy = "Basic testing".

:- end_tests(test_planner_semantics).