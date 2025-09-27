% Tests for Semantics Verifier Golem semantics
% Validates entity declarations, components, and semantic relationships

:- use_module(library(plunit)).

:- begin_tests(semantics_verifier_semantics).

% === ENTITY TESTS ===

test(semantics_verifier_entity_exists) :-
    entity(golem(semantics_verifier)).

% === COMPONENT TESTS ===

test(semantics_verifier_has_output_parser) :-
    component(golem(semantics_verifier), output_parser, parse_semantics_verification).

test(semantics_verifier_delegation_relationships) :-
    component(golem(semantics_verifier), can_delegate_to, golem(test_planner)),
    component(golem(semantics_verifier), can_delegate_to, golem(project_manager)).

test(semantics_verifier_has_available_tools) :-
    component(golem(semantics_verifier), available_tools, _Tools).

% === PARSER TESTS ===

test(parse_semantics_verification_with_full_dict) :-
    Dict = _{
        covered_files: ["src/main.py", "src/utils.py"],
        missing_files: ["src/helper.py"],
        suggestions: ["Add test for helper.py"],
        coverage_percentage: 66.7
    },
    parse_semantics_verification(Dict, semantics_verification(CoveredFiles, MissingFiles, Suggestions, Coverage)),
    CoveredFiles = ["src/main.py", "src/utils.py"],
    MissingFiles = ["src/helper.py"],
    Suggestions = ["Add test for helper.py"],
    Coverage = 66.7.

test(parse_semantics_verification_with_minimal_dict) :-
    Dict = _{
        covered_files: [],
        missing_files: [],
        coverage_percentage: 0.0
    },
    parse_semantics_verification(Dict, semantics_verification(CoveredFiles, MissingFiles, Suggestions, Coverage)),
    CoveredFiles = [],
    MissingFiles = [],
    Suggestions = [],
    Coverage = 0.0.

:- end_tests(semantics_verifier_semantics).