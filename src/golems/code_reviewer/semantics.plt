% Tests for Code Reviewer Golem semantics
% Validates entity declarations, components, and semantic relationships

:- use_module(library(plunit)).

:- begin_tests(code_reviewer_semantics).

% === ENTITY TESTS ===

test(code_reviewer_entity_exists) :-
    entity(golem(code_reviewer)).

% === COMPONENT TESTS ===

test(code_reviewer_has_output_parser) :-
    component(golem(code_reviewer), output_parser, parse_code_review).

test(code_reviewer_delegation_relationships) :-
    component(golem(code_reviewer), can_delegate_to, golem(architect)),
    component(golem(code_reviewer), can_delegate_to, golem(test_planner)).

test(code_reviewer_has_available_tools) :-
    component(golem(code_reviewer), available_tools, _Tools).

% === PARSER TESTS ===

test(parse_code_review_with_full_dict) :-
    Dict = _{
        issues: ["Variable naming inconsistent"],
        suggestions: ["Use descriptive variable names"],
        security_concerns: ["SQL injection risk"],
        performance_notes: ["Consider caching"],
        overall_quality: "Good with minor issues"
    },
    parse_code_review(Dict, code_review(Issues, Suggestions, Security, Performance, Quality)),
    Issues = ["Variable naming inconsistent"],
    Suggestions = ["Use descriptive variable names"],
    Security = ["SQL injection risk"],
    Performance = ["Consider caching"],
    Quality = "Good with minor issues".

test(parse_code_review_with_minimal_dict) :-
    Dict = _{
        issues: [],
        suggestions: [],
        overall_quality: "Excellent"
    },
    parse_code_review(Dict, code_review(Issues, Suggestions, Security, Performance, Quality)),
    Issues = [],
    Suggestions = [],
    Security = [],
    Performance = [],
    Quality = "Excellent".

:- end_tests(code_reviewer_semantics).