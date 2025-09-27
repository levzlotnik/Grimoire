% Tests for Architect Golem semantics
% Validates entity declarations, components, and semantic relationships

:- use_module(library(plunit)).

:- begin_tests(architect_semantics).

% === ENTITY TESTS ===

test(architect_entity_exists) :-
    entity(golem(architect)).

% === COMPONENT TESTS ===

test(architect_has_output_parser) :-
    component(golem(architect), output_parser, parse_architectural_plan).

test(architect_delegation_relationships) :-
    component(golem(architect), can_delegate_to, golem(code_reviewer)),
    component(golem(architect), can_delegate_to, golem(documentation)).

test(architect_has_available_tools) :-
    component(golem(architect), available_tools, _Tools).

% === PARSER TESTS ===

test(parse_architectural_plan_with_full_dict) :-
    Dict = _{
        patterns_used: ["MVC", "Observer"],
        strengths: ["Good separation of concerns"],
        weaknesses: ["Tight coupling in data layer"],
        recommendations: ["Implement dependency injection"],
        diagram: "ASCII diagram here"
    },
    parse_architectural_plan(Dict, architectural_plan(Patterns, Strengths, Weaknesses, Recommendations, Diagram)),
    Patterns = ["MVC", "Observer"],
    Strengths = ["Good separation of concerns"],
    Weaknesses = ["Tight coupling in data layer"],
    Recommendations = ["Implement dependency injection"],
    Diagram = "ASCII diagram here".

test(parse_architectural_plan_with_minimal_dict) :-
    Dict = _{
        patterns_used: [],
        strengths: [],
        weaknesses: [],
        recommendations: []
    },
    parse_architectural_plan(Dict, architectural_plan(Patterns, Strengths, Weaknesses, Recommendations, Diagram)),
    Patterns = [],
    Strengths = [],
    Weaknesses = [],
    Recommendations = [],
    Diagram = "".

:- end_tests(architect_semantics).