% Tests for Documentation Golem semantics
% Validates entity declarations, components, and semantic relationships

:- use_module(library(plunit)).

:- begin_tests(documentation_semantics).

% === ENTITY TESTS ===

test(documentation_entity_exists) :-
    entity(golem(documentation)).

% === COMPONENT TESTS ===

test(documentation_has_output_parser) :-
    component(golem(documentation), output_parser, parse_documentation).

test(documentation_delegation_relationships) :-
    component(golem(documentation), can_delegate_to, golem(code_assistant)),
    component(golem(documentation), can_delegate_to, golem(architect)).

test(documentation_has_available_tools) :-
    component(golem(documentation), available_tools, _Tools).

% === PARSER TESTS ===

test(parse_documentation_with_full_dict) :-
    Dict = _{
        summary: "Function calculates total",
        description: "Detailed calculation method",
        parameters: ["amount: float", "tax_rate: float"],
        returns: "Total amount with tax",
        examples: ["calculate_total(100, 0.08)"]
    },
    parse_documentation(Dict, documentation(Summary, Description, Parameters, Returns, Examples)),
    Summary = "Function calculates total",
    Description = "Detailed calculation method",
    Parameters = ["amount: float", "tax_rate: float"],
    Returns = "Total amount with tax",
    Examples = ["calculate_total(100, 0.08)"].

test(parse_documentation_with_minimal_dict) :-
    Dict = _{
        summary: "Basic function",
        description: "Does something"
    },
    parse_documentation(Dict, documentation(Summary, Description, Parameters, Returns, Examples)),
    Summary = "Basic function",
    Description = "Does something",
    Parameters = [],
    Returns = "",
    Examples = [].

:- end_tests(documentation_semantics).