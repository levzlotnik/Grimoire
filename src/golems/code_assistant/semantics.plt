% Tests for Code Assistant Golem semantics
% Validates entity declarations, components, and semantic relationships

:- use_module(library(plunit)).

:- begin_tests(code_assistant_semantics).

% === ENTITY TESTS ===

test(code_assistant_entity_exists) :-
    entity(golem(code_assistant)).

% === COMPONENT TESTS ===

test(code_assistant_has_output_parser) :-
    component(golem(code_assistant), output_parser, parse_code_response).

test(code_assistant_delegation_relationships) :-
    component(golem(code_assistant), can_delegate_to, golem(test_runner)),
    component(golem(code_assistant), can_delegate_to, golem(documentation)).

test(code_assistant_has_available_tools) :-
    component(golem(code_assistant), available_tools, _Tools).

% === PARSER TESTS ===

test(parse_code_response_with_full_dict) :-
    Dict = _{
        code: "print('hello')",
        language: "python",
        tests: ["test_hello()"],
        documentation: "Simple hello function",
        explanation: "Prints hello to stdout"
    },
    parse_code_response(Dict, code_response(Code, Language, Tests, Docs, Explanation)),
    Code = "print('hello')",
    Language = "python",
    Tests = ["test_hello()"],
    Docs = "Simple hello function",
    Explanation = "Prints hello to stdout".

test(parse_code_response_with_minimal_dict) :-
    Dict = _{
        code: "x = 1",
        language: "python"
    },
    parse_code_response(Dict, code_response(Code, Language, Tests, Docs, Explanation)),
    Code = "x = 1",
    Language = "python",
    Tests = [],
    Docs = "",
    Explanation = "".

:- end_tests(code_assistant_semantics).