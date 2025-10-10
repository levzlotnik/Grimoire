% Tests for Documentation Golem semantics
% Validates entity declarations, components, and semantic relationships

:- use_module(library(plunit)).

% Helper predicate: check if end-to-end tests should run
:- multifile e2e_tests_enabled/0.
e2e_tests_enabled :-
    getenv('GRIMOIRE_RUN_E2E_TESTS', '1').

:- begin_tests('golem(documentation)').

% === ENTITY TESTS ===

test(documentation_entity_exists) :-
    entity(golem(documentation)).

% === COMPONENT TESTS ===

test(documentation_delegation_relationships) :-
    component(golem(documentation), can_delegate_to, golem(code_assistant)),
    component(golem(documentation), can_delegate_to, golem(architect)).

test(documentation_has_available_tools) :-
    component(golem(documentation), available_tools, _Tools).

% === END-TO-END TESTS ===

test(documentation_end_to_end, [condition(e2e_tests_enabled)]) :-
    Input = input{prompt: "Document this function: def add(a, b): return a + b"},
    magic_cast(conjure(golem_task(golem(documentation), Input)), Result),
    Result = ok(golem_response(ParsedOutput, _Messages, documentation, _SessionId)),
    % Use dot notation for dict access
    assertion(ParsedOutput.type = 'Documentation'),
    assertion(atom(ParsedOutput.summary)),
    assertion(atom(ParsedOutput.description)),
    assertion(is_list(ParsedOutput.parameters)),
    assertion(atom(ParsedOutput.returns)),
    assertion(is_list(ParsedOutput.examples)).

:- end_tests('golem(documentation)').
