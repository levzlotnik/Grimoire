% Tests for Code Assistant Golem semantics
% Validates entity declarations, components, and semantic relationships

:- use_module(library(plunit)).

% Helper predicate: check if end-to-end tests should run
:- multifile e2e_tests_enabled/0.
e2e_tests_enabled :-
    getenv('GRIMOIRE_RUN_E2E_TESTS', '1').

:- begin_tests('golem(code_assistant)').

% === ENTITY TESTS ===

test(code_assistant_entity_exists) :-
    entity(golem(code_assistant)).

% === COMPONENT TESTS ===

test(code_assistant_delegation_relationships) :-
    user:please_verify(component(golem(code_assistant), can_delegate_to, golem(test_runner))),
    user:please_verify(component(golem(code_assistant), can_delegate_to, golem(documentation))).

test(code_assistant_has_available_tools) :-
    user:please_verify(component(golem(code_assistant), available_tools, _Tools)).

% === END-TO-END TESTS ===

test(code_assistant_end_to_end, [condition(e2e_tests_enabled)]) :-
    Input = input{prompt: "Return code='print(\"hello\")', language='python', tests=[], documentation='test', explanation='test'. Do not use any tools."},
    magic_cast(conjure(golem_task(golem(code_assistant), Input)), Result),
    Result = ok(golem_response(ParsedOutput, _Messages, code_assistant, _SessionId)),
    % Use dot notation for dict access - throws error if key missing
    assertion(ParsedOutput.type = 'CodeResponse'),
    assertion(atom(ParsedOutput.code)),
    assertion(atom(ParsedOutput.language)),
    assertion(is_list(ParsedOutput.tests)).

:- end_tests('golem(code_assistant)').
