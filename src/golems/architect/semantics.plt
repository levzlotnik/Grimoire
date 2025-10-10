% Tests for Architect Golem semantics
% Validates entity declarations, components, and semantic relationships

:- use_module(library(plunit)).

% Helper predicate: check if end-to-end tests should run
:- multifile e2e_tests_enabled/0.
e2e_tests_enabled :-
    getenv('GRIMOIRE_RUN_E2E_TESTS', '1').

:- begin_tests('golem(architect)').

% === ENTITY TESTS ===

test(architect_entity_exists) :-
    entity(golem(architect)).

% === COMPONENT TESTS ===

test(architect_delegation_relationships) :-
    component(golem(architect), can_delegate_to, golem(code_reviewer)),
    component(golem(architect), can_delegate_to, golem(documentation)).

test(architect_has_available_tools) :-
    component(golem(architect), available_tools, _Tools).

% === END-TO-END TESTS ===

test(architect_end_to_end, [condition(e2e_tests_enabled)]) :-
    Input = input{prompt: "Describe a simple client-server web application architecture"},
    magic_cast(conjure(golem_task(golem(architect), Input)), Result),
    Result = ok(golem_response(ParsedOutput, _Messages, architect, _SessionId)),
    % Use dot notation for dict access
    assertion(ParsedOutput.type = 'ArchitecturalPlan'),
    assertion(is_list(ParsedOutput.patterns_used)),
    assertion(is_list(ParsedOutput.strengths)),
    assertion(is_list(ParsedOutput.weaknesses)),
    assertion(is_list(ParsedOutput.recommendations)),
    assertion(atom(ParsedOutput.diagram)).

:- end_tests('golem(architect)').
