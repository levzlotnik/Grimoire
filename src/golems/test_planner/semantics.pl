% Test Planner Golem
% Creates comprehensive test plans for code with coverage analysis and edge case detection

:- self_entity(golem(test_planner)).

% Configuration now handled in Python __init__.py
% No longer need config component here since golem is instantiated in Python

% Output is already a typed dict from Python (via encode_to_prolog_dict)
% No custom parser needed - access fields directly with dot notation (Dict.field)

% Delegation relationships
component(golem(test_planner), can_delegate_to, golem(code_assistant)).
component(golem(test_planner), can_delegate_to, golem(semantics_verifier)).

% Auto-discovered from Golem(Id).tools() through python_bridge
component(golem(test_planner), available_tools, Tools) :-
    get_golem_tools(golem(test_planner), Tools).

% Docstring generated automatically by generic golem docstring rule in semantics.pl