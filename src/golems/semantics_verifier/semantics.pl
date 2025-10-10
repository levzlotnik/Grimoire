% Semantics Verifier Golem
% Verifies that all meaningful source code files are tested in semantics.plt

:- self_entity(golem(semantics_verifier)).

% Configuration now handled in Python __init__.py
% No longer need config component here since golem is instantiated in Python

% Output is already a typed dict from Python (via encode_to_prolog_dict)
% No custom parser needed - access fields directly with dot notation (Dict.field)

% Delegation relationships
component(golem(semantics_verifier), can_delegate_to, golem(test_planner)).
component(golem(semantics_verifier), can_delegate_to, golem(project_manager)).

% Auto-discovered from Golem(Id).tools() through python_bridge
component(golem(semantics_verifier), available_tools, Tools) :-
    get_golem_tools(golem(semantics_verifier), Tools).

% Docstring generated automatically by generic golem docstring rule in semantics.pl