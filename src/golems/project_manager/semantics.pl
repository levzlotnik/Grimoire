% Project Manager Golem
% Senior project manager responsible for coordinating development tasks

:- self_entity(golem(project_manager)).

% Configuration now handled in Python __init__.py
% No longer need config component here since golem is instantiated in Python

% Output is already a typed dict from Python (via encode_to_prolog_dict)
% No custom parser needed - access fields directly with dot notation (Dict.field)

% Delegation relationships
component(golem(project_manager), can_delegate_to, golem(architect)).
component(golem(project_manager), can_delegate_to, golem(semantics_verifier)).

% Auto-discovered from Golem(Id).tools() through python_bridge
component(golem(project_manager), available_tools, Tools) :-
    get_golem_tools(golem(project_manager), Tools).

% Docstring generated automatically by generic golem docstring rule in semantics.pl