% Architect Golem
% Software architect with expertise in design patterns, system architecture, and best practices

:- self_entity(golem(architect)).

% Configuration now handled in Python __init__.py
% No longer need config component here since golem is instantiated in Python

% Output is already a typed dict from Python (via encode_to_prolog_dict)
% No custom parser needed - access fields directly with dot notation (Dict.field)

% Delegation relationships
component(golem(architect), can_delegate_to, golem(code_reviewer)).
component(golem(architect), can_delegate_to, golem(documentation)).

% Auto-discovered from Golem(Id).tools() through python_bridge
component(golem(architect), available_tools, Tools) :-
    get_golem_tools(golem(architect), Tools).

% Docstring generated automatically by generic golem docstring rule in semantics.pl