% Documentation Golem
% Technical documentation writer specialized in creating clear, comprehensive docs

:- self_entity(golem(documentation)).

% Configuration now handled in Python __init__.py
% No longer need config component here since golem is instantiated in Python

% Output is already a typed dict from Python (via encode_to_prolog_dict)
% No custom parser needed - access fields directly with dot notation (Dict.field)

% Delegation relationships
component(golem(documentation), can_delegate_to, golem(code_assistant)).
component(golem(documentation), can_delegate_to, golem(architect)).

% Auto-discovered from Golem(Id).tools() through python_bridge
component(golem(documentation), available_tools, Tools) :-
    get_golem_tools(golem(documentation), Tools).

% Docstring generated automatically by generic golem docstring rule in semantics.pl