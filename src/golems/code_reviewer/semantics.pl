% Code Reviewer Golem
% Performs comprehensive code reviews with security and performance analysis

:- self_entity(golem(code_reviewer)).

% Configuration now handled in Python __init__.py
% No longer need config component here since golem is instantiated in Python

% Output is already a typed dict from Python (via encode_to_prolog_dict)
% No custom parser needed - access fields directly with dot notation (Dict.field)

% Delegation relationships
component(golem(code_reviewer), can_delegate_to, golem(architect)).
component(golem(code_reviewer), can_delegate_to, golem(test_planner)).

% Auto-discovered from Golem(Id).tools() through python_bridge
component(golem(code_reviewer), available_tools, Tools) :-
    get_golem_tools(golem(code_reviewer), Tools).

% Docstring generated automatically by generic golem docstring rule in semantics.pl