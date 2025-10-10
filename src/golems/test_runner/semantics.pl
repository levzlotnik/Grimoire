% Test Runner Golem
% QA engineer focused on testing, test automation, and quality assurance

:- self_entity(golem(test_runner)).

% Output is already a typed dict from Python (via encode_to_prolog_dict)
% No custom parser needed - access fields directly with dot notation (Dict.field)

% Hierarchical relationship
component(golem(test_runner), supervisor, golem(project_manager)).

% Auto-discovered tools
component(golem(test_runner), available_tools, Tools) :-
    get_golem_tools(golem(test_runner), Tools).

% Docstring generated automatically by generic golem docstring rule in parent semantics.pl
