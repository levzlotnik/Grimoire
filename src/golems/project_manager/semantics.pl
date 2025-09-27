% Project Manager Golem
% Senior project manager responsible for coordinating development tasks

:- self_entity(golem(project_manager)).

% Configuration now handled in Python __init__.py
% No longer need config component here since golem is instantiated in Python

% Structured output parser (optional)
component(golem(project_manager), output_parser, parse_project_analysis).

% Parser converts dict to Prolog term (from ProjectAnalysis type)
parse_project_analysis(Dict, project_analysis(Structure, Dependencies, EntryPoints, ConfigFiles, Recommendations)) :-
    get_dict(structure, Dict, Structure),
    get_dict(dependencies, Dict, Dependencies),
    get_dict(entry_points, Dict, EntryPoints),
    get_dict(configuration_files, Dict, ConfigFiles),
    (get_dict(recommendations, Dict, Recommendations) -> true; Recommendations = []).

% Delegation relationships
component(golem(project_manager), can_delegate_to, golem(architect)).
component(golem(project_manager), can_delegate_to, golem(semantics_verifier)).

% Auto-discovered from Golem(Id).tools() through python_bridge
component(golem(project_manager), available_tools, Tools) :-
    get_golem_tools(golem(project_manager), Tools).

% Docstring generated automatically by generic golem docstring rule in semantics.pl