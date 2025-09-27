% Project Manager Golem
% Senior project manager responsible for coordinating development tasks

:- self_entity(golem(project_manager)).

% Configuration now handled in Python __init__.py
% No longer need config component here since golem is instantiated in Python

% Structured output parser (optional)
component(golem(project_manager), output_parser, parse_project_analysis).

% Parser converts ProjectAnalysis Pydantic model to Prolog term
parse_project_analysis(ProjectAnalysisObj, project_analysis(Structure, Dependencies, EntryPoints, ConfigFiles, Recommendations)) :-
    % Extract fields from ProjectAnalysis Pydantic model using py_call
    py_call(ProjectAnalysisObj:structure, Structure),
    py_call(ProjectAnalysisObj:dependencies, Dependencies),
    py_call(ProjectAnalysisObj:entry_points, EntryPoints),
    py_call(ProjectAnalysisObj:configuration_files, ConfigFiles),
    py_call(ProjectAnalysisObj:recommendations, Recommendations).

% Delegation relationships
component(golem(project_manager), can_delegate_to, golem(architect)).
component(golem(project_manager), can_delegate_to, golem(semantics_verifier)).

% Auto-discovered from Golem(Id).tools() through python_bridge
component(golem(project_manager), available_tools, Tools) :-
    get_golem_tools(golem(project_manager), Tools).

% Docstring generated automatically by generic golem docstring rule in semantics.pl