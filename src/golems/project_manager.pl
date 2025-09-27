% Project Manager Golem  
% Senior project manager responsible for coordinating development tasks

:- self_entity(golem(project_manager)).

% Configuration with ProjectAnalysis output type
component(golem(project_manager), config, _{
    model: "openai:gpt-5-mini",
    temperature: 0.2,
    max_tokens: 4096,
    system_prompt: "Senior project manager responsible for coordinating development tasks and ensuring project quality",
    output_type: "ProjectAnalysis"
}).

% Structured output parser
component(golem(project_manager), output_parser, parse_project_analysis).

parse_project_analysis(Dict, project_analysis(Structure, Dependencies, EntryPoints, Config, Recommendations)) :-
    get_dict(structure, Dict, Structure),
    get_dict(dependencies, Dict, Dependencies),
    get_dict(entry_points, Dict, EntryPoints),
    get_dict(configuration_files, Dict, Config),
    (get_dict(recommendations, Dict, Recommendations) -> true; Recommendations = []).

% Delegation hierarchy
component(golem(project_manager), can_delegate_to, golem(code_assistant)).
component(golem(project_manager), can_delegate_to, golem(test_runner)).
component(golem(project_manager), can_delegate_to, golem(documentation)).

% Auto-discovered tools
component(golem(project_manager), available_tools, Tools) :-
    get_golem_tools(golem(project_manager), Tools).

% Docstring generated automatically by generic golem docstring rule in semantics.pl