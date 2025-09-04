% Project Manager Golem  
% Senior project manager responsible for coordinating development tasks

:- self_entity(golem(project_manager)).

% Role and LLM configuration
component(golem(project_manager), role, "Senior project manager responsible for coordinating development tasks and ensuring project quality").
component(golem(project_manager), llm_config, _{
    provider: openai,
    model: 'gpt-4-turbo',
    max_tokens: 4096,
    temperature: 0.2
}).

% Input/Output Schema for project management
component(golem(project_manager), input, project_description(string)).
component(golem(project_manager), input, optional(timeline_constraints(dict))).
component(golem(project_manager), input, optional(team_members(list))).
component(golem(project_manager), output, project_plan(structured_plan)).
component(golem(project_manager), output, task_assignments(list)).

% Delegation hierarchy
component(golem(project_manager), can_delegate_to, golem(code_assistant)).
component(golem(project_manager), can_delegate_to, golem(test_runner)).
component(golem(project_manager), can_delegate_to, golem(documentation)).

% Auto-discovered tools
component(golem(project_manager), available_tools, Tools) :-
    get_golem_tools(golem(project_manager), Tools).

% Dynamic docstring
docstring(golem(project_manager), DocString) :-
    component(golem(project_manager), role, Role),
    component(golem(project_manager), llm_config, Config),
    component(golem(project_manager), available_tools, Tools),
    findall(input(I), component(golem(project_manager), input, I), Inputs),
    findall(output(O), component(golem(project_manager), output, O), Outputs),
    format_golem_docstring(Role, Config, Tools, Inputs, Outputs, DocString).