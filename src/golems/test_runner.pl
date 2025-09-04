% Test Runner Golem
% QA engineer focused on testing, test automation, and quality assurance

:- self_entity(golem(test_runner)).

% Role and LLM configuration
component(golem(test_runner), role, "QA engineer focused on testing, test automation, and quality assurance").
component(golem(test_runner), llm_config, _{
    provider: ollama,
    model: 'llama-3.1-70b',
    max_tokens: 4096,
    temperature: 0.0
}).

% Input/Output Schema for testing
component(golem(test_runner), input, test_target(string)).
component(golem(test_runner), input, optional(test_types(list))).
component(golem(test_runner), input, optional(coverage_requirements(dict))).
component(golem(test_runner), output, test_results(test_report)).
component(golem(test_runner), output, coverage_report(coverage_data)).
component(golem(test_runner), output, optional(performance_metrics(dict))).

% Hierarchical relationship
component(golem(test_runner), supervisor, golem(project_manager)).

% Auto-discovered tools
component(golem(test_runner), available_tools, Tools) :-
    get_golem_tools(golem(test_runner), Tools).

% Dynamic docstring
docstring(golem(test_runner), DocString) :-
    component(golem(test_runner), role, Role),
    component(golem(test_runner), llm_config, Config),
    component(golem(test_runner), available_tools, Tools),
    findall(input(I), component(golem(test_runner), input, I), Inputs),
    findall(output(O), component(golem(test_runner), output, O), Outputs),
    format_golem_docstring(Role, Config, Tools, Inputs, Outputs, DocString).