% Test Runner Golem
% QA engineer focused on testing, test automation, and quality assurance

:- self_entity(golem(test_runner)).

% Configuration with TestResult output type using local Ollama
component(golem(test_runner), config, _{
    model: "llama-3.1:8b",
    base_url: "http://localhost:11434/v1",
    temperature: 0.0,
    max_tokens: 4096,
    system_prompt: "QA engineer focused on testing, test automation, and quality assurance",
    output_type: "TestResult"
}).

% Structured output parser
component(golem(test_runner), output_parser, parse_test_result).

parse_test_result(Dict, test_result(Passed, Failed, Skipped, Failures, Coverage)) :-
    get_dict(passed, Dict, Passed),
    get_dict(failed, Dict, Failed),
    get_dict(skipped, Dict, Skipped),
    (get_dict(failures, Dict, Failures) -> true; Failures = []),
    (get_dict(coverage, Dict, Coverage) -> true; Coverage = 0.0).

% Hierarchical relationship
component(golem(test_runner), supervisor, golem(project_manager)).

% Auto-discovered tools
component(golem(test_runner), available_tools, Tools) :-
    get_golem_tools(golem(test_runner), Tools).

% Docstring generated automatically by generic golem docstring rule in semantics.pl