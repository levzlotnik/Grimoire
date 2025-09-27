% Test Planner Golem
% Creates comprehensive test plans for code

:- self_entity(golem(test_planner)).

% Configuration with TestPlan output type
component(golem(test_planner), config, _{
    model: "openai:gpt-5-mini",
    temperature: 0.4,
    max_tokens: 6000,
    system_prompt: "You are a test planning expert. Create comprehensive test plans that cover unit tests, integration tests, and edge cases. Focus on high code coverage and meaningful test scenarios.",
    output_type: "TestPlan"
}).

% Structured output parser
component(golem(test_planner), output_parser, parse_test_plan).

% Parser for TestPlan type
parse_test_plan(Dict, test_plan(Cases, Coverage, EdgeCases, Strategy)) :-
    get_dict(test_cases, Dict, Cases),
    get_dict(coverage_areas, Dict, Coverage),
    get_dict(edge_cases, Dict, EdgeCases),
    get_dict(test_strategy, Dict, Strategy).

% Example usage for Ollama
component(golem(test_planner_local), config, _{
    model: "codellama:13b",
    base_url: "http://localhost:11434/v1",
    temperature: 0.3,
    max_tokens: 4096,
    system_prompt: "You are a test planning expert using local models.",
    output_type: "TestPlan"
}).