% Code Assistant Golem
% Expert software engineer specialized in code generation, review, and refactoring

:- self_entity(golem(code_assistant)).

% Configuration as dict with model string and output type
component(golem(code_assistant), config, _{
    model: "anthropic:claude-3-5-sonnet-20241022",
    temperature: 0.1,
    max_tokens: 8192,
    system_prompt: "Expert software engineer specialized in code generation, review, and refactoring",
    output_type: "CodeResponse"
}).

% Structured output parser (optional)
component(golem(code_assistant), output_parser, parse_code_response).

% Parser converts dict to Prolog term (from CodeResponse type)
parse_code_response(Dict, code_response(Code, Language, Tests, Docs, Explanation)) :-
    get_dict(code, Dict, Code),
    get_dict(language, Dict, Language),
    (get_dict(tests, Dict, Tests) -> true; Tests = []),
    (get_dict(documentation, Dict, Docs) -> true; Docs = ""),
    (get_dict(explanation, Dict, Explanation) -> true; Explanation = "").

% Delegation relationships
component(golem(code_assistant), can_delegate_to, golem(test_runner)).
component(golem(code_assistant), can_delegate_to, golem(documentation)).

% Auto-discovered from Golem(Id).tools() through python_bridge
component(golem(code_assistant), available_tools, Tools) :-
    get_golem_tools(golem(code_assistant), Tools).

% Docstring generated automatically by generic golem docstring rule in semantics.pl