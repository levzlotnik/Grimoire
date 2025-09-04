% Code Assistant Golem
% Expert software engineer specialized in code generation, review, and refactoring

:- self_entity(golem(code_assistant)).

% Role and LLM configuration as dictionary
component(golem(code_assistant), role, "Expert software engineer specialized in code generation, review, and refactoring").
component(golem(code_assistant), llm_config, _{
    provider: anthropic,
    model: 'claude-3-5-sonnet-20241022',
    max_tokens: 8192,
    temperature: 0.1
}).

% Input/Output Schema
component(golem(code_assistant), input, task_description(string)).
component(golem(code_assistant), input, optional(file_context(list))).
component(golem(code_assistant), input, optional(requirements(list))).
component(golem(code_assistant), output, code_response(generated_code)).
component(golem(code_assistant), output, optional(test_suggestions(list))).

% Delegation relationships
component(golem(code_assistant), can_delegate_to, golem(test_runner)).
component(golem(code_assistant), can_delegate_to, golem(documentation)).

% Auto-discovered from Golem(Id).tools() through python_bridge
component(golem(code_assistant), available_tools, Tools) :-
    get_golem_tools(golem(code_assistant), Tools).

% Docstring generated automatically by generic golem docstring rule in semantics.pl