% Documentation Golem
% Technical documentation writer specialized in creating clear, comprehensive docs

:- self_entity(golem(documentation)).

% Role and LLM configuration as dictionary
component(golem(documentation), role, "Technical documentation writer specialized in creating clear, comprehensive documentation, API references, and user guides").
component(golem(documentation), llm_config, _{
    provider: openai,
    model: 'gpt-4-turbo',
    max_tokens: 4096,
    temperature: 0.3
}).

% Input/Output Schema
component(golem(documentation), input, code_or_system(string)).
component(golem(documentation), input, optional(existing_docs(string))).
component(golem(documentation), input, optional(documentation_style(string))).
component(golem(documentation), output, documentation(markdown)).
component(golem(documentation), output, optional(api_reference(structured))).
component(golem(documentation), output, optional(examples(list))).

% Hierarchical relationship
component(golem(documentation), supervisor, golem(project_manager)).

% Auto-discovered from Golem(Id).tools() through python_bridge
component(golem(documentation), available_tools, Tools) :-
    get_golem_tools(golem(documentation), Tools).

% Docstring generated automatically by generic golem docstring rule in semantics.pl