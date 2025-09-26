% Documentation Golem
% Technical documentation writer specialized in creating clear, comprehensive docs

:- self_entity(golem(documentation)).

% Configuration with Documentation output type
component(golem(documentation), config, _{
    model: "openai:gpt-4o",
    temperature: 0.3,
    max_tokens: 4096,
    system_prompt: "Technical documentation writer specialized in creating clear, comprehensive documentation, API references, and user guides",
    output_type: "Documentation"
}).

% Structured output parser
component(golem(documentation), output_parser, parse_documentation).

parse_documentation(Dict, documentation(Summary, Description, Parameters, Returns, Examples)) :-
    get_dict(summary, Dict, Summary),
    get_dict(description, Dict, Description),
    (get_dict(parameters, Dict, Parameters) -> true; Parameters = []),
    (get_dict(returns, Dict, Returns) -> true; Returns = ""),
    (get_dict(examples, Dict, Examples) -> true; Examples = []).

% Hierarchical relationship
component(golem(documentation), supervisor, golem(project_manager)).

% Auto-discovered from Golem(Id).tools() through python_bridge
component(golem(documentation), available_tools, Tools) :-
    get_golem_tools(golem(documentation), Tools).

% Docstring generated automatically by generic golem docstring rule in semantics.pl