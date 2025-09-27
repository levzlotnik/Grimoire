% Documentation Golem
% Technical documentation writer specialized in creating clear, comprehensive docs

:- self_entity(golem(documentation)).

% Configuration now handled in Python __init__.py
% No longer need config component here since golem is instantiated in Python

% Structured output parser (optional)
component(golem(documentation), output_parser, parse_documentation).

% Parser converts dict to Prolog term (from Documentation type)
parse_documentation(Dict, documentation(Summary, Description, Parameters, Returns, Examples)) :-
    get_dict(summary, Dict, Summary),
    get_dict(description, Dict, Description),
    (get_dict(parameters, Dict, Parameters) -> true; Parameters = []),
    (get_dict(returns, Dict, Returns) -> true; Returns = ""),
    (get_dict(examples, Dict, Examples) -> true; Examples = []).

% Delegation relationships
component(golem(documentation), can_delegate_to, golem(code_assistant)).
component(golem(documentation), can_delegate_to, golem(architect)).

% Auto-discovered from Golem(Id).tools() through python_bridge
component(golem(documentation), available_tools, Tools) :-
    get_golem_tools(golem(documentation), Tools).

% Docstring generated automatically by generic golem docstring rule in semantics.pl