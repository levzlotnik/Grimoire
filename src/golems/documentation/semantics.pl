% Documentation Golem
% Technical documentation writer specialized in creating clear, comprehensive docs

:- self_entity(golem(documentation)).

% Configuration now handled in Python __init__.py
% No longer need config component here since golem is instantiated in Python

% Structured output parser (optional)
component(golem(documentation), output_parser, parse_documentation).

% Parser converts Documentation Pydantic model to Prolog term
parse_documentation(DocumentationObj, documentation(Summary, Description, Parameters, Returns, Examples)) :-
    % Extract fields from Documentation Pydantic model using py_call
    py_call(DocumentationObj:summary, Summary),
    py_call(DocumentationObj:description, Description),
    py_call(DocumentationObj:parameters, Parameters),
    py_call(DocumentationObj:returns, Returns),
    py_call(DocumentationObj:examples, Examples).

% Delegation relationships
component(golem(documentation), can_delegate_to, golem(code_assistant)).
component(golem(documentation), can_delegate_to, golem(architect)).

% Auto-discovered from Golem(Id).tools() through python_bridge
component(golem(documentation), available_tools, Tools) :-
    get_golem_tools(golem(documentation), Tools).

% Docstring generated automatically by generic golem docstring rule in semantics.pl