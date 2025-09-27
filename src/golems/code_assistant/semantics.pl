% Code Assistant Golem
% Expert software engineer specialized in code generation, review, and refactoring

:- self_entity(golem(code_assistant)).

% Configuration now handled in Python __init__.py
% No longer need config component here since golem is instantiated in Python

% Structured output parser (optional)
component(golem(code_assistant), output_parser, parse_code_response).

% Parser converts CodeResponse Pydantic model to Prolog term
parse_code_response(CodeResponseObj, code_response(Code, Language, Tests, Docs, Explanation)) :-
    % Extract fields from CodeResponse Pydantic model using py_call
    py_call(CodeResponseObj:code, Code),
    py_call(CodeResponseObj:language, Language),
    py_call(CodeResponseObj:tests, Tests),
    py_call(CodeResponseObj:documentation, Docs),
    py_call(CodeResponseObj:explanation, Explanation).

% Delegation relationships
component(golem(code_assistant), can_delegate_to, golem(test_runner)).
component(golem(code_assistant), can_delegate_to, golem(documentation)).

% Auto-discovered from Golem(Id).tools() through python_bridge
component(golem(code_assistant), available_tools, Tools) :-
    get_golem_tools(golem(code_assistant), Tools).

% Docstring generated automatically by generic golem docstring rule in semantics.pl