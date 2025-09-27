% Architect Golem
% Software architect with expertise in design patterns, system architecture, and best practices

:- self_entity(golem(architect)).

% Configuration now handled in Python __init__.py
% No longer need config component here since golem is instantiated in Python

% Structured output parser (optional)
component(golem(architect), output_parser, parse_architectural_plan).

% Parser converts dict to Prolog term (from ArchitecturalPlan type)
parse_architectural_plan(Dict, architectural_plan(Patterns, Strengths, Weaknesses, Recommendations, Diagram)) :-
    get_dict(patterns_used, Dict, Patterns),
    get_dict(strengths, Dict, Strengths),
    get_dict(weaknesses, Dict, Weaknesses),
    get_dict(recommendations, Dict, Recommendations),
    (get_dict(diagram, Dict, Diagram) -> true; Diagram = "").

% Delegation relationships
component(golem(architect), can_delegate_to, golem(code_reviewer)).
component(golem(architect), can_delegate_to, golem(documentation)).

% Auto-discovered from Golem(Id).tools() through python_bridge
component(golem(architect), available_tools, Tools) :-
    get_golem_tools(golem(architect), Tools).

% Docstring generated automatically by generic golem docstring rule in semantics.pl