% Architect Golem
% Software architect with expertise in design patterns, system architecture, and best practices

:- self_entity(golem(architect)).

% Configuration now handled in Python __init__.py
% No longer need config component here since golem is instantiated in Python

% Structured output parser (optional)
component(golem(architect), output_parser, parse_architectural_plan).

% Parser converts ArchitecturalPlan Pydantic model to Prolog term
parse_architectural_plan(ArchitecturalPlanObj, architectural_plan(Patterns, Strengths, Weaknesses, Recommendations, Diagram)) :-
    % Extract fields from ArchitecturalPlan Pydantic model using py_call
    py_call(ArchitecturalPlanObj:patterns_used, Patterns),
    py_call(ArchitecturalPlanObj:strengths, Strengths),
    py_call(ArchitecturalPlanObj:weaknesses, Weaknesses),
    py_call(ArchitecturalPlanObj:recommendations, Recommendations),
    py_call(ArchitecturalPlanObj:diagram, Diagram).

% Delegation relationships
component(golem(architect), can_delegate_to, golem(code_reviewer)).
component(golem(architect), can_delegate_to, golem(documentation)).

% Auto-discovered from Golem(Id).tools() through python_bridge
component(golem(architect), available_tools, Tools) :-
    get_golem_tools(golem(architect), Tools).

% Docstring generated automatically by generic golem docstring rule in semantics.pl