% Test Planner Golem
% Creates comprehensive test plans for code with coverage analysis and edge case detection

:- self_entity(golem(test_planner)).

% Configuration now handled in Python __init__.py
% No longer need config component here since golem is instantiated in Python

% Structured output parser (optional)
component(golem(test_planner), output_parser, parse_test_plan).

% Parser converts dict to Prolog term (from TestPlan type)
parse_test_plan(Dict, test_plan(TestCases, CoverageAreas, EdgeCases, Strategy)) :-
    get_dict(test_cases, Dict, TestCases),
    get_dict(coverage_areas, Dict, CoverageAreas),
    get_dict(edge_cases, Dict, EdgeCases),
    get_dict(test_strategy, Dict, Strategy).

% Delegation relationships
component(golem(test_planner), can_delegate_to, golem(code_assistant)).
component(golem(test_planner), can_delegate_to, golem(semantics_verifier)).

% Auto-discovered from Golem(Id).tools() through python_bridge
component(golem(test_planner), available_tools, Tools) :-
    get_golem_tools(golem(test_planner), Tools).

% Docstring generated automatically by generic golem docstring rule in semantics.pl