% Code Reviewer Golem
% Performs comprehensive code reviews with security and performance analysis

:- self_entity(golem(code_reviewer)).

% Configuration now handled in Python __init__.py
% No longer need config component here since golem is instantiated in Python

% Structured output parser (optional)
component(golem(code_reviewer), output_parser, parse_code_review).

% Parser converts dict to Prolog term (from CodeReview type)
parse_code_review(Dict, code_review(Issues, Suggestions, Security, Performance, Quality)) :-
    get_dict(issues, Dict, Issues),
    get_dict(suggestions, Dict, Suggestions),
    (get_dict(security_concerns, Dict, Security) -> true; Security = []),
    (get_dict(performance_notes, Dict, Performance) -> true; Performance = []),
    get_dict(overall_quality, Dict, Quality).

% Delegation relationships
component(golem(code_reviewer), can_delegate_to, golem(architect)).
component(golem(code_reviewer), can_delegate_to, golem(test_planner)).

% Auto-discovered from Golem(Id).tools() through python_bridge
component(golem(code_reviewer), available_tools, Tools) :-
    get_golem_tools(golem(code_reviewer), Tools).

% Docstring generated automatically by generic golem docstring rule in semantics.pl