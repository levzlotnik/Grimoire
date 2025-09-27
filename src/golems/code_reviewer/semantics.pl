% Code Reviewer Golem
% Performs comprehensive code reviews with security and performance analysis

:- self_entity(golem(code_reviewer)).

% Configuration now handled in Python __init__.py
% No longer need config component here since golem is instantiated in Python

% Structured output parser (optional)
component(golem(code_reviewer), output_parser, parse_code_review).

% Parser converts CodeReview Pydantic model to Prolog term
parse_code_review(CodeReviewObj, code_review(Issues, Suggestions, Security, Performance, Quality)) :-
    % Extract fields from CodeReview Pydantic model using py_call
    py_call(CodeReviewObj:issues, Issues),
    py_call(CodeReviewObj:suggestions, Suggestions),
    py_call(CodeReviewObj:security_concerns, Security),
    py_call(CodeReviewObj:performance_notes, Performance),
    py_call(CodeReviewObj:overall_quality, Quality).

% Delegation relationships
component(golem(code_reviewer), can_delegate_to, golem(architect)).
component(golem(code_reviewer), can_delegate_to, golem(test_planner)).

% Auto-discovered from Golem(Id).tools() through python_bridge
component(golem(code_reviewer), available_tools, Tools) :-
    get_golem_tools(golem(code_reviewer), Tools).

% Docstring generated automatically by generic golem docstring rule in semantics.pl