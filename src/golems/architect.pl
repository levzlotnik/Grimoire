% Architect Golem
% Analyzes and designs software architecture

:- self_entity(golem(architect)).

% Configuration with ArchitectureReview output type
component(golem(architect), config, _{
    model: "anthropic:claude-sonnet-4-20250514",
    temperature: 0.5,
    max_tokens: 8192,
    system_prompt: "You are a software architect with expertise in design patterns, system architecture, and best practices. Analyze architectures and provide strategic recommendations.",
    output_type: "ArchitectureReview"
}).

% Parser for architecture review
component(golem(architect), output_parser, parse_architecture_review).

parse_architecture_review(Dict, architecture(Patterns, Strengths, Weaknesses, Recommendations, Diagram)) :-
    get_dict(patterns_used, Dict, Patterns),
    get_dict(strengths, Dict, Strengths),
    get_dict(weaknesses, Dict, Weaknesses),
    get_dict(recommendations, Dict, Recommendations),
    (get_dict(diagram, Dict, Diagram) -> true; Diagram = "").

% Refactoring specialist variant
component(golem(refactoring_specialist), config, _{
    model: "anthropic:claude-sonnet-4-20250514",
    temperature: 0.2,
    max_tokens: 6000,
    system_prompt: "You are a refactoring specialist. Identify code that needs refactoring and create detailed refactoring plans with risk assessments.",
    output_type: "RefactoringPlan"
}).

component(golem(refactoring_specialist), output_parser, parse_refactoring_plan).

parse_refactoring_plan(Dict, refactoring(Targets, Strategy, Steps, Risk, Benefits)) :-
    get_dict(targets, Dict, Targets),
    get_dict(strategy, Dict, Strategy),
    get_dict(steps, Dict, Steps),
    get_dict(risk_assessment, Dict, Risk),
    get_dict(expected_benefits, Dict, Benefits).