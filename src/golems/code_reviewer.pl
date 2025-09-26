% Code Reviewer Golem
% Performs comprehensive code reviews with security and performance analysis

:- self_entity(golem(code_reviewer)).

% Configuration with CodeReview output type
component(golem(code_reviewer), config, _{
    model: "anthropic:claude-3-5-sonnet-20241022",
    temperature: 0.3,
    max_tokens: 8192,
    system_prompt: "You are an expert code reviewer specializing in identifying issues, security vulnerabilities, and performance optimizations. Provide constructive feedback and actionable suggestions.",
    output_type: "CodeReview"
}).

% Structured output parser
component(golem(code_reviewer), output_parser, parse_code_review).

% Parser for CodeReview type
parse_code_review(Dict, review(Issues, Suggestions, Security, Performance, Quality)) :-
    get_dict(issues, Dict, Issues),
    get_dict(suggestions, Dict, Suggestions),
    (get_dict(security_concerns, Dict, Security) -> true; Security = []),
    (get_dict(performance_notes, Dict, Performance) -> true; Performance = []),
    get_dict(overall_quality, Dict, Quality).

% Example usage
review_code(FilePath, Review) :-
    cast(conjure(golem_task(
        golem(code_reviewer),
        _{
            task: "Review the following code for quality, security, and performance",
            file_path: FilePath,
            focus_areas: ["security", "performance", "maintainability", "best_practices"]
        },
        ParsedReview
    )), _),
    Review = ParsedReview.