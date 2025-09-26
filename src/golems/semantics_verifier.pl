% Semantics Verifier Golem
% Verifies that all meaningful source code files are tested in semantics.plt

:- self_entity(golem(semantics_verifier)).

% Configuration with Anthropic Claude and SemanticsVerification output type
component(golem(semantics_verifier), config, _{
    model: "anthropic:claude-3-5-sonnet-20241022",
    temperature: 0.2,
    max_tokens: 4096,
    system_prompt: "You are a code coverage analyzer specializing in verifying that all meaningful source code files within a project are explicitly tested. You analyze directory structures and ensure proper test coverage in semantics.plt files.",
    output_type: "SemanticsVerification"
}).

% Structured output parser
component(golem(semantics_verifier), output_parser, parse_verification_result).

% Parser converts dict to Prolog term (from SemanticsVerification type)
parse_verification_result(Dict, verification_result(Covered, Missing, Suggestions, Coverage)) :-
    get_dict(covered_files, Dict, Covered),
    get_dict(missing_files, Dict, Missing),
    (get_dict(suggestions, Dict, Suggestions) -> true; Suggestions = []),
    get_dict(coverage_percentage, Dict, Coverage).

% Delegation relationships
component(golem(semantics_verifier), can_delegate_to, golem(code_assistant)).
component(golem(semantics_verifier), can_delegate_to, golem(test_runner)).

% Auto-discovered tools from Python bridge
component(golem(semantics_verifier), available_tools, Tools) :-
    get_golem_tools(golem(semantics_verifier), Tools).

% Example usage predicate
verify_semantics_coverage(ProjectPath, Result) :-
    cast(conjure(golem_task(
        golem(semantics_verifier),
        _{
            task: "Traverse the directory structure and verify all meaningful source files are tested in semantics.plt",
            project_path: ProjectPath,
            instructions: "List all source code files that should be tested, check semantics.plt for coverage, and identify any missing files"
        },
        ParsedResult
    )), OutputDict),
    Result = ParsedResult.