% Semantics Verifier Golem
% Verifies that all meaningful source code files are tested in semantics.plt

:- self_entity(golem(semantics_verifier)).

% Configuration now handled in Python __init__.py
% No longer need config component here since golem is instantiated in Python

% Structured output parser (optional)
component(golem(semantics_verifier), output_parser, parse_semantics_verification).

% Parser converts SemanticsVerification Pydantic model to Prolog term
parse_semantics_verification(SemanticsVerificationObj, semantics_verification(CoveredFiles, MissingFiles, Suggestions, Coverage)) :-
    % Extract fields from SemanticsVerification Pydantic model using py_call
    py_call(SemanticsVerificationObj:covered_files, CoveredFiles),
    py_call(SemanticsVerificationObj:missing_files, MissingFiles),
    py_call(SemanticsVerificationObj:suggestions, Suggestions),
    py_call(SemanticsVerificationObj:coverage_percentage, Coverage).

% Delegation relationships
component(golem(semantics_verifier), can_delegate_to, golem(test_planner)).
component(golem(semantics_verifier), can_delegate_to, golem(project_manager)).

% Auto-discovered from Golem(Id).tools() through python_bridge
component(golem(semantics_verifier), available_tools, Tools) :-
    get_golem_tools(golem(semantics_verifier), Tools).

% Docstring generated automatically by generic golem docstring rule in semantics.pl