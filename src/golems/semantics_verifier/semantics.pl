% Semantics Verifier Golem
% Verifies that all meaningful source code files are tested in semantics.plt

:- self_entity(golem(semantics_verifier)).

% Configuration now handled in Python __init__.py
% No longer need config component here since golem is instantiated in Python

% Structured output parser (optional)
component(golem(semantics_verifier), output_parser, parse_semantics_verification).

% Parser converts dict to Prolog term (from SemanticsVerification type)
parse_semantics_verification(Dict, semantics_verification(CoveredFiles, MissingFiles, Suggestions, Coverage)) :-
    get_dict(covered_files, Dict, CoveredFiles),
    get_dict(missing_files, Dict, MissingFiles),
    (get_dict(suggestions, Dict, Suggestions) -> true; Suggestions = []),
    get_dict(coverage_percentage, Dict, Coverage).

% Delegation relationships
component(golem(semantics_verifier), can_delegate_to, golem(test_planner)).
component(golem(semantics_verifier), can_delegate_to, golem(project_manager)).

% Auto-discovered from Golem(Id).tools() through python_bridge
component(golem(semantics_verifier), available_tools, Tools) :-
    get_golem_tools(golem(semantics_verifier), Tools).

% Docstring generated automatically by generic golem docstring rule in semantics.pl