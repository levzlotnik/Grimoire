% Tests for Project Manager Golem semantics
% Validates entity declarations, components, and semantic relationships

:- use_module(library(plunit)).

:- begin_tests(project_manager_semantics).

% === ENTITY TESTS ===

test(project_manager_entity_exists) :-
    entity(golem(project_manager)).

% === COMPONENT TESTS ===

test(project_manager_has_output_parser) :-
    component(golem(project_manager), output_parser, parse_project_analysis).

test(project_manager_delegation_relationships) :-
    component(golem(project_manager), can_delegate_to, golem(architect)),
    component(golem(project_manager), can_delegate_to, golem(semantics_verifier)).

test(project_manager_has_available_tools) :-
    component(golem(project_manager), available_tools, _Tools).

% === PARSER TESTS ===

test(parse_project_analysis_with_full_dict) :-
    Dict = _{
        structure: _{src: ["main.py"], tests: ["test_main.py"]},
        dependencies: ["requests", "numpy"],
        entry_points: ["main.py", "cli.py"],
        configuration_files: ["pyproject.toml", "setup.py"],
        recommendations: ["Add type hints", "Improve test coverage"]
    },
    parse_project_analysis(Dict, project_analysis(Structure, Dependencies, EntryPoints, ConfigFiles, Recommendations)),
    Structure = _{src: ["main.py"], tests: ["test_main.py"]},
    Dependencies = ["requests", "numpy"],
    EntryPoints = ["main.py", "cli.py"],
    ConfigFiles = ["pyproject.toml", "setup.py"],
    Recommendations = ["Add type hints", "Improve test coverage"].

test(parse_project_analysis_with_minimal_dict) :-
    Dict = _{
        structure: _{},
        dependencies: [],
        entry_points: [],
        configuration_files: []
    },
    parse_project_analysis(Dict, project_analysis(Structure, Dependencies, EntryPoints, ConfigFiles, Recommendations)),
    Structure = _{},
    Dependencies = [],
    EntryPoints = [],
    ConfigFiles = [],
    Recommendations = [].

:- end_tests(project_manager_semantics).