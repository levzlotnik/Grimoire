:- use_module(library(plunit)).

% Load all test files from their respective locations
:- ensure_loaded('core_rules.plt').
:- ensure_loaded('../git.plt').
:- ensure_loaded('../nix/semantics.plt').
:- ensure_loaded('../project/semantics.plt').
:- ensure_loaded('self_entity.plt').
:- ensure_loaded('integration.plt').
:- ensure_loaded('spell_system.plt').
:- ensure_loaded('session_cli.plt').
:- ensure_loaded('session.plt').
:- ensure_loaded('interface_session.plt').
:- ensure_loaded('template_instantiation.plt').

% Load template test suites
:- ensure_loaded('../nix/templates/rust/semantics.plt').
:- ensure_loaded('../nix/templates/cpp/semantics.plt').
:- ensure_loaded('../nix/templates/python/semantics.plt').
:- ensure_loaded('../nix/templates/haskell/semantics.plt').
% Note: Lean4 and MkDocs don't have test suites by design

% Main test runner
run_all_tests :-
    format('~n=== Running Grimoire Test Suite ===~n'),
    run_tests([
        core_ecs,
        git_semantics,
        nix_semantics,
        project_semantics,
        integration_tests,
        spell_system,
        session_cli,
        file_session_system,
        interface_session_integration,
        template_instantiation,
        rust_project_semantics,
        cpp_project_semantics,
        python_project_semantics,
        haskell_project_semantics
    ]),
    test_summary.

% Individual test runners for development
run_core_tests :-
    run_tests([core_ecs]).

run_nix_tests :-
    run_tests([nix_semantics]).

run_git_tests :-
    run_tests([git_semantics]).

run_project_tests :-
    run_tests([project_semantics]).

run_integration_tests :-
    run_tests([integration_tests]).

% Template test runners
run_template_tests :-
    run_tests([
        rust_project_semantics,
        cpp_project_semantics,
        python_project_semantics,
        haskell_project_semantics
    ]).

run_rust_tests :-
    run_tests([rust_project_semantics]).

run_cpp_tests :-
    run_tests([cpp_project_semantics]).

run_python_tests :-
    run_tests([python_project_semantics]).

run_haskell_tests :-
    run_tests([haskell_project_semantics]).

% Template instantiation test runner
run_template_instantiation_tests :-
    run_tests([template_instantiation]).

% Test summary reporting
test_summary :-
    format('~n=== Test Suite Complete ===~n'),
    format('All core system tests PASSED ✓~n').

% Run specific tests by ID
run_specific_tests(TestArgs) :-
    format('~n=== Running Specific Tests: ~w ===~n', [TestArgs]),
    maplist(atom_string, TestAtoms, TestArgs),
    run_tests(TestAtoms),
    test_summary.

% List all available test suites
list_available_tests :-
    format('~nAvailable test suites:~n'),
    All_Tests = [
        core_ecs,
        git_semantics,
        nix_semantics,
        project_semantics,
        integration_tests,
        spell_system,
        session_cli,
        file_session_system,
        interface_session_integration,
        template_instantiation,
        rust_project_semantics,
        cpp_project_semantics,
        python_project_semantics,
        haskell_project_semantics
    ],
    forall(member(Test, All_Tests), 
           format('  ~w~n', [Test])).

% Convenient alias
:- multifile user:term_expansion/2.
user:term_expansion((:- test), (:- run_all_tests)) :- !.
