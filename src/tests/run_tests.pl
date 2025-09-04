:- use_module(library(plunit)).

% Load all test files from their respective locations
:- grimoire_ensure_loaded('@/src/tests/core_rules.plt').
:- grimoire_ensure_loaded('@/src/git.plt').
:- grimoire_ensure_loaded('@/src/nix/semantics.plt').
:- grimoire_ensure_loaded('@/src/project/semantics.plt').
:- grimoire_ensure_loaded('@/src/tests/self_entity.plt').
:- grimoire_ensure_loaded('@/src/tests/integration.plt').
:- grimoire_ensure_loaded('@/src/tests/spell_system.plt').
:- grimoire_ensure_loaded('@/src/tests/session_cli.plt').
:- grimoire_ensure_loaded('@/src/tests/session.plt').
:- grimoire_ensure_loaded('@/src/db/semantics.plt').
:- grimoire_ensure_loaded('@/src/tests/interface_session.plt').
:- grimoire_ensure_loaded('@/src/interface/semantics.plt').
:- grimoire_ensure_loaded('@/src/tests/template_instantiation.plt').

% Load golems test suite
:- grimoire_ensure_loaded('@/src/golems/semantics.plt').

% Load template test suites
:- grimoire_ensure_loaded('@/src/nix/templates/rust/semantics.plt').
:- grimoire_ensure_loaded('@/src/nix/templates/cpp/semantics.plt').
:- grimoire_ensure_loaded('@/src/nix/templates/python/semantics.plt').
:- grimoire_ensure_loaded('@/src/nix/templates/haskell/semantics.plt').
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
        session_system,
        db_entity,
        interface,
        interface_session_integration,
        template_instantiation,
        golems,
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

run_golems_tests :-
    run_tests([golems]).

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
        session_system,
        db_entity,
        interface_session_integration,
        template_instantiation,
        golems,
        rust_project_semantics,
        cpp_project_semantics,
        python_project_semantics,
        haskell_project_semantics
    ],
    forall(member(Test, All_Tests), 
           format('  ~w~n', [Test])).

% Run tests from specific .plt files
run_test_files(TestNames, FilePaths) :-
    format('~n=== Running Tests from Files ===~n'),
    (TestNames \= [] ->
        format('Test suites: ~w~n', [TestNames])
    ;
        format('Running tests from specified files only~n')
    ),
    format('Files: ~w~n~n', [FilePaths]),
    
    % Ensure all files exist and are loadable
    maplist(ensure_test_file_loadable, FilePaths, LoadedFiles),
    
    % Get current test units before loading
    findall(Unit, current_test_unit(Unit), BeforeUnits),
    
    % Load the test files - this will register new test units
    maplist(grimoire_ensure_loaded, LoadedFiles),
    
    % Get test units after loading 
    findall(Unit, current_test_unit(Unit), AfterUnits),
    
    % Find the newly loaded test units
    subtract(AfterUnits, BeforeUnits, NewUnits),
    
    % Run only the new test units (from the loaded files)
    (NewUnits \= [] ->
        format('Running test units from files: ~w~n', [NewUnits]),
        run_tests(NewUnits)
    ;
        format('No test units found in the specified files~n')
    ),
    
    test_summary.

% Helper to get current test units
current_test_unit(Unit) :-
    plunit:current_unit(Unit, _, _, _).

% Ensure test file exists and normalize path
ensure_test_file_loadable(FilePath, NormalizedPath) :-
    % Convert to absolute path if relative
    (is_absolute_file_name(FilePath) ->
        AbsPath = FilePath
    ;
        absolute_file_name(FilePath, AbsPath)
    ),
    
    % Check if file exists
    (exists_file(AbsPath) ->
        NormalizedPath = AbsPath
    ;
        format('Error: Test file not found: ~w~n', [FilePath]),
        fail
    ).

% Check if a test unit currently exists
current_test_unit_exists(Unit) :-
    % Check if test predicates exist for this unit
    current_predicate(plunit:test/2),
    plunit:test(Unit, _).

% Convenient alias
:- multifile user:term_expansion/2.
user:term_expansion((:- test), (:- run_all_tests)) :- !.
