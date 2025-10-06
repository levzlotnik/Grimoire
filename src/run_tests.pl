% Test infrastructure
:- use_module(library(plunit)).
:- use_module(library(filesex)).

% Load grimoire.pl which loads all ECS infrastructure
:- grimoire_ensure_loaded('@/src/grimoire.pl').

% Load verification infrastructure (.plt files define please_verify/1 and verify/1 hooks)
:- grimoire_ensure_loaded('@/src/ecs_kernel.plt').
:- grimoire_ensure_loaded('@/src/grimoire.plt').

:- self_entity(run_tests).

docstring(run_tests, "
Simple test infrastructure for discovering and running PLUnit tests.

Key Features:
- Automatic discovery via load_test_files/1
- Test filtering with exclude patterns
- Targeted test execution by entity:test_name format
- Environment variable configuration
- Integration with existing PLUnit test framework

Architecture:
- load_test_files/1: Auto-load .plt files for loaded .pl files
- get_test_exclude_patterns/1: Filter tests (env or defaults)
- run_test_arg/2: Handle unit:test syntax
- current_test_unit/1: Query loaded test units

Usage Patterns:
1. Run all (filtered): grimoire test
2. Run specific test: grimoire test entity:test_name
3. Run entity tests: grimoire test entity
4. Custom filter: TEST_EXCLUDE='pattern1,pattern2' grimoire test

Test Filtering:
- Set TEST_EXCLUDE env var to comma-separated patterns
- Default excludes Level 2+ domains during Phase 2 implementation
- Excludes golems (requires PydanticAI), integration tests, etc.
").

% === TEST DISCOVERY ===

% Discover all *.plt files in src directory
discover_test_files(Files) :-
    grimoire_root(Root),
    atomic_list_concat([Root, '/src'], '', SrcDir),
    find_files_recursive(SrcDir, '*.plt', Files).

% Helper to recursively find files with extension
find_files_recursive(Dir, Pattern, Files) :-
    exists_directory(Dir),
    directory_files(Dir, AllEntries),
    findall(File, (
        member(Entry, AllEntries),
        Entry \= '.',
        Entry \= '..',
        directory_file_path(Dir, Entry, Path),
        (   exists_file(Path),
            atom_string(Pattern, PatternStr),
            atom_string(Path, PathStr),
            (   sub_string(PatternStr, 0, 1, _, "*")
            ->  % Wildcard pattern - check extension
                sub_string(PatternStr, 1, _, 0, ExtStr),
                sub_string(PathStr, _, _, 0, ExtStr)
            ;   % Exact match
                PathStr = PatternStr
            ),
            File = Path
        ;   exists_directory(Path),
            find_files_recursive(Path, Pattern, SubFiles),
            member(File, SubFiles)
        )
    ), Files).

% Extract test unit name from .plt file
extract_test_unit_from_file(File, TestUnit) :-
    setup_call_cleanup(
        open(File, read, Stream),
        read_test_unit_name(Stream, TestUnit),
        close(Stream)
    ).

% Read file looking for :- begin_tests(UnitName)
read_test_unit_name(Stream, TestUnit) :-
    read_term(Stream, Term, []),
    (   Term == end_of_file
    ->  fail  % No test unit found
    ;   Term = (:- begin_tests(TestUnit))
    ->  true  % Found it
    ;   read_test_unit_name(Stream, TestUnit)  % Keep looking
    ).

% Build test registry mapping files to test units
build_test_registry(Registry) :-
    discover_test_files(Files),
    findall(Unit-File, (
        member(File, Files),
        extract_test_unit_from_file(File, Unit)
    ), Registry).

% === QUERY INTERFACE ===

% Get all available test units
available_tests(Tests) :-
    build_test_registry(Registry),
    findall(Unit, member(Unit-_, Registry), Tests).

% Get test units for a specific entity (by file path pattern)
available_tests_for_entity(Entity, Tests) :-
    build_test_registry(Registry),
    findall(Unit, (
        member(Unit-File, Registry),
        atom_string(File, FileStr),
        atom_string(Entity, EntityStr),
        sub_string(FileStr, _, _, _, EntityStr)
    ), Tests).

% === TEST EXECUTION ===

% Load a test file if not already loaded
ensure_test_file_loaded(File) :-
    absolute_file_name(File, AbsPath),
    (   exists_file(AbsPath)
    ->  grimoire_ensure_loaded(AbsPath)
    ;   throw(error(test_file_not_found(AbsPath)))
    ).

% Run a specific test unit
run_test(TestUnit, Result) :-
    build_test_registry(Registry),
    (   member(TestUnit-File, Registry)
    ->  ensure_test_file_loaded(File),
        catch(
            (run_tests([TestUnit]), Result = passed),
            Error,
            Result = failed(Error)
        )
    ;   Result = error(test_unit_not_found(TestUnit))
    ).

% Run all tests for a specific entity
run_tests_for_entity(Entity, Results) :-
    available_tests_for_entity(Entity, Tests),
    maplist(run_test, Tests, Results).

% Run tests matching a pattern
run_tests_matching(Pattern, Results) :-
    available_tests(AllTests),
    findall(Test, (
        member(Test, AllTests),
        atom_string(Test, TestStr),
        atom_string(Pattern, PatternStr),
        sub_string(TestStr, _, _, _, PatternStr)
    ), MatchingTests),
    maplist(run_test, MatchingTests, Results).

% Test filtering - exclude patterns from environment or default
get_test_exclude_patterns(Patterns) :-
    % Check for TEST_EXCLUDE environment variable
    (getenv('TEST_EXCLUDE', ExcludeStr) ->
        split_string(ExcludeStr, ",", " ", PatternStrs),
        maplist(atom_string, Patterns, PatternStrs)
    ;
        % Default exclude patterns for Phase 2 implementation
        Patterns = [
            project,            % Level 3 - not yet reimplemented
            session_cli,        % Level 3 - not yet reimplemented
            session_system,     % Level 3 - not yet reimplemented
            golems,             % Level 3 - requires PydanticAI
            integration_tests,  % Requires all levels
            spell_system,       % Requires all levels
            interface,          % Level 4 - not yet reimplemented
            interface_session_integration,  % Level 4 - not yet reimplemented
            self_entity,        % Legacy test
            template_instantiation  % Level 3 - templates
        ]
    ).

% Check if test should be excluded
test_excluded(TestUnit, ExcludePatterns) :-
    member(Pattern, ExcludePatterns),
    (   atom(Pattern)
    ->  TestUnit = Pattern
    ;   atom_string(TestUnit, TestStr),
        atom_string(Pattern, PatternStr),
        sub_string(TestStr, _, _, _, PatternStr)
    ).

% Run all discovered tests (with filtering)
run_all_tests :-
    format('~n=== Running All Discovered Tests ===~n'),
    % Manually load all .plt files to avoid PLUnit's module scoping issues
    discover_test_files(TestFiles),
    maplist(grimoire_ensure_loaded, TestFiles),
    % Get all test units and filter
    findall(Unit, current_test_unit(Unit), AllTests),
    get_test_exclude_patterns(ExcludePatterns),
    findall(Unit,
        (member(Unit, AllTests), \+ test_excluded(Unit, ExcludePatterns)),
        Tests),
    length(AllTests, TotalCount),
    length(Tests, Count),
    (ExcludePatterns \= [] ->
        format('Found ~w test units (~w excluded)~n~n', [Count, TotalCount])
    ;
        format('Found ~w test units~n~n', [Count])
    ),
    run_tests(Tests),
    test_summary.

% Run specific tests (interface compatibility)
run_specific_tests(TestArgs) :-
    format('~n=== Running Specific Tests: ~w ===~n', [TestArgs]),
    build_test_registry(Registry),
    % Load all test files first
    findall(File, (
        member(TestArg, TestArgs),
        atom(TestArg),
        member(TestArg-File, Registry)
    ), Files),
    maplist(ensure_test_file_loaded, Files),
    % Then run all tests together
    run_tests(TestArgs),
    test_summary.

% Helper to run a single test argument (handles unit:test syntax)
run_test_arg(Registry, TestArg) :-
    atom_string(TestArg, TestArgStr),
    (   sub_string(TestArgStr, _, 1, _, ":")
    ->  % entity:test_name format - run specific test within unit
        split_string(TestArgStr, ":", "", [UnitStr, TestNameStr]),
        atom_string(Unit, UnitStr),
        atom_string(TestName, TestNameStr),
        (   member(Unit-File, Registry)
        ->  ensure_test_file_loaded(File),
            format('Running ~w:~w~n', [Unit, TestName]),
            run_tests(Unit:TestName)
        ;   format('Error: Test unit ~w not found~n', [Unit])
        )
    ;   % Just a unit name - run all tests in unit
        atom_string(TestAtom, TestArgStr),
        (   member(TestAtom-File, Registry)
        ->  ensure_test_file_loaded(File),
            run_tests([TestAtom])
        ;   format('Error: Test unit ~w not found~n', [TestAtom])
        )
    ).

% === CLI INTEGRATION ===

% Handle grimoire test commands
handle_test_command([]) :-
    % Run all tests
    run_all_tests.

handle_test_command([TestArg]) :-
    % Check if it's entity:test format or just entity
    atom_string(TestArg, TestStr),
    (   sub_string(TestStr, _, 1, _, ":")
    ->  % Specific test in entity:test_name format
        split_string(TestStr, ":", "", [EntityStr, TestNameStr]),
        atom_string(Entity, EntityStr),
        atom_string(TestName, TestNameStr),
        format('~n=== Running Test: ~w in ~w ===~n', [TestName, Entity]),
        % For now, run all entity tests (PLUnit doesn't easily support individual test selection)
        run_tests_for_entity(Entity, Results),
        report_test_results(Results)
    ;   % All tests for entity
        format('~n=== Running Tests for Entity: ~w ===~n', [TestArg]),
        run_tests_for_entity(TestArg, Results),
        report_test_results(Results)
    ).

handle_test_command(TestArgs) :-
    % Multiple test units specified
    format('~n=== Running Specific Tests: ~w ===~n', [TestArgs]),
    maplist(run_test, TestArgs, Results),
    report_test_results(Results).

% Report test results
report_test_results(Results) :-
    length(Results, Total),
    findall(R, (member(R, Results), R = passed), Passed),
    length(Passed, PassedCount),
    Failed is Total - PassedCount,
    format('~nTests run: ~w, Passed: ~w, Failed: ~w~n',
           [Total, PassedCount, Failed]),
    (Failed > 0 ->
        format('~nFailed tests:~n'),
        forall(
            member(failed(Error), Results),
            format('  - ~w~n', [Error])
        )
    ; true).

% Test summary reporting
test_summary :-
    format('~n=== Test Suite Complete ===~n').

% List all available tests
list_available_tests :-
    format('~nAvailable test units:~n'),
    available_tests(Tests),
    forall(member(Test, Tests),
           format('  ~w~n', [Test])).

% Run tests from specific .plt files (interface compatibility)
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
