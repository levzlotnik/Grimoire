% Interface Session Integration Tests
:- use_module(library(plunit)).
:- use_module(library(uuid)).

% Test suite for stateful interface operations
:- begin_tests(interface_session_integration).

% === SETUP AND CLEANUP ===

setup :-
    % Ensure clean state before each test
    catch(run(command(git(checkout(['main']))), _), _, true),
    cleanup_all_test_branches,
    cleanup_all_session_files.

teardown :-
    % Clean up after each test
    catch(run(command(git(checkout(['main']))), _), _, true),
    cleanup_all_test_branches,
    cleanup_all_session_files.

cleanup_all_test_branches :-
    % Clean up any session or transition branches from tests
    run(command(git(branch(['--format=%(refname:short)']))), BranchResult),
    (BranchResult = ok(result(BranchOutput, _)) ->
        split_string(BranchOutput, '\n', '\n \t', BranchLines),
        include(is_test_branch, BranchLines, TestBranches),
        maplist(force_delete_branch, TestBranches)
    ; 
        true
    ).

is_test_branch(BranchName) :-
    string_concat("session-", _, BranchName) ;
    string_concat("transition_branch/", _, BranchName).

force_delete_branch(BranchName) :-
    run(command(git(branch(['-D', BranchName]))), _).

cleanup_all_session_files :-
    % Clean up any session state files from tests
    grimoire_root_directory(Root),
    format(atom(Pattern), '~w/session-*.pl', [Root]),
    expand_file_name(Pattern, Files),
    maplist(safe_delete_file, Files).

safe_delete_file(File) :-
    catch(delete_file(File), _, true).

% === INTERFACE SESSION STATE LOADING TESTS ===

test(interface_operations_load_session_state, [setup(setup), cleanup(teardown)]) :-
    % Start session and load an entity
    start_session('test-interface-loading', _),
    run(command(interface(load('system'))), _),
    
    % Switch away and back to session
    start_session('temp-session', _),
    run(command(git(checkout(['session-test-interface-loading']))), _),
    
    % Interface operations should automatically load session state
    % We can't directly test that load_session_state_file was called,
    % but we can test that the session context is maintained
    run(command(interface(compt)), Result),
    assertion(Result = ok(component_types(_, _))).

test(session_state_persistence_across_switches, [setup(setup), cleanup(teardown)]) :-
    % Start session and load entity
    start_session('test-persistence-switch', _),
    run(command(interface(load('system'))), LoadResult1),
    assertion(LoadResult1 = ok(entity_loaded(system))),
    
    % Switch to another session
    start_session('temp-session-2', _),
    
    % Switch back to original session
    run(command(git(checkout(['session-test-persistence-switch']))), _),
    
    % Session state should be reloaded
    % Test that interface operations work (implicitly loading session state)
    run(command(interface(status)), StatusResult),
    assertion(StatusResult = ok(session_status(_))).

test(interface_commands_ensure_session_state, [setup(setup), cleanup(teardown)]) :-
    % Start session
    start_session('test-ensure-state', _),
    
    % All interface commands should call ensure_session_state_loaded
    % Test various interface commands work correctly in session context
    run(command(interface(compt)), ComptResult),
    assertion(ComptResult = ok(component_types(_, _))),
    
    run(command(interface(doc('system'))), DocResult),
    assertion(DocResult = ok(documentation(system, _))),
    
    run(command(interface(comp('system', 'concept'))), CompResult),
    assertion(CompResult = ok(components(system, concept, _))).

% === LOAD COMMAND COMPREHENSIVE TESTS ===

test(load_command_semantic_spec_mapping, [setup(setup), cleanup(teardown)]) :-
    start_session('test-semantic-mapping', _),
    
    % Test current directory maps to folder semantic
    run(command(interface(load('.'))), DotResult),
    assertion(DotResult = error(entity_load_failed('.', semantic(folder(.)), _))),
    
    % Test path with slash maps to folder semantic
    run(command(interface(load('/tmp'))), PathResult),
    assertion(PathResult = error(entity_load_failed('/tmp', semantic(folder('/tmp')), _))),
    
    % Test entity name maps to entity semantic
    run(command(interface(load('nonexistent'))), EntityResult),
    assertion(EntityResult = error(entity_load_failed(nonexistent, semantic(entity(nonexistent)), _))).

test(load_command_existing_entity_handling, [setup(setup), cleanup(teardown)]) :-
    start_session('test-existing-entity', _),
    
    % Loading existing entity should succeed
    run(command(interface(load('system'))), SystemResult),
    assertion(SystemResult = ok(entity_loaded(system))),
    
    % Loading interface entity should succeed
    run(command(interface(load('interface'))), InterfaceResult),
    assertion(InterfaceResult = ok(entity_loaded(interface))).

test(load_command_error_types, [setup(setup), cleanup(teardown)]) :-
    start_session('test-error-types', _),
    
    % Test entity_load_failed error
    run(command(interface(load('bad_entity'))), BadEntityResult),
    assertion(BadEntityResult = error(entity_load_failed(bad_entity, semantic(entity(bad_entity)), _))),
    
    % Test path load failure
    run(command(interface(load('/nonexistent/path'))), BadPathResult),
    assertion(BadPathResult = error(entity_load_failed('/nonexistent/path', semantic(folder('/nonexistent/path')), _))).

% === SESSION ISOLATION TESTS ===

test(load_command_session_isolation, [setup(setup), cleanup(teardown)]) :-
    % Start first session and load entity
    start_session('isolation-session-1', _),
    run(command(interface(load('system'))), _),
    session_state_file_path('isolation-session-1', FilePath1),
    
    % Start second session - should have separate state
    start_session('isolation-session-2', _),
    session_state_file_path('isolation-session-2', FilePath2),
    
    % Session files should be different
    assertion(FilePath1 \= FilePath2),
    
    % First session file should contain load, second should not
    read_file_to_string(FilePath1, Content1, []),
    assertion(string_concat(_, "load_entity", Content1)),
    
    read_file_to_string(FilePath2, Content2, []),
    assertion(\+ string_concat(_, "load_entity", Content2)).

% === SESSION STATE FILE LIFECYCLE TESTS ===

test(session_state_file_initialization, [setup(setup), cleanup(teardown)]) :-
    % Session creation should initialize state file
    start_session('init-test', _),
    session_state_file_path('init-test', FilePath),
    
    % File should exist and contain header
    assertion(exists_file(FilePath)),
    read_file_to_string(FilePath, Content, []),
    assertion(string_concat("% Session state file for session init-test", _, Content)).

test(session_state_file_cleanup_on_closure, [setup(setup), cleanup(teardown)]) :-
    % Create session and load entity
    start_session('cleanup-test', _),
    run(command(interface(load('system'))), _),
    session_state_file_path('cleanup-test', FilePath),
    assertion(exists_file(FilePath)),
    
    % Close session should clean up state file
    close_session('cleanup-test', merge_to_main, _),
    assertion(\+ exists_file(FilePath)),
    
    % Test abandon cleanup too
    start_session('abandon-cleanup-test', _),
    run(command(interface(load('system'))), _),
    session_state_file_path('abandon-cleanup-test', AbandonPath),
    assertion(exists_file(AbandonPath)),
    
    close_session('abandon-cleanup-test', abandon, _),
    assertion(\+ exists_file(AbandonPath)).

% === MAIN SESSION BEHAVIOR TESTS ===

test(main_session_no_persistence, [setup(setup), cleanup(teardown)]) :-
    % Ensure we're on main
    run(command(git(checkout(['main']))), _),
    get_current_session_id(SessionId),
    assertion(SessionId = main),
    
    % Load in main session should succeed but not create files
    run(command(interface(load('system'))), MainResult),
    assertion(MainResult = ok(entity_loaded(system))),
    
    % No session state file should be created for main
    session_state_file_path('main', MainPath),
    assertion(\+ exists_file(MainPath)).

:- end_tests(interface_session_integration).