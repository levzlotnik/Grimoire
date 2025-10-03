% Interface Session Integration Tests
:- use_module(library(plunit)).
:- use_module(library(uuid)).

% Test suite for stateful interface operations
:- begin_tests(interface_session_integration).

% Helper function for starting sessions in tests
start_session(SessionId, Result) :-
    cast(conjure(session(start(SessionId))), Result).

% === SETUP AND CLEANUP ===

setup :-
    % Ensure clean state before each test - clean up test sessions
    cleanup_test_sessions,
    % Reset to main session
    set_current_session_id(main).

teardown :-
    % Clean up after each test
    cleanup_test_sessions,
    % Reset to main session
    set_current_session_id(main).

cleanup_test_sessions :-
    % Clean up any test session directories
    findall(SessionId, test_session_id(SessionId), SessionIds),
    maplist(cleanup_session, SessionIds).

cleanup_session(SessionId) :-
    catch(cast(conjure(session(delete(SessionId))), _), _, true).

% Define all test session IDs
test_session_id('test-interface-loading').
test_session_id('temp-session').
test_session_id('test-persistence-switch').
test_session_id('temp-session-2').
test_session_id('test-ensure-state').
test_session_id('test-semantic-mapping').
test_session_id('test-existing-entity').
test_session_id('test-error-types').
test_session_id('isolation-session-1').
test_session_id('isolation-session-2').
test_session_id('init-test').
test_session_id('cleanup-test').
test_session_id('abandon-cleanup-test').
test_session_id('test-load-state').

safe_delete_file(File) :-
    catch(delete_file(File), _, true).

% === INTERFACE SESSION STATE LOADING TESTS ===

test(interface_operations_load_session_state, [setup(setup), cleanup(teardown)]) :-
    % Start session and load an entity
    start_session('test-interface-loading', _),
    cast(conjure(interface(load('system'))), _),
    
    % Interface operations should work in the session context
    cast(conjure(interface(compt)), Result),
    assertion(Result = ok(component_types(_, _))), !.

test(session_state_persistence_in_workspace, [setup(setup), cleanup(teardown)]) :-
    % Start session and load entity
    start_session('test-persistence-switch', _),
    cast(conjure(interface(load('system'))), LoadResult1),
    assertion(LoadResult1 == ok(entity_loaded(system))),
    
    % Verify session state was persisted to file
    session_state_file_path('test-persistence-switch', StatePath),
    assertion(exists_file(StatePath)),
    read_file_to_string(StatePath, Content, []),
    assertion(sub_string(Content, _, _, _, 'load_entity')),
    
    % Interface operations should work with the loaded state
    cast(conjure(interface(status)), StatusResult),
    assertion(StatusResult = ok(session_status(_))), !.

test(interface_commands_ensure_session_state, [setup(setup), cleanup(teardown)]) :-
    % Start session
    start_session('test-ensure-state', _),

    % All interface commands should call ensure_session_state_loaded
    % Test various interface commands work correctly in session context
    cast(conjure(interface(compt)), ComptResult),
    assertion(ComptResult = ok(component_types(_, _))),

    cast(conjure(interface(doc('system'))), DocResult),
    assertion(DocResult = ok(documentation(system, _))),

    cast(conjure(interface(comp('system', 'concept'))), CompResult),
    assertion(CompResult = ok(components(system, concept, _))), !.

% === LOAD COMMAND COMPREHENSIVE TESTS ===

test(load_command_semantic_spec_mapping, [setup(setup), cleanup(teardown)]) :-
    start_session('test-semantic-mapping', _),

    % Test current directory maps to folder semantic
    cast(conjure(interface(load('.'))), DotResult),
    assertion(DotResult = error(entity_load_failed('.', semantic(folder(.)), _))),

    % Test path with slash maps to folder semantic
    cast(conjure(interface(load('/tmp'))), PathResult),
    assertion(PathResult = error(entity_load_failed('/tmp', semantic(folder('/tmp')), _))),

    % Test entity name maps to entity semantic
    cast(conjure(interface(load('nonexistent'))), EntityResult),
    assertion(EntityResult = error(entity_load_failed(nonexistent, semantic(entity(nonexistent)), _))), !.

test(load_command_existing_entity_handling, [setup(setup), cleanup(teardown)]) :-
    start_session('test-existing-entity', _),

    % Loading existing entity should succeed
    cast(conjure(interface(load('system'))), SystemResult),
    assertion(SystemResult == ok(entity_loaded(system))),

    % Loading interface entity should succeed
    cast(conjure(interface(load('interface'))), InterfaceResult),
    assertion(InterfaceResult == ok(entity_loaded(interface))), !.

test(load_command_error_types, [setup(setup), cleanup(teardown)]) :-
    start_session('test-error-types', _),

    % Test entity_load_failed error
    cast(conjure(interface(load('bad_entity'))), BadEntityResult),
    assertion(BadEntityResult = error(entity_load_failed(bad_entity, semantic(entity(bad_entity)), _))),

    % Test path load failure
    cast(conjure(interface(load('/nonexistent/path'))), BadPathResult),
    assertion(BadPathResult = error(entity_load_failed('/nonexistent/path', semantic(folder('/nonexistent/path')), _))), !.

% === SESSION ISOLATION TESTS ===

test(load_command_session_isolation, [setup(setup), cleanup(teardown)]) :-
    % Start first session and load entity
    start_session('isolation-session-1', _),
    cast(conjure(interface(load('system'))), _),
    session_state_file_path('isolation-session-1', FilePath1),

    % Start second session - should have separate state
    start_session('isolation-session-2', _),
    session_state_file_path('isolation-session-2', FilePath2),

    % Session files should be different
    assertion(FilePath1 \= FilePath2),

    % First session file should contain load, second should not
    read_file_to_string(FilePath1, Content1, []),
    assertion(sub_string(Content1, _, _, _, "load_entity")),

    read_file_to_string(FilePath2, Content2, []),
    assertion(\+ sub_string(Content2, _, _, _, "load_entity")), !.

% === SESSION STATE FILE LIFECYCLE TESTS ===

test(session_state_file_initialization, [setup(setup), cleanup(teardown)]) :-
    % Session creation should initialize state file
    start_session('init-test', _),
    session_state_file_path('init-test', FilePath),

    % File should exist and contain header
    assertion(exists_file(FilePath)),
    read_file_to_string(FilePath, Content, []),
    assertion(sub_string(Content, _, _, _, "Session state file for session init-test")), !.

test(session_state_file_cleanup_on_closure, [setup(setup), cleanup(teardown)]) :-
    % Create session and load entity
    start_session('cleanup-test', _),
    cast(conjure(interface(load('system'))), _),
    session_state_file_path('cleanup-test', FilePath),
    assertion(exists_file(FilePath)),

    % Close session should clean up state file
    close_session('cleanup-test', merge_to_main, _),
    assertion(\+ exists_file(FilePath)),

    % Test abandon cleanup too
    start_session('abandon-cleanup-test', _),
    cast(conjure(interface(load('system'))), _),
    session_state_file_path('abandon-cleanup-test', AbandonPath),
    assertion(exists_file(AbandonPath)),

    close_session('abandon-cleanup-test', abandon, _),
    assertion(\+ exists_file(AbandonPath)), !.

% === MAIN SESSION BEHAVIOR TESTS ===

test(main_session_no_persistence, [setup(setup), cleanup(teardown)]) :-
    % Main session is the default
    get_current_session_id(SessionId),
    assertion(SessionId == main),

    % Load in main session should succeed but not create files
    cast(conjure(interface(load('system'))), MainResult),
    assertion(MainResult == ok(entity_loaded(system))),

    % No session state file should be created for main
    session_state_file_path('main', MainPath),
    assertion(\+ exists_file(MainPath)), !.

:- end_tests(interface_session_integration).