% File-Based Session System Tests
:- use_module(library(plunit)).
:- use_module(library(uuid)).
:- use_module(library(prosqlite)).
:- use_module(library(readutil)).

% Test suite for file-based session system
:- begin_tests(file_session_system).

% === SETUP AND CLEANUP ===

setup :-
    % Clean up any existing test workspaces
    cleanup_test_workspaces.

teardown :-
    % Clean up after each test
    cleanup_test_workspaces.

cleanup_test_workspaces :-
    % Remove any test session workspaces
    grimoire_root_directory(GrimoireRoot),
    format(atom(SessionsDir), '~w/sessions', [GrimoireRoot]),
    (exists_directory(SessionsDir) ->
        % Clean up test sessions (ones starting with 'test-')
        catch(
            (directory_files(SessionsDir, Files),
             forall(
                (member(File, Files),
                 atom_concat('test-', _, File)),
                (format(atom(TestDir), '~w/~w', [SessionsDir, File]),
                 catch(delete_directory_and_contents(TestDir), _, true))
             )),
            _, true)
    ;
        true
    ).

% === BASIC FUNCTIONALITY TESTS ===

test(session_start_creates_workspace, [setup(setup), cleanup(teardown)]) :-
    % Starting a session should create workspace directory
    run(command(session(start('test-workspace'))), Result),
    assertion(Result = ok(session_started('test-workspace'))),
    
    % Verify workspace directory exists
    session_workspace_path('test-workspace', WorkspacePath),
    assertion(exists_directory(WorkspacePath)),
    
    % Verify database file exists (prosqlite adds .sqlite extension)
    session_commands_db_path('test-workspace', DbPath),
    format(atom(ActualDbPath), '~w.sqlite', [DbPath]),
    assertion(exists_file(ActualDbPath)),
    
    % Verify state file exists
    session_state_file_path('test-workspace', StatePath),
    assertion(exists_file(StatePath)).

test(think_command_with_string, [setup(setup), cleanup(teardown)]) :-
    % Think command should work with proper strings
    run(command(think("This is a test thought")), Result),
    assertion(Result = ok(thought_recorded("This is a test thought"))).

test(think_command_with_atom_conversion, [setup(setup), cleanup(teardown)]) :-
    % Think command should convert atoms to strings
    run(command(think(test_thought)), Result),
    assertion(Result = ok(thought_recorded("test_thought"))).

test(session_history_command, [setup(setup), cleanup(teardown)]) :-
    % History command should return actual session history
    run(command(session(history)), Result),
    assertion(Result = ok(session_history(main, []))).

test(commit_accumulated_placeholder, [setup(setup), cleanup(teardown)]) :-
    % Commit accumulated should work with string messages
    run(command(session(commit_accumulated("Test commit message"))), Result),
    assertion(Result = ok(commit_accumulated_placeholder("Test commit message"))).

% === WORKSPACE PATH TESTS ===

test(workspace_path_resolution, [setup(setup), cleanup(teardown)]) :-
    % Test workspace path resolution
    session_workspace_path('test-session', WorkspacePath),
    grimoire_root_directory(GrimoireRoot),
    format(atom(ExpectedPath), '~w/sessions/test-session', [GrimoireRoot]),
    assertion(WorkspacePath = ExpectedPath).

test(commands_db_path_resolution, [setup(setup), cleanup(teardown)]) :-
    % Test commands database path resolution
    session_commands_db_path('test-session', DbPath),
    session_workspace_path('test-session', WorkspacePath),
    format(atom(ExpectedDbPath), '~w/commands', [WorkspacePath]),
    assertion(DbPath = ExpectedDbPath).

test(state_file_path_resolution, [setup(setup), cleanup(teardown)]) :-
    % Test state file path resolution
    session_state_file_path('test-session', StatePath),
    session_workspace_path('test-session', WorkspacePath),
    format(atom(ExpectedStatePath), '~w/state.pl', [WorkspacePath]),
    assertion(StatePath = ExpectedStatePath).

% === ENTITY LOADING TESTS ===

test(add_entity_load_to_session, [setup(setup), cleanup(teardown)]) :-
    % Test adding entity load to session state file
    run(command(session(start('test-entity-load'))), _),
    
    % Add an entity load
    add_entity_load_to_session('test-entity-load', semantic(folder(test))),
    
    % Check that state file contains the load
    session_state_file_path('test-entity-load', StatePath),
    read_file_to_string(StatePath, Content, []),
    assertion(sub_string(Content, _, _, _, 'load_entity(semantic(folder(test)))')).

test(load_session_state_file, [setup(setup), cleanup(teardown)]) :-
    % Test loading session state file
    run(command(session(start('test-state-load'))), _),
    
    % Add some content to state file
    session_state_file_path('test-state-load', StatePath),
    open(StatePath, append, Stream),
    format(Stream, 'test_fact(loaded_from_session).~n', []),
    close(Stream),
    
    % Load the state file
    load_session_state_file('test-state-load'),
    
    % Verify the fact was loaded
    assertion(test_fact(loaded_from_session)).

% === DATABASE FUNCTIONALITY TESTS ===

test(database_schema_creation, [setup(setup), cleanup(teardown)]) :-
    % Test that database schema is created properly
    run(command(session(start('test-db-schema'))), _),
    
    % Check database file exists (prosqlite adds .sqlite extension)
    session_commands_db_path('test-db-schema', DbPath),
    format(atom(ActualDbPath), '~w.sqlite', [DbPath]),
    assertion(exists_file(ActualDbPath)),
    
    % Verify schema by checking if we can query the commands table
    catch(
        (sqlite_connect(DbPath, test_schema_conn, []),
         sqlite_query(test_schema_conn, 'SELECT name FROM sqlite_master WHERE type="table" AND name="commands"', row('commands')),
         sqlite_disconnect(test_schema_conn)),
        Error,
        (format('Database schema test failed: ~w~n', [Error]), fail)
    ).

test(command_logging_to_database, [setup(setup), cleanup(teardown)]) :-
    % Test that commands are properly logged to database
    run(command(session(start('test-logging'))), _),
    
    % Log a test command
    log_command_to_session_db('test-logging', test_command(param), action, ok(test_result), user),
    
    % Verify command was logged
    session_commands_db_path('test-logging', DbPath),
    sqlite_connect(DbPath, test_logging_conn, []),
    sqlite_query(test_logging_conn, 'SELECT COUNT(*) FROM commands WHERE command_type = "action"', row(Count)),
    sqlite_disconnect(test_logging_conn),
    assertion(Count = 1).

test(session_history_retrieval, [setup(setup), cleanup(teardown)]) :-
    % Test retrieving command history from database
    run(command(session(start('test-history'))), _),
    
    % Add some test commands
    log_command_to_session_db('test-history', think("test thought"), think, ok(thought_recorded("test thought")), user),
    log_command_to_session_db('test-history', git(status), action, ok(status_result), user),
    
    % Retrieve history
    get_session_command_history('test-history', Commands),
    
    % Verify we got the commands back
    assertion(length(Commands, 2)),
    assertion(Commands = [command_entry(_, think, _, _, user), command_entry(_, action, _, _, user)]).

test(filtered_history_by_type, [setup(setup), cleanup(teardown)]) :-
    % Test filtering command history by type
    run(command(session(start('test-filter'))), _),
    
    % Add commands of different types
    log_command_to_session_db('test-filter', think("thought 1"), think, ok(result), user),
    log_command_to_session_db('test-filter', git(status), action, ok(result), user),
    log_command_to_session_db('test-filter', think("thought 2"), think, ok(result), user),
    
    % Filter by think type
    get_filtered_command_history('test-filter', [type(think)], ThinkCommands),
    
    % Should only get think commands
    assertion(length(ThinkCommands, 2)),
    forall(member(command_entry(_, Type, _, _, _), ThinkCommands), assertion(Type = think)).

test(think_command_database_integration, [setup(setup), cleanup(teardown)]) :-
    % Test that think command properly integrates with database logging
    % First create a test session to avoid main session issues
    run(command(session(start('test-think-db'))), _),
    
    % Override get_current_session_id for this test
    retractall(current_test_session(_)),
    assert(current_test_session('test-think-db')),
    
    % Execute think command (this should log to database)
    run(command(think("Test database integration")), Result),
    assertion(Result = ok(thought_recorded("Test database integration"))),
    
    % Check that it was logged to database
    get_session_command_history('test-think-db', Commands),
    length(Commands, N),
    assertion(N > 0),
    
    % Clean up
    retractall(current_test_session(_)).

:- end_tests(file_session_system).