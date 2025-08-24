% Session System Tests
% Tests for the file-based session system with SQLite command logging

:- use_module(library(plunit)).
:- use_module(library(uuid)).

:- begin_tests(session_system).

% Test cleanup helper
cleanup_session(SessionId) :-
    catch(cast(conjure(session(delete(SessionId))), _), _, true).


% === SETUP AND CLEANUP ===

setup :-
    cleanup_test_sessions,
    % Reset to main session
    set_current_session_id(main).

teardown :-
    cleanup_test_sessions,
    % Reset to main session
    set_current_session_id(main).

cleanup_test_sessions :-
    % Clean up test session workspaces
    grimoire_root_directory(GrimoireRoot),
    format(atom(SessionsDir), '~w/sessions', [GrimoireRoot]),
    (exists_directory(SessionsDir) ->
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

% === BASIC SESSION FUNCTIONALITY ===

test(session_start_without_id) :-
    % Session start without ID should generate UUID
    cast(conjure(session(start)), Result),
    assertion(Result = ok(session_started(_))), !.

test(session_start_with_id, [cleanup(reset_to_main)]) :-
    % Session start with specific ID should create named session
    cast(conjure(session(start('test-session-1'))), Result),
    assertion(Result = ok(session_started('test-session-1'))),
    
    % Verify workspace was created
    session_workspace_path('test-session-1', WorkspacePath),
    assertion(exists_directory(WorkspacePath)),
    
    % Verify database was created (.db extension)
    session_commands_db_path('test-session-1', DbPath),
    assertion(exists_file(DbPath)),
    
    % Verify state file was created
    session_state_file_path('test-session-1', StatePath),
    assertion(exists_file(StatePath)), !.

reset_to_main :-
    set_current_session_id(main).

test(session_history_main_session) :-
    % Main session should return empty history
    perceive(session(history(Commands))),
    assertion(Commands = []), !.

test(commit_accumulated_placeholder) :-
    % Commit accumulated should work as placeholder
    cast(conjure(session(commit_accumulated("Test commit"))), Result),
    assertion(Result = ok(commit_placeholder("Test commit"))).

% === THINK COMMAND TESTS ===

test(think_command_string) :-
    % Think command should work with strings
    cast(conjure(think("Test thought")), Result),
    assertion(Result = ok(thought_recorded("Test thought"))), !.

test(think_command_atom) :-
    % Think command should convert atoms to strings
    cast(conjure(think(test_atom)), Result),
    assertion(Result = ok(thought_recorded("test_atom"))).

% === DATABASE FUNCTIONALITY ===

test(database_schema_creation) :-
    % Starting a session should create proper database schema
    cast(conjure(session(start('test-db-schema'))), _),
    
    % Check that database file exists
    session_commands_db_path('test-db-schema', DbPath),
    assertion(exists_file(DbPath)),
    
    % Verify we can query the schema using sqlite3 CLI
    catch(
        (sqlite3_query(DbPath, 'SELECT name FROM sqlite_master WHERE type=\'table\' AND name=\'commands\'', Results),
         assertion(is_list(Results)),
         length(Results, 1)),  % Should find exactly one 'commands' table
        Error,
        (format('Schema test failed: ~w~n', [Error]), fail)
    ), !.

test(command_logging, [cleanup(cleanup_session('test-logging'))]) :-
    % Test that commands can be logged to session database
    cleanup_session('test-logging'),  % Clean up any previous run
    cast(conjure(session(start('test-logging'))), _),
    
    % Log a test command directly
    log_command_to_session_db('test-logging', action, test_command, ok(success)),
    
    % Verify command was logged
    session_commands_db_path('test-logging', DbPath),
    sqlite3_query(DbPath, 'SELECT COUNT(*) AS count FROM commands WHERE command_type = \'action\'', Results),
    Results = [CountDict|_],
    get_dict(count, CountDict, Count),
    assertion(Count = 1), !.

test(session_history_retrieval, [cleanup(cleanup_session('test-history'))]) :-
    % Test retrieving command history from database
    cleanup_session('test-history'),  % Clean up any previous run
    cast(conjure(session(start('test-history'))), _),
    
    % Add some test commands
    log_command_to_session_db('test-history', think, "test thought", ok(result)),
    log_command_to_session_db('test-history', action, test_action, ok(result)),
    
    % Retrieve history
    get_session_command_history('test-history', Commands),
    
    % Should have 2 commands (in reverse chronological order)
    length(Commands, 2),
    Commands = [command_entry(_, CT1, _, _, _), command_entry(_, CT2, _, _, _)],
    assertion(CT1 = "action"),
    assertion(CT2 = "think"), !.

% === PATH RESOLUTION TESTS ===

test(workspace_path_resolution) :-
    session_workspace_path('test-session', WorkspacePath),
    grimoire_data_directory(DataDir),
    format(atom(ExpectedPath), '~w/sessions/test-session', [DataDir]),
    assertion(WorkspacePath = ExpectedPath).

test(database_path_resolution) :-
    session_commands_db_path('test-session', DbPath),
    session_workspace_path('test-session', WorkspacePath),
    format(atom(ExpectedPath), '~w/commands.db', [WorkspacePath]),
    assertion(DbPath = ExpectedPath).

test(state_file_path_resolution) :-
    session_state_file_path('test-session', StatePath),
    session_workspace_path('test-session', WorkspacePath),
    format(atom(ExpectedPath), '~w/state.pl', [WorkspacePath]),
    assertion(StatePath = ExpectedPath).

% === SESSION STATE TESTS ===

test(session_state_file_initialization) :-
    % Test that session state file is properly initialized
    cast(conjure(session(start('test-state'))), _),
    
    session_state_file_path('test-state', StatePath),
    assertion(exists_file(StatePath)),
    
    % State file should contain session comment
    read_file_to_string(StatePath, Content, []),
    assertion(sub_string(Content, _, _, _, 'Session state file for session test-state')), !.

test(add_entity_load_to_session) :-
    % Test adding entity loads to session state
    cast(conjure(session(start('test-entity'))), _),
    
    add_entity_load_to_session('test-entity', semantic(folder(test))),
    
    session_state_file_path('test-entity', StatePath),
    read_file_to_string(StatePath, Content, []),
    assertion(sub_string(Content, _, _, _, 'load_entity(semantic(folder(test)))')), !.

test(load_session_state_file) :-
    % Test loading session state file
    cast(conjure(session(start('test-load-state'))), _),
    
    % Add a test fact to state file
    session_state_file_path('test-load-state', StatePath),
    open(StatePath, append, Stream),
    format(Stream, 'test_fact(loaded).~n', []),
    close(Stream),
    
    % Load the state file
    load_session_state_file('test-load-state'),
    
    % Verify fact was loaded
    assertion(test_fact(loaded)), !.

:- end_tests(session_system).