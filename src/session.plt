:- begin_tests(session).

:- use_module(library(filesex)).

% === TEST SETUP/CLEANUP ===

% Setup: Create test GRIMOIRE_DATA directory
setup_grimoire_data :-
    TestDataPath = '/tmp/grimoire_test_data',
    (exists_directory(TestDataPath)
    -> delete_directory_and_contents(TestDataPath)
    ; true),
    make_directory(TestDataPath),
    setenv('GRIMOIRE_DATA', TestDataPath).

% Cleanup: Remove test GRIMOIRE_DATA directory
cleanup_grimoire_data :-
    TestDataPath = '/tmp/grimoire_test_data',
    (exists_directory(TestDataPath)
    -> delete_directory_and_contents(TestDataPath)
    ; true),
    unsetenv('GRIMOIRE_DATA').

% === TESTS ===

test(session_create_and_verify, [
    setup(setup_grimoire_data),
    cleanup(cleanup_grimoire_data)
]) :-
    % Create a new session
    SessionId = test_session_create,
    user:magic_cast(conjure(session(create(id(SessionId)))), CreateResult),
    assertion(CreateResult = ok(session(id(SessionId), path(_)))),

    % Verify session components exist and are valid
    user:please_verify(component(session(SessionId), session_directory, SessionPath)),
    user:please_verify(component(session(SessionId), session_semantics_path, _)),
    user:please_verify(component(session(SessionId), session_activity_log_path, _)),

    % Delete session
    user:magic_cast(conjure(session(delete(id(SessionId)))), DeleteResult),
    assertion(DeleteResult = ok(session(deleted(id(SessionId))))),
    assertion(\+ exists_directory(SessionPath)).

test(session_switch, [
    setup(setup_grimoire_data),
    cleanup(cleanup_grimoire_data)
]) :-
    % Create two sessions
    SessionId1 = test_session_switch_1,
    SessionId2 = test_session_switch_2,
    user:magic_cast(conjure(session(create(id(SessionId1)))), _),
    user:magic_cast(conjure(session(create(id(SessionId2)))), _),

    % Switch to first session
    user:magic_cast(conjure(session(switch(id(SessionId1)))), Result1),
    assertion(Result1 = ok(session(switched(id(SessionId1))))),

    % Verify current session
    user:current_session_id(CurrentId1),
    assertion(CurrentId1 = SessionId1),

    % Switch to second session
    user:magic_cast(conjure(session(switch(id(SessionId2)))), Result2),
    assertion(Result2 = ok(session(switched(id(SessionId2))))),

    % Verify current session changed
    user:current_session_id(CurrentId2),
    assertion(CurrentId2 = SessionId2),

    % Cleanup: delete sessions
    user:magic_cast(conjure(session(switch(id(SessionId1)))), _),
    user:magic_cast(conjure(session(delete(id(SessionId2)))), _),
    user:magic_cast(conjure(session(delete(id(SessionId1)))), _).

test(session_delete_active_clears_cli_session, [
    setup(setup_grimoire_data),
    cleanup(cleanup_grimoire_data)
]) :-
    % Create a session
    SessionId = test_session_delete_active,
    user:magic_cast(conjure(session(create(id(SessionId)))), _),
    user:magic_cast(conjure(session(switch(id(SessionId)))), _),

    % Verify it's the active session
    user:current_session_id(CurrentId),
    assertion(CurrentId = SessionId),

    % Get session path before deletion
    user:component(session(SessionId), session_directory, SessionPath),

    % Delete active session (should succeed and clear cli-session)
    user:magic_cast(conjure(session(delete(id(SessionId)))), Result),
    assertion(Result = ok(session(deleted(id(SessionId))))),

    % Verify session directory was deleted
    assertion(\+ exists_directory(SessionPath)),

    % Verify cli-session file was cleared
    user:grimoire_data_path(DataPath),
    atomic_list_concat([DataPath, '/cli-session'], CliSessionPath),
    assertion(\+ exists_file(CliSessionPath)).

test(session_export_import, [
    setup(setup_grimoire_data),
    cleanup(cleanup_grimoire_data)
]) :-
    % Create a session
    SessionId = test_session_export,
    user:magic_cast(conjure(session(create(id(SessionId)))), _),

    % Export session
    TmpDir = '/tmp',
    user:magic_cast(conjure(session(export(id(SessionId), destination(TmpDir)))), ExportResult),
    ExportResult = ok(session(exported(archive(ArchivePath)))),
    assertion(exists_file(ArchivePath)),

    % Get session path before deletion
    user:component(session(SessionId), session_directory, SessionPath),

    % Delete original session
    user:magic_cast(conjure(session(delete(id(SessionId)))), _),
    assertion(\+ exists_directory(SessionPath)),

    % Import session back
    user:magic_cast(conjure(session(import(archive(ArchivePath)))), ImportResult),
    assertion(ImportResult = ok(session(imported(id(SessionId))))),

    % Verify session was restored using components
    user:please_verify(component(session(SessionId), session_directory, _)),
    user:please_verify(component(session(SessionId), session_semantics_path, _)),

    % Cleanup
    delete_file(ArchivePath),
    user:magic_cast(conjure(session(delete(id(SessionId)))), _).

test(session_load_entity_persistence, [
    setup(setup_grimoire_data),
    cleanup(cleanup_grimoire_data)
]) :-
    % Create a session
    SessionId = test_session_load_entity,
    user:magic_cast(conjure(session(create(id(SessionId)))), _),
    user:magic_cast(conjure(session(switch(id(SessionId)))), _),

    % Create a test semantic file to load
    TestSemanticFile = '/tmp/test_semantic.pl',
    open(TestSemanticFile, write, Stream),
    write(Stream, ':- self_entity(test_loaded_entity).\n'),
    write(Stream, 'entity(test_loaded_entity).\n'),
    close(Stream),

    % Load entity into session
    user:magic_cast(conjure(session(load_entity(semantic(file(TestSemanticFile))))), Result),
    assertion(Result = ok(semantic(file(TestSemanticFile)))),

    % Verify it was persisted to semantics.pl using component
    user:please_verify(component(session(SessionId), session_semantics_path, SemanticsPath)),
    read_file_to_string(SemanticsPath, SemanticsContent, []),
    assertion(sub_string(SemanticsContent, _, _, _, "has(loaded_entity)")),

    % Verify component exists
    user:please_verify(component(session(SessionId), has(loaded_entity), semantic(file(TestSemanticFile)))),

    % Cleanup
    delete_file(TestSemanticFile),
    user:magic_cast(conjure(session(delete(id(SessionId)))), _).

test(session_command_history, [
    setup(setup_grimoire_data),
    cleanup(cleanup_grimoire_data)
]) :-
    % Create a session
    SessionId = test_session_history,
    user:magic_cast(conjure(session(create(id(SessionId)))), _),
    user:magic_cast(conjure(session(switch(id(SessionId)))), _),

    % Execute some spells to generate history
    % (spells are auto-logged to activity_log.jsonl)
    user:magic_cast(conjure(session(switch(id(SessionId)))), _),
    user:magic_cast(conjure(session(switch(id(SessionId)))), _),

    % Query history
    user:magic_cast(perceive(session(history(options([limit(5)])))), HistoryResult),
    HistoryResult = ok(history(commands(Commands))),
    assertion(is_list(Commands)),
    length(Commands, Len),
    assertion(Len >= 2),

    % Cleanup
    user:magic_cast(conjure(session(delete(id(SessionId)))), _).

test(session_persistent_spell_verification, [
    setup(setup_grimoire_data),
    cleanup(cleanup_grimoire_data)
]) :-
    % Create a session and database
    SessionId = test_session_persistent,
    user:magic_cast(conjure(session(create(id(SessionId)))), _),
    user:magic_cast(conjure(session(switch(id(SessionId)))), _),

    % Create a database (which has session_persistence flag)
    TestDbPath = '/tmp/test_session_db.db',
    (exists_file(TestDbPath) -> delete_file(TestDbPath) ; true),

    user:magic_cast(conjure(db(create(
        file(TestDbPath),
        schema(sql("CREATE TABLE test (id INTEGER)"))
    ))), DbResult),
    assertion(DbResult = ok(db(sqlite(file(TestDbPath))))),

    % Verify it was persisted to session using component
    user:component(session(SessionId), session_semantics_path, SemanticsPath),
    read_file_to_string(SemanticsPath, SemanticsContent, []),
    assertion(sub_string(SemanticsContent, _, _, _, "has(db(sqlite))")),

    % Verify component can be verified
    user:please_verify(component(session(SessionId), has(db(sqlite)), db(sqlite(file(TestDbPath))))),

    % Cleanup
    delete_file(TestDbPath),
    user:magic_cast(conjure(session(delete(id(SessionId)))), _).

test(session_unload_entity_not_implemented, [
    setup(setup_grimoire_data),
    cleanup(cleanup_grimoire_data)
]) :-
    % Create a session
    SessionId = test_session_unload,
    user:magic_cast(conjure(session(create(id(SessionId)))), _),
    user:magic_cast(conjure(session(switch(id(SessionId)))), _),

    % Try to unload entity (should throw not_implemented)
    catch(
        user:magic_cast(conjure(session(unload_entity(semantic(file('/tmp/test.pl'))))), _),
        error(not_implemented, _),
        true
    ),

    % Cleanup
    user:magic_cast(conjure(session(delete(id(SessionId)))), _).

:- end_tests(session).
