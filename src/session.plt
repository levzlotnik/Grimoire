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
    user:magic_cast(conjure(session(create(id(SessionId1)))), CreateResult1),
    assertion(CreateResult1 = ok(session(id(SessionId1), path(_)))),

    user:magic_cast(conjure(session(create(id(SessionId2)))), CreateResult2),
    assertion(CreateResult2 = ok(session(id(SessionId2), path(_)))),

    % Switch to first session
    user:magic_cast(conjure(session(switch(id(SessionId1)))), SwitchResult1),
    assertion(SwitchResult1 = ok(session(switched(id(SessionId1))))),

    % Verify current session
    user:current_session_id(CurrentId1),
    assertion(CurrentId1 = SessionId1),

    % Switch to second session
    user:magic_cast(conjure(session(switch(id(SessionId2)))), SwitchResult2),
    assertion(SwitchResult2 = ok(session(switched(id(SessionId2))))),

    % Verify current session changed
    user:current_session_id(CurrentId2),
    assertion(CurrentId2 = SessionId2),

    % Cleanup: delete sessions
    user:magic_cast(conjure(session(switch(id(SessionId1)))), SwitchResult3),
    assertion(SwitchResult3 = ok(session(switched(id(SessionId1))))),
    user:magic_cast(conjure(session(delete(id(SessionId2)))), DeleteResult2),
    assertion(DeleteResult2 = ok(session(deleted(id(SessionId2))))),
    user:magic_cast(conjure(session(delete(id(SessionId1)))), DeleteResult1),
    assertion(DeleteResult1 = ok(session(deleted(id(SessionId1))))).

test(session_delete_active_clears_cli_session, [
    setup(setup_grimoire_data),
    cleanup(cleanup_grimoire_data)
]) :-
    % Create a session
    SessionId = test_session_delete_active,
    user:magic_cast(conjure(session(create(id(SessionId)))), CreateResult),
    assertion(CreateResult = ok(session(id(SessionId), path(_)))),
    user:magic_cast(conjure(session(switch(id(SessionId)))), SwitchResult),
    assertion(SwitchResult = ok(session(switched(id(SessionId))))),

    % Verify it's the active session
    user:current_session_id(CurrentId),
    assertion(CurrentId = SessionId),

    % Get session path before deletion
    user:component(session(SessionId), session_directory, SessionPath),

    % Delete active session (should succeed and clear cli-session)
    user:magic_cast(conjure(session(delete(id(SessionId)))), DeleteResult),
    assertion(DeleteResult = ok(session(deleted(id(SessionId))))),

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
    user:magic_cast(conjure(session(create(id(SessionId)))), CreateResult),
    assertion(CreateResult = ok(session(id(SessionId), path(_)))),

    % Export session
    TmpDir = '/tmp',
    user:magic_cast(conjure(session(export(id(SessionId), destination(TmpDir)))), ExportResult),
    ExportResult = ok(session(exported(archive(ArchivePath)))),
    assertion(exists_file(ArchivePath)),

    % Get session path before deletion
    user:component(session(SessionId), session_directory, SessionPath),

    % Delete original session
    user:magic_cast(conjure(session(delete(id(SessionId)))), DeleteResult),
    assertion(DeleteResult = ok(session(deleted(id(SessionId))))),
    assertion(\+ exists_directory(SessionPath)),

    % Import session back
    user:magic_cast(conjure(session(import(archive(ArchivePath)))), ImportResult),
    assertion(ImportResult = ok(session(imported(id(SessionId))))),

    % Verify session was restored using components
    user:please_verify(component(session(SessionId), session_directory, _)),
    user:please_verify(component(session(SessionId), session_semantics_path, _)),

    % Cleanup
    delete_file(ArchivePath),
    user:magic_cast(conjure(session(delete(id(SessionId)))), FinalDeleteResult),
    assertion(FinalDeleteResult = ok(session(deleted(id(SessionId))))).

test(session_load_entity_persistence, [
    setup(setup_grimoire_data),
    cleanup(cleanup_grimoire_data)
]) :-
    % Create a session
    SessionId = test_session_load_entity,
    user:magic_cast(conjure(session(create(id(SessionId)))), CreateResult),
    assertion(CreateResult = ok(session(id(SessionId), path(_)))),
    user:magic_cast(conjure(session(switch(id(SessionId)))), SwitchResult),
    assertion(SwitchResult = ok(session(switched(id(SessionId))))),

    % Create a test semantic file to load
    TestSemanticFile = '/tmp/test_semantic.pl',
    open(TestSemanticFile, write, Stream),
    write(Stream, ':- self_entity(test_loaded_entity).\n'),
    write(Stream, 'entity(test_loaded_entity).\n'),
    close(Stream),

    % Load entity into session
    user:magic_cast(conjure(session(load_entity(semantic(file(TestSemanticFile))))), LoadResult),
    assertion(LoadResult = ok(semantic(file(TestSemanticFile)))),

    % Verify it was persisted to semantics.pl using component
    user:please_verify(component(session(SessionId), session_semantics_path, SemanticsPath)),
    read_file_to_string(SemanticsPath, SemanticsContent, []),
    assertion(sub_string(SemanticsContent, _, _, _, "has(loaded_entity)")),

    % Verify component exists
    user:please_verify(component(session(SessionId), has(loaded_entity), semantic(file(TestSemanticFile)))),

    % Cleanup
    delete_file(TestSemanticFile),
    user:magic_cast(conjure(session(delete(id(SessionId)))), DeleteResult),
    assertion(DeleteResult = ok(session(deleted(id(SessionId))))).

test(session_command_history, [
    setup(setup_grimoire_data),
    cleanup(cleanup_grimoire_data)
]) :-
    % Create a session
    SessionId = test_session_history,
    user:magic_cast(conjure(session(create(id(SessionId)))), CreateResult),
    assertion(CreateResult = ok(session(id(SessionId), path(_)))),
    user:magic_cast(conjure(session(switch(id(SessionId)))), SwitchResult1),
    assertion(SwitchResult1 = ok(session(switched(id(SessionId))))),

    % Execute some spells to generate history
    % (spells are auto-logged to activity_log.jsonl)
    user:magic_cast(conjure(session(switch(id(SessionId)))), SwitchResult2),
    assertion(SwitchResult2 = ok(session(switched(id(SessionId))))),
    user:magic_cast(conjure(session(switch(id(SessionId)))), SwitchResult3),
    assertion(SwitchResult3 = ok(session(switched(id(SessionId))))),

    % Query history
    user:magic_cast(perceive(session(history(options([limit(5)])))), HistoryResult),
    HistoryResult = ok(history(commands(Commands))),
    assertion(is_list(Commands)),
    length(Commands, Len),
    assertion(Len >= 2),

    % Cleanup
    user:magic_cast(conjure(session(delete(id(SessionId)))), DeleteResult),
    assertion(DeleteResult = ok(session(deleted(id(SessionId))))).

test(session_persistent_spell_verification, [
    setup(setup_grimoire_data),
    cleanup(cleanup_grimoire_data)
]) :-
    % Create a session and database
    SessionId = test_session_persistent,
    user:magic_cast(conjure(session(create(id(SessionId)))), CreateResult),
    assertion(CreateResult = ok(session(id(SessionId), path(_)))),
    user:magic_cast(conjure(session(switch(id(SessionId)))), SwitchResult),
    assertion(SwitchResult = ok(session(switched(id(SessionId))))),

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
    user:magic_cast(conjure(session(delete(id(SessionId)))), DeleteResult),
    assertion(DeleteResult = ok(session(deleted(id(SessionId))))).

test(session_unload_entity_works, [
    setup(setup_grimoire_data),
    cleanup(cleanup_grimoire_data)
]) :-
    % Create a session
    SessionId = test_session_unload,
    user:magic_cast(conjure(session(create(id(SessionId)))), CreateResult),
    assertion(CreateResult = ok(session(id(SessionId), path(_)))),
    user:magic_cast(conjure(session(switch(id(SessionId)))), SwitchResult),
    assertion(SwitchResult = ok(session(switched(id(SessionId))))),

    % Create a test semantic file to load
    TestSemanticFile = '/tmp/test_unload_entity.pl',
    open(TestSemanticFile, write, Stream),
    write(Stream, ':- self_entity(test_unload_entity).\n'),
    write(Stream, 'entity(test_unload_entity).\n'),
    close(Stream),

    % Load entity into session
    user:magic_cast(conjure(session(load_entity(semantic(file(TestSemanticFile))))), LoadResult),
    assertion(LoadResult = ok(semantic(file(TestSemanticFile)))),

    % Unload entity
    user:magic_cast(conjure(session(unload_entity(semantic(file(TestSemanticFile))))), UnloadResult),
    assertion(UnloadResult = ok(unloaded(semantic(file(TestSemanticFile))))),

    % Verify it was removed from session tracking
    \+ user:component(session(SessionId), has(loaded_entity), semantic(file(TestSemanticFile))),

    % Cleanup
    delete_file(TestSemanticFile),
    user:magic_cast(conjure(session(delete(id(SessionId)))), DeleteResult),
    assertion(DeleteResult = ok(session(deleted(id(SessionId))))).

test(session_focus_entity_by_name, [
    setup(setup_grimoire_data),
    cleanup(cleanup_grimoire_data)
]) :-
    % Create a test entity
    TestSemanticFile = '/tmp/test_focus_entity.pl',
    open(TestSemanticFile, write, Stream),
    write(Stream, ':- self_entity(test_focus_entity).\n'),
    write(Stream, 'entity(test_focus_entity).\n'),
    close(Stream),
    load_entity(semantic(file(TestSemanticFile))),

    % Create a session
    SessionId = test_session_focus_entity,
    user:magic_cast(conjure(session(create(id(SessionId)))), CreateResult),
    assertion(CreateResult = ok(session(id(SessionId), path(_)))),
    user:magic_cast(conjure(session(switch(id(SessionId)))), SwitchResult),
    assertion(SwitchResult = ok(session(switched(id(SessionId))))),

    % Focus on test entity
    user:magic_cast(conjure(session(focus_entity(entity(test_focus_entity)))), FocusResult),
    assertion(FocusResult = ok(focused(entity(test_focus_entity)))),

    % Verify focus was persisted to session
    user:please_verify(component(session(SessionId), focused_entity, test_focus_entity)),

    % Cleanup
    user:magic_cast(conjure(session(delete(id(SessionId)))), DeleteResult),
    assertion(DeleteResult = ok(session(deleted(id(SessionId))))),
    unload_entity(semantic(file(TestSemanticFile))),
    delete_file(TestSemanticFile).

test(session_focus_by_path, [
    setup(setup_grimoire_data),
    cleanup(cleanup_grimoire_data)
]) :-
    % Create a session
    SessionId = test_session_focus_path,
    user:magic_cast(conjure(session(create(id(SessionId)))), CreateResult),
    assertion(CreateResult = ok(session(id(SessionId), path(_)))),
    user:magic_cast(conjure(session(switch(id(SessionId)))), SwitchResult),
    assertion(SwitchResult = ok(session(switched(id(SessionId))))),

    % Create a test directory and entity with self component
    TestDir = '/tmp/test_focus_path_dir',
    (exists_directory(TestDir) -> delete_directory_and_contents(TestDir) ; true),
    make_directory(TestDir),

    TestSemanticFile = '/tmp/test_focus_path_entity.pl',
    open(TestSemanticFile, write, Stream),
    write(Stream, ':- self_entity(test_focus_path_entity).\n'),
    write(Stream, 'entity(test_focus_path_entity).\n'),
    format(Stream, 'component(test_focus_path_entity, self, semantic(folder(\'~w\'))).\n', [TestDir]),
    close(Stream),

    % Load the test entity
    load_entity(semantic(file(TestSemanticFile))),

    % Focus by path
    user:magic_cast(conjure(session(focus_path(path(TestDir)))), FocusResult),
    assertion(FocusResult = ok(focused(entity(test_focus_path_entity)))),

    % Verify focus was persisted
    user:component(session(SessionId), focused_entity, FocusedEntity),
    assertion(FocusedEntity = test_focus_path_entity),

    % Cleanup
    unload_entity(semantic(file(TestSemanticFile))),
    delete_file(TestSemanticFile),
    delete_directory_and_contents(TestDir),
    user:magic_cast(conjure(session(delete(id(SessionId)))), DeleteResult),
    assertion(DeleteResult = ok(session(deleted(id(SessionId))))).

test(session_get_focused_structured_output, [
    setup(setup_grimoire_data),
    cleanup(cleanup_grimoire_data)
]) :-
    % Create a test entity
    TestSemanticFile = '/tmp/test_get_focused_entity.pl',
    open(TestSemanticFile, write, Stream),
    write(Stream, ':- self_entity(test_get_focused_entity).\n'),
    write(Stream, 'entity(test_get_focused_entity).\n'),
    close(Stream),
    load_entity(semantic(file(TestSemanticFile))),

    % Create a session
    SessionId = test_session_get_focused,
    user:magic_cast(conjure(session(create(id(SessionId)))), CreateResult),
    assertion(CreateResult = ok(session(id(SessionId), path(_)))),
    user:magic_cast(conjure(session(switch(id(SessionId)))), SwitchResult),
    assertion(SwitchResult = ok(session(switched(id(SessionId))))),

    % Focus on test entity
    user:magic_cast(conjure(session(focus_entity(entity(test_get_focused_entity)))), FocusResult),
    assertion(FocusResult = ok(focused(entity(test_get_focused_entity)))),

    % Get focused entity with structured output
    user:magic_cast(perceive(session(focused)), GetFocusedResult),
    GetFocusedResult = ok(focused_entity(entity(test_get_focused_entity), loc(_Location), key_comps(_KeyComps))),

    % Cleanup
    user:magic_cast(conjure(session(delete(id(SessionId)))), DeleteResult),
    assertion(DeleteResult = ok(session(deleted(id(SessionId))))),
    unload_entity(semantic(file(TestSemanticFile))),
    delete_file(TestSemanticFile).

test(session_unfocus, [
    setup(setup_grimoire_data),
    cleanup(cleanup_grimoire_data)
]) :-
    % Create a test entity
    TestSemanticFile = '/tmp/test_unfocus_entity.pl',
    open(TestSemanticFile, write, Stream),
    write(Stream, ':- self_entity(test_unfocus_entity).\n'),
    write(Stream, 'entity(test_unfocus_entity).\n'),
    close(Stream),
    load_entity(semantic(file(TestSemanticFile))),

    % Create a session
    SessionId = test_session_unfocus,
    user:magic_cast(conjure(session(create(id(SessionId)))), CreateResult),
    assertion(CreateResult = ok(session(id(SessionId), path(_)))),
    user:magic_cast(conjure(session(switch(id(SessionId)))), SwitchResult),
    assertion(SwitchResult = ok(session(switched(id(SessionId))))),

    % Focus on test entity
    user:magic_cast(conjure(session(focus_entity(entity(test_unfocus_entity)))), FocusResult),
    assertion(FocusResult = ok(focused(entity(test_unfocus_entity)))),

    % Verify focus exists
    user:component(session(SessionId), focused_entity, test_unfocus_entity),

    % Unfocus
    user:magic_cast(conjure(session(unfocus)), UnfocusResult),
    assertion(UnfocusResult = ok(unfocused)),

    % Verify focus was removed
    \+ user:component(session(SessionId), focused_entity, _),

    % Cleanup
    user:magic_cast(conjure(session(delete(id(SessionId)))), DeleteResult),
    assertion(DeleteResult = ok(session(deleted(id(SessionId))))),
    unload_entity(semantic(file(TestSemanticFile))),
    delete_file(TestSemanticFile).

test(session_status_shows_focused_entity, [
    setup(setup_grimoire_data),
    cleanup(cleanup_grimoire_data)
]) :-
    % Create a test entity
    TestSemanticFile = '/tmp/test_status_entity.pl',
    open(TestSemanticFile, write, Stream),
    write(Stream, ':- self_entity(test_status_entity).\n'),
    write(Stream, 'entity(test_status_entity).\n'),
    close(Stream),
    load_entity(semantic(file(TestSemanticFile))),

    % Create a session
    SessionId = test_session_status_focused,
    user:magic_cast(conjure(session(create(id(SessionId)))), CreateResult),
    assertion(CreateResult = ok(session(id(SessionId), path(_)))),
    user:magic_cast(conjure(session(switch(id(SessionId)))), SwitchResult),
    assertion(SwitchResult = ok(session(switched(id(SessionId))))),

    % Get status with no focus
    user:magic_cast(perceive(session(status)), StatusResult1),
    StatusResult1 = ok(session_status(id(SessionId), path(_), focused(none), active(true))),

    % Focus on test entity
    user:magic_cast(conjure(session(focus_entity(entity(test_status_entity)))), FocusResult),
    assertion(FocusResult = ok(focused(entity(test_status_entity)))),

    % Get status with focus
    user:magic_cast(perceive(session(status)), StatusResult2),
    StatusResult2 = ok(session_status(id(SessionId), path(_), focused(entity(test_status_entity)), active(true))),

    % Cleanup
    user:magic_cast(conjure(session(delete(id(SessionId)))), DeleteResult),
    assertion(DeleteResult = ok(session(deleted(id(SessionId))))),
    unload_entity(semantic(file(TestSemanticFile))),
    delete_file(TestSemanticFile).

test(session_focus_nonexistent_entity_fails, [
    setup(setup_grimoire_data),
    cleanup(cleanup_grimoire_data)
]) :-
    % Create a session
    SessionId = test_session_focus_error,
    user:magic_cast(conjure(session(create(id(SessionId)))), CreateResult),
    assertion(CreateResult = ok(session(id(SessionId), path(_)))),
    user:magic_cast(conjure(session(switch(id(SessionId)))), SwitchResult),
    assertion(SwitchResult = ok(session(switched(id(SessionId))))),

    % Try to focus on nonexistent entity - should return error
    user:magic_cast(conjure(session(focus_entity(entity(nonexistent_entity_xyz)))), FocusResult),
    assertion(FocusResult = error(focus_error(entity_not_found(nonexistent_entity_xyz)))),

    % Cleanup
    user:magic_cast(conjure(session(delete(id(SessionId)))), DeleteResult),
    assertion(DeleteResult = ok(session(deleted(id(SessionId))))).

test(session_focus_path_no_entity_fails, [
    setup(setup_grimoire_data),
    cleanup(cleanup_grimoire_data)
]) :-
    % Create a session
    SessionId = test_session_focus_path_error,
    user:magic_cast(conjure(session(create(id(SessionId)))), CreateResult),
    assertion(CreateResult = ok(session(id(SessionId), path(_)))),
    user:magic_cast(conjure(session(switch(id(SessionId)))), SwitchResult),
    assertion(SwitchResult = ok(session(switched(id(SessionId))))),

    % Create a directory with no entity
    TestDir = '/tmp/test_no_entity_dir',
    (exists_directory(TestDir) -> delete_directory_and_contents(TestDir) ; true),
    make_directory(TestDir),

    % Try to focus by path - should return error
    user:magic_cast(conjure(session(focus_path(path(TestDir)))), FocusResult),
    assertion(FocusResult = error(focus_error(no_entity_for_path(TestDir)))),

    % Cleanup
    delete_directory_and_contents(TestDir),
    user:magic_cast(conjure(session(delete(id(SessionId)))), DeleteResult),
    assertion(DeleteResult = ok(session(deleted(id(SessionId))))).

test(session_activity_log_empty, [
    setup(setup_grimoire_data),
    cleanup(cleanup_grimoire_data)
]) :-
    % Create a session
    SessionId = test_activity_empty,
    user:magic_cast(conjure(session(create(id(SessionId)))), CreateResult),
    assertion(CreateResult = ok(_)),
    user:magic_cast(conjure(session(switch(id(SessionId)))), SwitchResult),
    assertion(SwitchResult = ok(_)),

    % Get activity log - should only have switch (which was just performed)
    user:magic_cast(perceive(session(activity_log)), ActivityResult),
    assertion(ActivityResult = ok(activity_log([spell_activity(conjure(session), 1)]))),

    % Cleanup
    user:magic_cast(conjure(session(delete(id(SessionId)))), DeleteResult),
    assertion(DeleteResult = ok(_)).

test(session_activity_log_counts_spells, [
    setup(setup_grimoire_data),
    cleanup(cleanup_grimoire_data)
]) :-
    % Create a session with some activity
    SessionId = test_activity_counts,
    user:magic_cast(conjure(session(create(id(SessionId)))), CreateResult),
    assertion(CreateResult = ok(_)),
    user:magic_cast(conjure(session(switch(id(SessionId)))), SwitchResult),
    assertion(SwitchResult = ok(_)),

    % Perform some spells that will be logged
    user:magic_cast(perceive(interface(entities)), R1),
    assertion(R1 = ok(_)),
    user:magic_cast(perceive(interface(entities)), R2),
    assertion(R2 = ok(_)),
    user:magic_cast(perceive(interface(entities)), R3),
    assertion(R3 = ok(_)),

    % Get activity log - should have counts
    user:magic_cast(perceive(session(activity_log)), ok(activity_log(Activities))),
    assertion(Activities \= []),

    % Verify we have multiple spell types logged
    % Should have: 1 conjure(session) for switch, 3+ perceive(interface) for entities
    member(spell_activity(perceive(interface), PerceiveCount), Activities),
    assertion(PerceiveCount >= 3),

    % Cleanup
    user:magic_cast(conjure(session(delete(id(SessionId)))), DeleteResult),
    assertion(DeleteResult = ok(_)).

test(session_context_no_focused_entity, [
    setup(setup_grimoire_data),
    cleanup(cleanup_grimoire_data)
]) :-
    % Create a session without focusing
    SessionId = test_context_no_focus,
    user:magic_cast(conjure(session(create(id(SessionId)))), CreateResult),
    assertion(CreateResult = ok(_)),
    user:magic_cast(conjure(session(switch(id(SessionId)))), SwitchResult),
    assertion(SwitchResult = ok(_)),

    % Get context - should have no focused entity
    user:magic_cast(perceive(session(context)), ContextResult),
    assertion(ContextResult = ok(session(SessionId), focused(none, components_values([])), common_activity(_))),

    % Cleanup
    user:magic_cast(conjure(session(delete(id(SessionId)))), DeleteResult),
    assertion(DeleteResult = ok(_)).

test(session_context_with_focused_entity, [
    setup(setup_grimoire_data),
    cleanup(cleanup_grimoire_data)
]) :-
    % Create a test entity
    TestSemanticFile = '/tmp/test_context_entity.pl',
    open(TestSemanticFile, write, Stream),
    write(Stream, ':- self_entity(test_context_entity).\n'),
    write(Stream, 'entity(test_context_entity).\n'),
    write(Stream, 'component(test_context_entity, test_comp, test_value).\n'),
    close(Stream),
    load_entity(semantic(file(TestSemanticFile))),

    % Create session and focus on entity
    SessionId = test_context_focus,
    user:magic_cast(conjure(session(create(id(SessionId)))), CreateResult),
    assertion(CreateResult = ok(_)),
    user:magic_cast(conjure(session(switch(id(SessionId)))), SwitchResult),
    assertion(SwitchResult = ok(_)),
    user:magic_cast(conjure(session(focus_entity(entity(test_context_entity)))), FocusResult),
    assertion(FocusResult = ok(_)),

    % Get context - should have focused entity with components
    user:magic_cast(perceive(session(context)), ok(session(SessionId), focused(entity(test_context_entity), components_values(ComponentValues)), common_activity(_))),
    assertion(ComponentValues \= []),

    % Cleanup
    unload_entity(semantic(file(TestSemanticFile))),
    delete_file(TestSemanticFile),
    user:magic_cast(conjure(session(delete(id(SessionId)))), DeleteResult),
    assertion(DeleteResult = ok(_)).

:- end_tests(session).
