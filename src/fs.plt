:- use_module(library(plunit)).
:- use_module(library(filesex)).

% Load test entity from file-based knowledge
:- load_entity(semantic(file('@/src/tests/fs_test_entity.pl'))).

% === DISCRIMINATIVE FLOW: VERIFICATION RULES ===

% === VERIFICATION RULES FOR FS DSL PATTERNS ===

% Verify composite has(fs(file)) pattern (if it exists in the codebase)
verify(component(Entity, has(fs(file)), fs(file(_Spec)))) :-
    % Delegate to primitive verification
    please_verify(component(Entity, fs_file_path, Path)),
    % Check OS reality
    (exists_file(Path) ->
        true
    ;
        throw(verification_error(fs, file_not_found(Path)))
    ).

% Verify primitive fs_file_path component
verify(component(_Entity, fs_file_path, Path)) :-
    % Component existence already proven by please_verify
    % Check OS reality
    (exists_file(Path) ->
        true
    ;
        throw(verification_error(fs, file_not_found(Path)))
    ).

% Verify primitive fs_directory_path component
verify(component(_Entity, fs_directory_path, Path)) :-
    % Component existence already proven by please_verify
    % Check OS reality
    (exists_directory(Path) ->
        true
    ;
        throw(verification_error(fs, directory_not_found(Path)))
    ).

% Verify fs(structure) DSL pattern
verify(component(_Entity, has(fs(structure)), fs(structure(Items)))) :-
    % Extract file and folder specs directly (inline expansion instead of relying on component/3)
    extract_file_specs(Items, FileSpecs),
    extract_folder_specs(Items, FolderSpecs),
    % Verify each file and folder against actual filesystem
    forall(member(file_spec(Path, Opts), FileSpecs),
        verify_file_against_fs(Path, Opts)),
    forall(member(folder_spec(Path, _Contents), FolderSpecs),
        verify_folder_against_fs(Path)).

% Helper to verify a file against filesystem
verify_file_against_fs(Path, Options) :-
    (exists_file(Path) ->
        verify_file_options(Path, Options)
    ;
        throw(verification_error(fs, missing_file(Path)))
    ).

% Helper to verify a folder against filesystem
verify_folder_against_fs(Path) :-
    (exists_directory(Path) ->
        true
    ;
        throw(verification_error(fs, missing_folder(Path)))
    ).

% Verify fs(file_content) DSL pattern
verify(component(_Entity, has(fs(file_content)), fs(file_content(Path, Requirements)))) :-
    % Expansion component exists via generative flow, just verify semantics
    verify_content_against_filesystem(Path, Requirements).

% Verify fs(permissions) DSL pattern
verify(component(_Entity, has(fs(permissions)), fs(permissions(Path, PermType)))) :-
    % Expansion component exists via generative flow, just verify semantics
    verify_permissions_against_filesystem(Path, PermType).

% === PRIMITIVE VERIFICATION (AGAINST ACTUAL FILESYSTEM) ===

% Verify individual file specs against actual filesystem
verify(component(_Entity, fs_structure_file, file_spec(Path, Options))) :-
    (exists_file(Path) ->
        verify_file_options(Path, Options)
    ;
        throw(verification_error(fs, missing_file(Path)))
    ).

% Verify individual folder specs against actual filesystem
verify(component(_Entity, fs_structure_folder, folder_spec(Path, _Contents))) :-
    (exists_directory(Path) ->
        true
    ;
        throw(verification_error(fs, missing_folder(Path)))
    ).

% Verify content requirements
verify(component(_Entity, fs_content_requirement, content_spec(Path, Requirements))) :-
    (exists_file(Path) ->
        read_file_to_string(Path, Content, []),
        (verify_content_requirements(Content, Requirements) ->
            true
        ;
            throw(verification_error(fs, content_mismatch(Path, Requirements)))
        )
    ;
        throw(verification_error(fs, file_not_found(Path)))
    ).

% Verify permission requirements
verify(component(_Entity, fs_permission_requirement, permission_spec(Path, PermType))) :-
    (exists_file(Path) ->
        (verify_permission_type(Path, PermType) ->
            true
        ;
            throw(verification_error(fs, permission_mismatch(Path, PermType)))
        )
    ;
        throw(verification_error(fs, file_not_found(Path)))
    ).

% === VERIFICATION HELPER PREDICATES ===

% Verify filesystem structure matches reality
verify_filesystem_structure_reality(FileSpecs, FolderSpecs) :-
    % All files must exist
    forall(member(file_spec(Path, Options), FileSpecs),
        (exists_file(Path), verify_file_options(Path, Options))),
    % All folders must exist
    forall(member(folder_spec(Path, _), FolderSpecs),
        exists_directory(Path)).

% Verify file options (placeholder for now)
verify_file_options(_Path, []) :- !.
verify_file_options(Path, [Option|Rest]) :-
    verify_file_option(Path, Option),
    verify_file_options(Path, Rest).

verify_file_option(_Path, _Option) :-
    % Placeholder - can be extended with specific option checks
    true.

% Verify content requirements
verify_content_requirements(Content, contains(RequiredStrings)) :-
    % All required strings must be present in content
    forall(member(RequiredString, RequiredStrings),
        sub_string(Content, _, _, _, RequiredString)).

% Verify content against actual filesystem
verify_content_against_filesystem(Path, Requirements) :-
    (exists_file(Path) ->
        (read_file_to_string(Path, Content, []),
         (verify_content_requirements(Content, Requirements) ->
             true
         ;
             throw(verification_error(fs, content_mismatch(Path, Requirements)))
         ))
    ;
        throw(verification_error(fs, missing_file(Path)))
    ).

% Verify permission type
verify_permission_type(Path, executable) :-
    access_file(Path, execute).
verify_permission_type(Path, readable) :-
    access_file(Path, read).
verify_permission_type(Path, writable) :-
    access_file(Path, write).

% Verify permissions against actual filesystem
verify_permissions_against_filesystem(Path, PermType) :-
    verify_permission_type(Path, PermType).

% === PLUNIT TEST SUITE ===

:- begin_tests(fs).

% === DSL PATTERN TESTS ===

% Test fs(structure) verification with real files
test(fs_structure_verification, [
    setup(setup_fs_structure_test),
    cleanup(cleanup_fs_structure_test)
]) :-
    % Verify it exists
    user:please_verify(component(test_entity(fs), has(fs(structure)), fs(structure([
        file('test_file.txt'),
        folder('test_dir', [file('nested_file.txt')])
    ])))).

% Test fs(file_content) verification
test(fs_file_content_verification, [
    setup(setup_content_test),
    cleanup(cleanup_content_test)
]) :-
    user:please_verify(component(content_test_entity, has(fs(file_content)), fs(file_content(
        'content_test.txt', contains(["hello", "world"])
    )))).

% Test fs(permissions) verification on executable script
test(fs_permissions_verification, [
    setup(setup_permissions_test),
    cleanup(cleanup_permissions_test),
    condition(current_prolog_flag(unix, true))  % Only on Unix systems
]) :-
    user:please_verify(component(perm_test_entity, has(fs(permissions)), fs(permissions(
        'test_script.sh', executable
    )))).

% === SPELL OPERATION TESTS ===

% Test read_file spell
test(fs_read_file_spell, [
    setup(create_read_test_file),
    cleanup(cleanup_read_test_file)
]) :-
    user:magic_cast(perceive(fs(read_file('read_test.txt', 1, 2))), Result),
    assertion(Result = ok(file_content(Content))),
    length(Content, 2),
    Content = [line(1, "line 1"), line(2, "line 2")].

% Test edit_file spell - insert operation
test(fs_edit_file_insert_spell, [
    setup(create_edit_test_file),
    cleanup(cleanup_edit_test_file)
]) :-
    user:magic_cast(conjure(fs(edit_file(file('edit_test.txt'), [insert(2, "inserted line")]))), Result), !,
    assertion(Result == ok(file_modified('edit_test.txt'))),
    read_file_to_lines('edit_test.txt', Lines),
    assertion(Lines == ["original line 1", "inserted line", "original line 2", "original line 3"]).

% Test edit_file spell - append operation
test(fs_edit_file_append_spell, [
    setup(create_edit_test_file),
    cleanup(cleanup_edit_test_file)
]) :-
    user:magic_cast(conjure(fs(edit_file(file('edit_test.txt'), [append("appended line")]))), Result), !,
    assertion(Result == ok(file_modified('edit_test.txt'))),
    read_file_to_lines('edit_test.txt', Lines),
    assertion(nth1(4, Lines, "appended line")).

% Test edit_file spell - delete operation
test(fs_edit_file_delete_spell, [
    setup(create_edit_test_file),
    cleanup(cleanup_edit_test_file)
]) :-
    user:magic_cast(conjure(fs(edit_file(file('edit_test.txt'), [delete(2, 2)]))), Result), !,
    assertion(Result == ok(file_modified('edit_test.txt'))),
    read_file_to_lines('edit_test.txt', Lines),
    assertion(Lines == ["original line 1", "original line 3"]).

% Test edit_file spell - replace operation
test(fs_edit_file_replace_spell, [
    setup(create_edit_test_file),
    cleanup(cleanup_edit_test_file)
]) :-
    user:magic_cast(conjure(fs(edit_file(file('edit_test.txt'), [replace(2, 2, "replaced line")]))), Result), !,
    assertion(Result == ok(file_modified('edit_test.txt'))),
    read_file_to_lines('edit_test.txt', Lines),
    assertion(Lines == ["original line 1", "replaced line", "original line 3"]).

% Test mkdir spell
test(fs_mkdir_spell, [cleanup(cleanup_mkdir_test)]) :-
    user:magic_cast(conjure(fs(mkdir('test_mkdir_dir'))), Result), !,
    assertion(Result == ok(directory_created('test_mkdir_dir'))),
    assertion(exists_directory('test_mkdir_dir')).

% Test mkfile spell
test(fs_mkfile_spell, [cleanup(cleanup_mkfile_test)]) :-
    user:magic_cast(conjure(fs(mkfile('test_mkfile.txt'))), Result), !,
    assertion(Result == ok(file_created('test_mkfile.txt'))),
    assertion(exists_file('test_mkfile.txt')).

% === ASSERTZ→PLEASE_VERIFY→RETRACTALL PATTERN TESTS ===

% Test the canonical pattern with mock filesystem
test(verify_domain_component) :-
    % Entity already loaded from file, just verify it
    user:please_verify(component(mock_entity, fs_test_prop, test_value)).

% Test verification failure for missing file
test(verify_missing_file_throws, [
    setup(setup_missing_file_test),
    cleanup(cleanup_missing_file_test),
    throws(verification_error(fs, missing_file('nonexistent.txt')))
]) :-
    user:please_verify(component(fail_entity, has(fs(structure)), fs(structure([
        file('nonexistent.txt')
    ])))).

% Test verification failure for content mismatch
test(verify_content_mismatch_throws, [
    setup(setup_content_mismatch_test),
    cleanup(cleanup_content_mismatch_test),
    throws(verification_error(fs, content_mismatch('content_test.txt', contains(["missing"]))))
]) :-
    user:please_verify(component(content_fail_entity, has(fs(file_content)), fs(file_content(
        'content_test.txt', contains(["missing"])
    )))).

% Debug test - check if other entities still exist after fs tests
test(debug_entity_integrity) :-
    entity(nix),
    entity(utils),
    entity(db), !.

:- end_tests(fs).

% === TEST SETUP/CLEANUP HELPERS ===

% Setup test directory structure - just create the actual files
setup_fs_structure_test :-
    % Create test files that match the entity declaration
    open('test_file.txt', write, Stream1),
    write(Stream1, 'test content'),
    close(Stream1),
    make_directory('test_dir'),
    open('test_dir/nested_file.txt', write, Stream2),
    write(Stream2, 'nested content'),
    close(Stream2).

cleanup_fs_structure_test :-
    % Cleanup files only - entity persists in loaded file
    (exists_file('test_file.txt') -> delete_file('test_file.txt'); true),
    (exists_file('test_dir/nested_file.txt') -> delete_file('test_dir/nested_file.txt'); true),
    (exists_directory('test_dir') -> delete_directory('test_dir'); true).

create_test_structure :-
    open('test_file.txt', write, Stream1),
    write(Stream1, 'test content'),
    close(Stream1),
    make_directory('test_dir'),
    open('test_dir/nested_file.txt', write, Stream2),
    write(Stream2, 'nested content'),
    close(Stream2).

cleanup_test_structure :-
    (exists_file('test_file.txt') -> delete_file('test_file.txt'); true),
    (exists_file('test_dir/nested_file.txt') -> delete_file('test_dir/nested_file.txt'); true),
    (exists_directory('test_dir') -> delete_directory('test_dir'); true).

% Setup for content test
setup_content_test :-
    % Create test file
    open('content_test.txt', write, Stream),
    write(Stream, 'hello world from test file'),
    close(Stream).

cleanup_content_test :-
    % Cleanup file only
    (exists_file('content_test.txt') -> delete_file('content_test.txt'); true).

% Setup file with specific content for content verification
create_content_test_file :-
    open('content_test.txt', write, Stream),
    write(Stream, 'hello world from test file'),
    close(Stream).

cleanup_content_test_file :-
    (exists_file('content_test.txt') -> delete_file('content_test.txt'); true).

% Setup for permissions test
setup_permissions_test :-
    % Create executable script
    open('test_script.sh', write, Stream),
    write(Stream, '#!/bin/bash\necho "test"\n'),
    close(Stream),
    % Make it executable on Unix systems
    (current_prolog_flag(unix, true) ->
        process_create(path(chmod), ['+x', 'test_script.sh'], [])
    ; true).

cleanup_permissions_test :-
    % Cleanup file only
    (exists_file('test_script.sh') -> delete_file('test_script.sh'); true).

% Setup executable script for permission verification
create_executable_script :-
    open('test_script.sh', write, Stream),
    write(Stream, '#!/bin/bash\necho "test"\n'),
    close(Stream),
    % Make it executable on Unix systems
    (current_prolog_flag(unix, true) ->
        process_create(path(chmod), ['+x', 'test_script.sh'], [])
    ; true).

cleanup_executable_script :-
    (exists_file('test_script.sh') -> delete_file('test_script.sh'); true).

% Setup file for read tests
create_read_test_file :-
    open('read_test.txt', write, Stream),
    write(Stream, 'line 1\nline 2\nline 3\n'),
    close(Stream).

cleanup_read_test_file :-
    (exists_file('read_test.txt') -> delete_file('read_test.txt'); true).

% Setup file for edit tests
create_edit_test_file :-
    write_lines_to_file('edit_test.txt', [
        "original line 1",
        "original line 2",
        "original line 3"
    ]).

cleanup_edit_test_file :-
    (exists_file('edit_test.txt') -> delete_file('edit_test.txt'); true).

% Cleanup mkdir test
cleanup_mkdir_test :-
    (exists_file('test_mkdir_dir/semantics.pl') -> delete_file('test_mkdir_dir/semantics.pl'); true),
    (exists_directory('test_mkdir_dir') -> delete_directory('test_mkdir_dir'); true).

% Cleanup mkfile test
cleanup_mkfile_test :-
    (exists_file('test_mkfile.txt') -> delete_file('test_mkfile.txt'); true).

% Setup mock filesystem for assertz→please_verify→retractall pattern
setup_mock_fs :-
    open('mock_test.txt', write, Stream),
    write(Stream, 'mock test content'),
    close(Stream),
    make_directory('mock_testdir'),
    open('mock_testdir/mock_nested.txt', write, NestedStream),
    write(NestedStream, 'mock nested content'),
    close(NestedStream).

cleanup_mock_fs :-
    (exists_file('mock_test.txt') -> delete_file('mock_test.txt'); true),
    (exists_file('mock_testdir/mock_nested.txt') -> delete_file('mock_testdir/mock_nested.txt'); true),
    (exists_directory('mock_testdir') -> delete_directory('mock_testdir'); true).

% Setup for missing file test
setup_missing_file_test :-
    % No setup needed - entity already loaded from file
    true.

cleanup_missing_file_test :-
    % No cleanup needed
    true.

% Setup for content mismatch test
setup_content_mismatch_test :-
    open('content_test.txt', write, Stream),
    write(Stream, 'hello world from test file'),
    close(Stream).

cleanup_content_mismatch_test :-
    (exists_file('content_test.txt') -> delete_file('content_test.txt'); true).
