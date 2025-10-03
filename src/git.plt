:- use_module(library(plunit)).
:- use_module(library(filesex)).

% Note: git.pl already loaded by grimoire.pl
% ECS predicates (entity/1, component/3, etc.) are multifile and globally available

% === DISCRIMINATIVE FLOW: VERIFICATION IMPLEMENTATIONS ===

% Main repository verification overload
verify(component(Entity, has(git(repository)), git(repository(Spec)))) :-
    please_verify(component(Entity, git_repository_root, Root)),
    please_verify(component(Entity, git_repository_verified, true)),
    verify_git_repository_spec(Entity, Spec, Root).

% Repository specification verification helper
verify_git_repository_spec(Entity, Spec, Root) :-
    verify_git_repository_exists(Root),
    verify_git_repository_remotes(Entity, Spec),
    verify_git_repository_clean_state(Entity, Spec).

% Repository root verification
verify(component(_Entity, git_repository_root, Root)) :-
    (exists_directory(Root) ->
        (atomic_list_concat([Root, '/.git'], GitDir),
         exists_directory(GitDir) ->
            true
        ;
            throw(verification_error(git, not_git_repository(Root)))
        )
    ;
        throw(verification_error(git, missing_directory(Root)))
    ).

% Repository verified flag verification
verify(component(Entity, git_repository_verified, true)) :-
    component(Entity, git_repository_root, Root),
    exists_directory(Root),
    atomic_list_concat([Root, '/.git'], GitDir),
    exists_directory(GitDir).

% Repository remote URL verification
verify(component(Entity, git_repository_remote_url, URL)) :-
    component(Entity, git_repository_root, Root),
    component(Entity, git_repository_remote_name, RemoteName),
    working_directory(OldCwd, Root),
    cast(conjure(git(remote(['get-url', RemoteName]))), Result),
    working_directory(_, OldCwd),
    (Result = ok(result(ActualURL, _)) ->
        (string_concat(URL, "\n", ActualURL) ->
            true
        ;
            atom_string(URL, ActualURL) ->
                true
        ;
            throw(verification_error(git, remote_url_mismatch(expected(URL), actual(ActualURL))))
        )
    ;
        throw(verification_error(git, remote_not_found(RemoteName)))
    ).

% Repository current branch verification
verify(component(Entity, git_repository_current_branch, Branch)) :-
    component(Entity, git_repository_root, Root),
    working_directory(OldCwd, Root),
    cast(conjure(git(branch(['--show-current']))), Result),
    working_directory(_, OldCwd),
    (Result = ok(result(Output, _)) ->
        (string_concat(BranchAtom, "\n", Output),
         atom_string(Branch, BranchAtom) ->
            true
        ;
            throw(verification_error(git, unexpected_branch(Output)))
        )
    ;
        throw(verification_error(git, branch_detection_failed))
    ).

% Repository working status verification
verify(component(Entity, git_repository_working_status, Status)) :-
    component(Entity, git_repository_root, Root),
    working_directory(OldCwd, Root),
    cast(perceive(git(status(_, ActualStatus, Files))), ok(_)),
    working_directory(_, OldCwd),
    (Status = ActualStatus ->
        true
    ;
        throw(verification_error(git, working_status_mismatch(expected(Status), actual(ActualStatus), files(Files))))
    ).

% OS Reality Verification Helpers
verify_git_repository_exists(Root) :-
    exists_directory(Root),
    atomic_list_concat([Root, '/.git'], GitDir),
    exists_directory(GitDir).

verify_git_repository_remotes(Entity, Spec) :-
    findall(remote(Name, URL), member(remote(Name, URL), Spec), Remotes),
    maplist(verify_single_remote(Entity), Remotes).

verify_single_remote(Entity, remote(_Name, URL)) :-
    please_verify(component(Entity, git_repository_remote_url, URL)).

verify_git_repository_clean_state(Entity, Spec) :-
    (member(clean(true), Spec) ->
        please_verify(component(Entity, git_repository_working_status, clean))
    ;
        true  % clean(false) or not specified - allow dirty state
    ).

% === PLUNIT TESTS ===

:- begin_tests(git).

% Test basic Git entity existence
test(git_entity_exists, [true]) :-
    user:please_verify(component(git, defined, true)).

% Test Git command constructors
test(git_command_constructors, [true]) :-
    user:please_verify(component(conjure, ctor, git(clone))),
    user:please_verify(component(conjure, ctor, git(init))),
    user:please_verify(component(conjure, ctor, git(add))),
    user:please_verify(component(conjure, ctor, git(commit))),
    user:please_verify(component(perceive, ctor, git(status))), !.

% Test Git subcommand declarations
test(git_subcommands, [true]) :-
    user:please_verify(component(git, subcommand, clone)),
    user:please_verify(component(git, subcommand, status)),
    user:please_verify(component(git, subcommand, diff)),
    user:please_verify(component(git, subcommand, log)), !.

% Test Git docstrings exist
test(git_docstrings_exist, [
    forall(component(git, subcommand, Cmd))
]) :-
    docstring(git(Cmd), _), !.

% Test Git argument parsing (DCG)
test(git_args_parsing, [true]) :-
    phrase(git_args(clone("https://github.com/user/repo", "/tmp/repo")),
           ["clone", "https://github.com/user/repo", "/tmp/repo"]),
    phrase(git_args(status), ["status"]),
    phrase(git_args(diff), ["diff"]).

% Test Git command validation
test(git_command_validation, [true]) :-
    % Test that we can validate git commands exist
    functor(clone("url", "path"), clone, 2),
    component(git, subcommand, clone), !,
    functor(status, status, 0),
    component(git, subcommand, status), !.

% Test spell format registrations exist
test(git_spell_formats_exist, [true]) :-
    register_spell(perceive(git(status)), _, _, _),
    register_spell(perceive(git(ls_files)), _, _, _),
    register_spell(perceive(git(current_branch)), _, _, _).

% Test DSL pattern: repository fact schema expansion
test(git_repository_dsl_expansion, [
    setup(setup_git_repository_dsl),
    cleanup(cleanup_git_repository_dsl)
]) :-
    % Verify DSL pattern expands correctly
    user:please_verify(component(test_project, git_repository_remote_name, origin)),
    user:please_verify(component(test_project, git_repository_remote_url, 'https://github.com/test/repo')),
    user:please_verify(component(test_project, git_repository_branch, main)),
    user:please_verify(component(test_project, git_repository_clean, true)), !.

% Test verify/1 for git_repository_root with mock directory
test(verify_git_repository_root, [
    setup(setup_mock_git_repo),
    cleanup(cleanup_mock_git_repo)
]) :-
    test_git_root(TestRoot),
    user:assertz(component(mock_entity, git_repository_root, TestRoot)),
    user:please_verify(component(mock_entity, git_repository_root, TestRoot)).

% Test verify/1 for missing git directory should fail
test(verify_missing_git_directory, [
    cleanup(cleanup_missing_git_directory),
    throws(verification_error(git, missing_directory(_)))
]) :-
    user:assertz(component(bad_entity, git_repository_root, '/nonexistent/path')),
    user:please_verify(component(bad_entity, git_repository_root, '/nonexistent/path')).

cleanup_missing_git_directory :-
    % Retract only FACTS for bad_entity (not rules - check body = true)
    forall(
        clause(user:component(bad_entity, C, V), true),
        retract(user:component(bad_entity, C, V))
    ).

% Test verify/1 for directory that is not a git repository
test(verify_not_git_repository, [
    cleanup(cleanup_not_git_repository),
    throws(verification_error(git, not_git_repository(_)))
]) :-
    tmp_file_stream(text, TmpFile, Stream),
    close(Stream),
    file_directory_name(TmpFile, TmpDir),
    user:assertz(component(not_git_entity, git_repository_root, TmpDir)),
    user:please_verify(component(not_git_entity, git_repository_root, TmpDir)).

cleanup_not_git_repository :-
    % Retract only FACTS for not_git_entity (not rules - check body = true)
    forall(
        clause(user:component(not_git_entity, C, V), true),
        retract(user:component(not_git_entity, C, V))
    ).

% Test complete repository DSL pattern
test(verify_git_repository_complete, [
    setup(setup_complete_mock_git),
    cleanup(cleanup_complete_mock_git)
]) :-
    % Get test data
    test_git_root(TestRoot),
    test_git_url(TestURL),
    % Assert the DSL pattern - this is how users will use it
    user:assertz(component(complete_test, has(git(repository)), git(repository([
        remote(origin, TestURL),
        branch(main)
    ])))),
    user:assertz(component(complete_test, self, semantic(folder(TestRoot)))),
    % Debug: check if derivation works before please_verify
    (user:component(complete_test, git_repository_root, CheckRoot) ->
        format('DEBUG: Derivation works, root=~w~n', [CheckRoot])
    ;
        format('DEBUG: Derivation FAILED~n')
    ),
    % Verify the DSL - verify/1 will check all derived components against OS
    user:please_verify(component(complete_test, has(git(repository)), git(repository([
        remote(origin, TestURL),
        branch(main)
    ])))).

:- end_tests(git).

% === TEST SETUP/CLEANUP HELPERS ===

% Setup DSL pattern test
setup_git_repository_dsl :-
    user:assertz(entity(test_project)),
    user:assertz(component(test_project, has(git(repository)), git(repository([
        remote(origin, 'https://github.com/test/repo'),
        branch(main),
        clean(true)
    ])))).

cleanup_git_repository_dsl :-
    user:retractall(entity(test_project)),
    % Only retract asserted facts, not derived rules
    user:retractall(component(test_project, has(git(repository)), _)).

% Setup mock git repository for verification tests
:- dynamic test_git_root/1.

setup_mock_git_repo :-
    tmp_file_stream(text, TmpFile, Stream),
    close(Stream),
    file_directory_name(TmpFile, TmpDir),
    directory_file_path(TmpDir, 'test_git_repo', TestRoot),
    make_directory(TestRoot),
    directory_file_path(TestRoot, '.git', GitDir),
    make_directory(GitDir),
    assertz(test_git_root(TestRoot)).

cleanup_mock_git_repo :-
    (test_git_root(TestRoot) ->
        (exists_directory(TestRoot) ->
            delete_directory_and_contents(TestRoot)
        ; true),
        retractall(test_git_root(_))
    ; true),
    % Retract only FACTS for mock_entity (not rules - check body = true)
    forall(
        clause(user:component(mock_entity, C, V), true),
        retract(user:component(mock_entity, C, V))
    ).

% Setup complete mock git repository with remote and branch
:- dynamic test_git_url/1.

setup_complete_mock_git :-
    tmp_file_stream(text, TmpFile, Stream),
    close(Stream),
    file_directory_name(TmpFile, TmpDir),
    directory_file_path(TmpDir, 'test_git_complete', TestRoot),
    make_directory(TestRoot),
    directory_file_path(TestRoot, '.git', GitDir),
    make_directory(GitDir),
    % Initialize git repo
    working_directory(OldCwd, TestRoot),
    process_create(path(git), ['init'], [stdout(null), stderr(null)]),
    process_create(path(git), ['config', 'user.email', 'test@example.com'], [stdout(null), stderr(null)]),
    process_create(path(git), ['config', 'user.name', 'Test User'], [stdout(null), stderr(null)]),
    process_create(path(git), ['checkout', '-b', 'main'], [stdout(null), stderr(null)]),
    TestURL = 'https://github.com/test/complete_repo',
    atom_string(TestURL, TestURLStr),
    process_create(path(git), ['remote', 'add', 'origin', TestURLStr], [stdout(null), stderr(null)]),
    working_directory(_, OldCwd),
    assertz(test_git_root(TestRoot)),
    assertz(test_git_url(TestURL)).

cleanup_complete_mock_git :-
    (test_git_root(TestRoot) ->
        (exists_directory(TestRoot) ->
            delete_directory_and_contents(TestRoot)
        ; true),
        retractall(test_git_root(_))
    ; true),
    retractall(test_git_url(_)),
    % Retract only FACTS for complete_test (not rules - check body = true)
    forall(
        clause(user:component(complete_test, C, V), true),
        retract(user:component(complete_test, C, V))
    ).
