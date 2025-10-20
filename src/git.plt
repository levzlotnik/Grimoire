% Git domain tests - Phase 3 behavioral testing

:- use_module(library(plunit)).

% Load domain semantics first
:- grimoire_ensure_loaded('@/src/git.pl').

% Load test entities from file-based knowledge
:- load_entity(semantic(file('@/src/tests/git_test_entity.pl'))).

% === TEST SUITE ===

:- begin_tests(git).

% === ENTITY TESTS ===

test(git_entity_exists) :-
    entity(git).

test(git_repository_entity_from_dsl) :-
    % test_project loaded from file with has(git(repository))
    entity(test_project),
    user:please_verify(component(test_project, has(git(repository)), _)).

% === COMPONENT EXPANSION TESTS ===

test(git_repository_expands_to_remote_name, [
    setup(setup_git_test_repo),
    cleanup(cleanup_git_test_repo)
]) :-
    % Test entity expansion
    user:please_verify(component(test_project, git_repository_remote_name, Name)),
    assertion(Name = origin).

test(git_repository_expands_to_remote_url, [
    setup(setup_git_test_repo),
    cleanup(cleanup_git_test_repo)
]) :-
    user:please_verify(component(test_project, git_repository_remote_url, URL)),
    assertion(URL = 'https://github.com/test/repo').

test(git_repository_expands_to_branch, [
    setup(setup_git_test_repo),
    cleanup(cleanup_git_test_repo)
]) :-
    user:please_verify(component(test_project, git_repository_branch, Branch)),
    assertion(Branch = main).

test(git_repository_expands_to_clean, [
    setup(setup_git_test_repo),
    cleanup(cleanup_git_test_repo)
]) :-
    user:please_verify(component(test_project, git_repository_clean, Clean)),
    assertion(Clean = true).

% === ROOT DERIVATION TESTS ===

test(git_repository_root_from_self, [
    setup(setup_git_test_repo),
    cleanup(cleanup_git_test_repo)
]) :-
    % Root derived from self component
    user:please_verify(component(test_project, git_repository_root, Root)),
    assertion(Root = '/tmp/test_project').

% === COMPOSITE VERIFICATION TESTS ===

test(git_repository_composite_verification, [
    setup(setup_git_test_repo),
    cleanup(cleanup_git_test_repo)
]) :-
    % This verifies all expanded components using please_verify composition
    user:please_verify(component(test_project, has(git(repository)), _)).

% === LEAF VERIFICATION TESTS ===

test(git_repository_root_verification_success, [
    setup(setup_git_test_repo),
    cleanup(cleanup_git_test_repo)
]) :-
    % Verify valid git repository root
    user:please_verify(component(test_project, git_repository_root, '/tmp/test_project')).

test(git_repository_root_verification_missing, [
    throws(error(verification_failed(_), _))
]) :-
    % Verify missing directory throws error
    user:please_verify(component(git_bad_entity, git_repository_root, '/nonexistent/path')).

test(git_repository_root_verification_not_git, [
    setup(setup_not_git_directory),
    cleanup(cleanup_not_git_directory),
    throws(error(verification_failed(_), _))
]) :-
    % Verify directory without .git throws error
    user:please_verify(component(git_not_git_entity, git_repository_root, '/tmp/test_not_git')).

test(git_repository_remote_url_verification, [
    setup(setup_git_test_repo),
    cleanup(cleanup_git_test_repo)
]) :-
    % Valid URL formats
    user:please_verify(component(test_project, git_repository_remote_url, 'https://github.com/test/repo')).

% DELETED: This test used assertz/retract which is forbidden
% Need to add a static test entity with invalid URL to git_test_entity.pl if we want to test this

% === SPELL TESTS ===

% Test git init spell
test(spell_git_init, [
    cleanup(cleanup_spell_init)
]) :-
    TestPath = '/tmp/test_git_init',
    (exists_directory(TestPath) -> delete_directory_and_contents(TestPath) ; true),
    make_directory_path(TestPath),
    user:magic_cast(conjure(git(init(path(TestPath)))), Result),
    assertion(Result = ok(initialized(path(TestPath)))),
    assertion(exists_directory(TestPath)),
    atomic_list_concat([TestPath, '/.git'], GitDir),
    assertion(exists_directory(GitDir)).

% Test git add spell
test(spell_git_add, [
    setup(setup_git_add_test),
    cleanup(cleanup_git_add_test)
]) :-
    user:magic_cast(conjure(git(add(git_root('/tmp/test_git_add'), paths(['test.txt'])))), Result),
    assertion(Result = ok(staged(files(['test.txt'])))).

% Test git commit spell
test(spell_git_commit, [
    setup(setup_git_commit_test),
    cleanup(cleanup_git_commit_test)
]) :-
    user:magic_cast(conjure(git(commit(git_root('/tmp/test_git_commit'), message("test message")))), Result),
    assertion(Result = ok(committed(hash(_Hash)))).

% Test git status spell
test(spell_git_status, [
    setup(setup_git_status_test),
    cleanup(cleanup_git_status_test)
]) :-
    user:magic_cast(perceive(git(status(git_root('/tmp/test_git_status')))), Result),
    assertion(Result = ok(status_info(branch(_Branch), working_status(_Status), files(_Files)))).

% Test git branch list spell
test(spell_git_branch_list, [
    setup(setup_git_branch_test),
    cleanup(cleanup_git_branch_test)
]) :-
    user:magic_cast(perceive(git(branch(git_root('/tmp/test_git_branch'), operation(list)))), Result),
    Result = ok(branches(Branches)),
    assertion(is_list(Branches)).

% Test git current_branch spell
test(spell_git_current_branch, [
    setup(setup_git_current_branch_test),
    cleanup(cleanup_git_current_branch_test)
]) :-
    user:magic_cast(perceive(git(current_branch(git_root('/tmp/test_git_current_branch')))), Result),
    assertion(Result = ok(current_branch(_Branch))).

% Test git config spell
test(spell_git_config, [
    setup(setup_git_config_test),
    cleanup(cleanup_git_config_test)
]) :-
    user:magic_cast(conjure(git(config(git_root('/tmp/test_git_config'), args(['user.email'])))), Result),
    assertion(Result = ok(config_output(_Output))).

% Test git diff spell
test(spell_git_diff, [
    setup(setup_git_diff_test),
    cleanup(cleanup_git_diff_test)
]) :-
    user:magic_cast(perceive(git(diff(git_root('/tmp/test_git_diff')))), Result),
    assertion(Result = ok(diff_output(_Output))).

% Test git log spell
test(spell_git_log, [
    setup(setup_git_log_test),
    cleanup(cleanup_git_log_test)
]) :-
    user:magic_cast(perceive(git(log(git_root('/tmp/test_git_log')))), Result),
    assertion(Result = ok(log_output(_Output))).

% === AUTO-DETECT COMPONENT TESTS ===

test(git_repository_current_branch_auto_detect, [
    setup(setup_git_auto_detect_test),
    cleanup(cleanup_git_auto_detect_test)
]) :-
    % Auto-detect current branch via spell
    user:please_verify(component(git_complete_test, git_repository_current_branch, CurrentBranch)),
    assertion(atom(CurrentBranch)).

test(git_repository_working_status_auto_detect, [
    setup(setup_git_auto_detect_test),
    cleanup(cleanup_git_auto_detect_test)
]) :-
    % Auto-detect working status via spell
    user:please_verify(component(git_complete_test, git_repository_working_status, WorkingStatus)),
    assertion(member(WorkingStatus, [clean, dirty, unknown])).

:- end_tests(git).

% === SETUP/CLEANUP HELPERS ===

% Main test repo setup
setup_git_test_repo :-
    TestPath = '/tmp/test_project',
    (exists_directory(TestPath) -> delete_directory_and_contents(TestPath) ; true),
    make_directory_path(TestPath),
    user:magic_cast(conjure(git(init(path(TestPath)))), InitResult),
    assertion(InitResult = ok(_)),
    user:magic_cast(conjure(git(config(git_root(TestPath), args(['user.email', 'test@example.com'])))), EmailResult),
    assertion(EmailResult = ok(_)),
    user:magic_cast(conjure(git(config(git_root(TestPath), args(['user.name', 'Test User'])))), NameResult),
    assertion(NameResult = ok(_)).

cleanup_git_test_repo :-
    TestPath = '/tmp/test_project',
    (exists_directory(TestPath) -> delete_directory_and_contents(TestPath) ; true).

% Not-git directory setup
setup_not_git_directory :-
    TestPath = '/tmp/test_not_git',
    (exists_directory(TestPath) -> delete_directory_and_contents(TestPath) ; true),
    make_directory_path(TestPath).

cleanup_not_git_directory :-
    TestPath = '/tmp/test_not_git',
    (exists_directory(TestPath) -> delete_directory_and_contents(TestPath) ; true).

% Git init spell cleanup
cleanup_spell_init :-
    TestPath = '/tmp/test_git_init',
    (exists_directory(TestPath) -> delete_directory_and_contents(TestPath) ; true).

% Git add spell setup
setup_git_add_test :-
    TestPath = '/tmp/test_git_add',
    (exists_directory(TestPath) -> delete_directory_and_contents(TestPath) ; true),
    make_directory_path(TestPath),
    user:magic_cast(conjure(git(init(path(TestPath)))), InitResult),
    assertion(InitResult = ok(_)),
    user:magic_cast(conjure(git(config(git_root(TestPath), args(['user.email', 'test@example.com'])))), EmailResult),
    assertion(EmailResult = ok(_)),
    user:magic_cast(conjure(git(config(git_root(TestPath), args(['user.name', 'Test User'])))), NameResult),
    assertion(NameResult = ok(_)),
    % Create a test file
    atomic_list_concat([TestPath, '/test.txt'], TestFile),
    open(TestFile, write, Stream),
    write(Stream, 'test content'),
    close(Stream).

cleanup_git_add_test :-
    TestPath = '/tmp/test_git_add',
    (exists_directory(TestPath) -> delete_directory_and_contents(TestPath) ; true).

% Git commit spell setup
setup_git_commit_test :-
    TestPath = '/tmp/test_git_commit',
    (exists_directory(TestPath) -> delete_directory_and_contents(TestPath) ; true),
    make_directory_path(TestPath),
    user:magic_cast(conjure(git(init(path(TestPath)))), InitResult),
    assertion(InitResult = ok(_)),
    user:magic_cast(conjure(git(config(git_root(TestPath), args(['user.email', 'test@example.com'])))), EmailResult),
    assertion(EmailResult = ok(_)),
    user:magic_cast(conjure(git(config(git_root(TestPath), args(['user.name', 'Test User'])))), NameResult),
    assertion(NameResult = ok(_)),
    % Create and stage a file
    atomic_list_concat([TestPath, '/test.txt'], TestFile),
    open(TestFile, write, Stream),
    write(Stream, 'test content'),
    close(Stream),
    user:magic_cast(conjure(git(add(git_root(TestPath), paths(['test.txt'])))), AddResult),
    assertion(AddResult = ok(_)).

cleanup_git_commit_test :-
    TestPath = '/tmp/test_git_commit',
    (exists_directory(TestPath) -> delete_directory_and_contents(TestPath) ; true).

% Git status spell setup
setup_git_status_test :-
    TestPath = '/tmp/test_git_status',
    (exists_directory(TestPath) -> delete_directory_and_contents(TestPath) ; true),
    make_directory_path(TestPath),
    user:magic_cast(conjure(git(init(path(TestPath)))), InitResult),
    assertion(InitResult = ok(_)),
    user:magic_cast(conjure(git(config(git_root(TestPath), args(['user.email', 'test@example.com'])))), EmailResult),
    assertion(EmailResult = ok(_)),
    user:magic_cast(conjure(git(config(git_root(TestPath), args(['user.name', 'Test User'])))), NameResult),
    assertion(NameResult = ok(_)).

cleanup_git_status_test :-
    TestPath = '/tmp/test_git_status',
    (exists_directory(TestPath) -> delete_directory_and_contents(TestPath) ; true).

% Git branch spell setup
setup_git_branch_test :-
    TestPath = '/tmp/test_git_branch',
    (exists_directory(TestPath) -> delete_directory_and_contents(TestPath) ; true),
    make_directory_path(TestPath),
    user:magic_cast(conjure(git(init(path(TestPath)))), InitResult),
    assertion(InitResult = ok(_)),
    user:magic_cast(conjure(git(config(git_root(TestPath), args(['user.email', 'test@example.com'])))), EmailResult),
    assertion(EmailResult = ok(_)),
    user:magic_cast(conjure(git(config(git_root(TestPath), args(['user.name', 'Test User'])))), NameResult),
    assertion(NameResult = ok(_)).

cleanup_git_branch_test :-
    TestPath = '/tmp/test_git_branch',
    (exists_directory(TestPath) -> delete_directory_and_contents(TestPath) ; true).

% Git current_branch spell setup
setup_git_current_branch_test :-
    TestPath = '/tmp/test_git_current_branch',
    (exists_directory(TestPath) -> delete_directory_and_contents(TestPath) ; true),
    make_directory_path(TestPath),
    user:magic_cast(conjure(git(init(path(TestPath)))), InitResult),
    assertion(InitResult = ok(_)),
    user:magic_cast(conjure(git(config(git_root(TestPath), args(['user.email', 'test@example.com'])))), EmailResult),
    assertion(EmailResult = ok(_)),
    user:magic_cast(conjure(git(config(git_root(TestPath), args(['user.name', 'Test User'])))), NameResult),
    assertion(NameResult = ok(_)).

cleanup_git_current_branch_test :-
    TestPath = '/tmp/test_git_current_branch',
    (exists_directory(TestPath) -> delete_directory_and_contents(TestPath) ; true).

% Git config spell setup
setup_git_config_test :-
    TestPath = '/tmp/test_git_config',
    (exists_directory(TestPath) -> delete_directory_and_contents(TestPath) ; true),
    make_directory_path(TestPath),
    user:magic_cast(conjure(git(init(path(TestPath)))), InitResult),
    assertion(InitResult = ok(_)),
    user:magic_cast(conjure(git(config(git_root(TestPath), args(['user.email', 'test@example.com'])))), EmailResult),
    assertion(EmailResult = ok(_)),
    user:magic_cast(conjure(git(config(git_root(TestPath), args(['user.name', 'Test User'])))), NameResult),
    assertion(NameResult = ok(_)).

cleanup_git_config_test :-
    TestPath = '/tmp/test_git_config',
    (exists_directory(TestPath) -> delete_directory_and_contents(TestPath) ; true).

% Git diff spell setup
setup_git_diff_test :-
    TestPath = '/tmp/test_git_diff',
    (exists_directory(TestPath) -> delete_directory_and_contents(TestPath) ; true),
    make_directory_path(TestPath),
    user:magic_cast(conjure(git(init(path(TestPath)))), InitResult),
    assertion(InitResult = ok(_)),
    user:magic_cast(conjure(git(config(git_root(TestPath), args(['user.email', 'test@example.com'])))), EmailResult),
    assertion(EmailResult = ok(_)),
    user:magic_cast(conjure(git(config(git_root(TestPath), args(['user.name', 'Test User'])))), NameResult),
    assertion(NameResult = ok(_)).

cleanup_git_diff_test :-
    TestPath = '/tmp/test_git_diff',
    (exists_directory(TestPath) -> delete_directory_and_contents(TestPath) ; true).

% Git log spell setup
setup_git_log_test :-
    TestPath = '/tmp/test_git_log',
    (exists_directory(TestPath) -> delete_directory_and_contents(TestPath) ; true),
    make_directory_path(TestPath),
    user:magic_cast(conjure(git(init(path(TestPath)))), InitResult),
    assertion(InitResult = ok(_)),
    user:magic_cast(conjure(git(config(git_root(TestPath), args(['user.email', 'test@example.com'])))), EmailResult),
    assertion(EmailResult = ok(_)),
    user:magic_cast(conjure(git(config(git_root(TestPath), args(['user.name', 'Test User'])))), NameResult),
    assertion(NameResult = ok(_)),
    % Create initial commit
    atomic_list_concat([TestPath, '/test.txt'], TestFile),
    open(TestFile, write, Stream),
    write(Stream, 'test'),
    close(Stream),
    user:magic_cast(conjure(git(add(git_root(TestPath), paths(['test.txt'])))), AddResult),
    assertion(AddResult = ok(_)),
    user:magic_cast(conjure(git(commit(git_root(TestPath), message('initial')))), CommitResult),
    assertion(CommitResult = ok(_)).

cleanup_git_log_test :-
    TestPath = '/tmp/test_git_log',
    (exists_directory(TestPath) -> delete_directory_and_contents(TestPath) ; true).

% Git auto-detect setup
setup_git_auto_detect_test :-
    TestPath = '/tmp/test_git_complete',
    (exists_directory(TestPath) -> delete_directory_and_contents(TestPath) ; true),
    make_directory_path(TestPath),
    user:magic_cast(conjure(git(init(path(TestPath)))), InitResult),
    assertion(InitResult = ok(_)),
    user:magic_cast(conjure(git(config(git_root(TestPath), args(['user.email', 'test@example.com'])))), EmailResult),
    assertion(EmailResult = ok(_)),
    user:magic_cast(conjure(git(config(git_root(TestPath), args(['user.name', 'Test User'])))), NameResult),
    assertion(NameResult = ok(_)).

cleanup_git_auto_detect_test :-
    TestPath = '/tmp/test_git_complete',
    (exists_directory(TestPath) -> delete_directory_and_contents(TestPath) ; true).
