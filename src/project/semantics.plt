% Project domain tests - Phase 3 behavioral testing

:- use_module(library(plunit)).

% Load domain semantics first
:- grimoire_ensure_loaded('@/src/project/semantics.pl').

% Load dependent domains (project uses git and nix spells)
:- grimoire_ensure_loaded('@/src/git.pl').
:- grimoire_ensure_loaded('@/src/nix/semantics.pl').

% Note: Test entities are NOT loaded at module load time
% They are loaded by test setup after copying to /tmp/ and initializing git

% === TEST SUITE ===

:- begin_tests(project).

% === ENTITY TESTS ===

test(project_entity_exists) :-
    entity(project), !.

test(mkproject_entity_exists) :-
    entity(mkproject), !.

test(project_context_entities) :-
    entity(project(context(build))), !,
    entity(project(context(runtime))), !,
    entity(project(context(test))), !.

% === SPELL METADATA TESTS ===

test(mkproject_spell_registered) :-
    user:please_verify(component(conjure, ctor, mkproject)).

test(project_validate_spell_registered) :-
    user:please_verify(component(perceive, ctor, project(validate))).

test(project_structure_spell_registered) :-
    user:please_verify(component(perceive, ctor, project(structure))).

test(spells_have_docstrings) :-
    user:please_verify(component(conjure(mkproject), docstring, _)),
    user:please_verify(component(perceive(project(validate)), docstring, _)),
    user:please_verify(component(perceive(project(structure)), docstring, _)).

% === COMPONENT EXPANSION TESTS ===

test(project_app_expands_to_type, [
    setup(setup_git_test),
    cleanup(cleanup_git_test)
]) :-
    user:please_verify(component(test_web_app, project_type, Type)),
    assertion(Type = web_service).

test(project_app_expands_to_git_origin, [
    setup(setup_git_test),
    cleanup(cleanup_git_test)
]) :-
    user:please_verify(component(test_web_app, project_git_origin, Origin)),
    assertion(Origin = 'https://github.com/test/web-app.git').

test(project_app_expands_to_nix_flake, [
    setup(setup_git_test),
    cleanup(cleanup_git_test)
]) :-
    user:please_verify(component(test_web_app, project_nix_flake, NixFlake)),
    assertion(NixFlake = nix(flake('.'))).

test(project_app_expands_to_fs_structure, [
    setup(setup_git_test),
    cleanup(cleanup_git_test)
]) :-
    user:please_verify(component(test_web_app, project_fs_structure, Structure)),
    assertion(is_list(Structure)).

% === REVERSE BRIDGE TESTS (Cross-domain triggering) ===

test(project_git_origin_triggers_git_repository, [
    setup(setup_git_test),
    cleanup(cleanup_git_test)
]) :-
    user:please_verify(component(test_web_app, has(git(repository)), _)).

test(project_nix_flake_triggers_nix_schema, [
    setup(setup_git_test),
    cleanup(cleanup_git_test)
]) :-
    user:please_verify(component(test_web_app, has(nix(flake)), _)).

% === COMPOSITE VERIFICATION TESTS ===

test(project_app_composite_verification, [
    setup(setup_git_test),
    cleanup(cleanup_git_test)
]) :-
    % This verifies all expanded components using please_verify composition
    user:please_verify(component(test_web_app, has(project(app)), _)).

% === LEAF VERIFICATION TESTS ===

test(project_type_verification_valid, [
    setup(setup_git_test),
    cleanup(cleanup_git_test)
]) :-
    user:please_verify(component(test_web_app, project_type, web_service)).

test(project_type_verification_invalid, [
    throws(error(verification_failed(_), _))
]) :-
    % test_project_invalid_type loaded from file with invalid type
    user:please_verify(component(test_project_invalid_type, project_type, invalid_type)).

% === SPELL BEHAVIORAL TESTS ===

% Test mkproject spell creates directory
test(spell_mkproject_creates_directory, [
    cleanup(cleanup_mkproject_test)
]) :-
    TestPath = '/tmp/grimoire_tests/mkproject_basic',
    (exists_directory(TestPath) -> delete_directory_and_contents(TestPath) ; true),
    make_directory_path(TestPath),
    user:magic_cast(conjure(mkproject(TestPath, test_proj, [git(false)])), Result),
    assertion(Result = ok(project_created(_, test_proj))),
    directory_file_path(TestPath, 'test_proj', ProjectPath),
    assertion(exists_directory(ProjectPath)),
    directory_file_path(ProjectPath, 'semantics.pl', SemFile),
    assertion(exists_file(SemFile)).

% Test mkproject initializes git by default
test(spell_mkproject_with_git, [
    cleanup(cleanup_mkproject_test)
]) :-
    TestPath = '/tmp/grimoire_tests/mkproject_git',
    (exists_directory(TestPath) -> delete_directory_and_contents(TestPath) ; true),
    make_directory_path(TestPath),
    user:magic_cast(conjure(mkproject(TestPath, test_git_proj, [])), Result),
    assertion(Result = ok(project_created(_, test_git_proj))),
    directory_file_path(TestPath, 'test_git_proj', ProjectPath),
    directory_file_path(ProjectPath, '.git', GitDir),
    assertion(exists_directory(GitDir)).

% Test mkproject with template
test(spell_mkproject_with_template, [
    cleanup(cleanup_mkproject_test)
]) :-
    TestPath = '/tmp/grimoire_tests/mkproject_template',
    (exists_directory(TestPath) -> delete_directory_and_contents(TestPath) ; true),
    make_directory_path(TestPath),
    user:magic_cast(conjure(mkproject(TestPath, test_template, [template(python), git(false)])), Result),
    assertion(Result = ok(project_created(_, test_template))),
    directory_file_path(TestPath, 'test_template', ProjectPath),
    directory_file_path(ProjectPath, 'flake.nix', FlakeFile),
    assertion(exists_file(FlakeFile)).

% Test project validate spell
test(spell_project_validate, [
    setup(setup_git_test),
    cleanup(cleanup_git_test)
]) :-
    user:magic_cast(perceive(project(validate(test_web_app))), Result),
    assertion(Result = ok(valid)).

% Test project structure spell
test(spell_project_structure, [
    setup(setup_git_test),
    cleanup(cleanup_git_test)
]) :-
    user:magic_cast(perceive(project(structure(test_web_app))), Result),
    assertion(Result = ok(project_info(type(web_service), sources(_), contexts(_)))).

% === PROJECT INIT TESTS ===

% Test basic init without any infrastructure
test(init_basic_no_infrastructure, [
    setup(setup_test_init_dir),
    cleanup(cleanup_test_init_dir)
]) :-
    TestDir = '/tmp/grimoire_test_init',

    % Run init
    user:magic_cast(conjure(project(init(folder(TestDir), options([])))), Result),

    % Check result is success
    assertion(Result = ok(initialized(entity(project(_)), path(TestDir), detected([]), skills(_)))),

    % Verify files were created
    directory_file_path(TestDir, 'semantics.pl', SemanticsPath),
    directory_file_path(TestDir, 'semantics.plt', TestPath),
    assertion(exists_file(SemanticsPath)),
    assertion(exists_file(TestPath)),

    % Cleanup Grimoire-tracked session
    user:magic_cast(conjure(session(delete(id(default)))), DeleteResult),
    assertion(DeleteResult = ok(_)).

% Test init with git repository
test(init_with_git) :-
    TestDir = '/tmp/grimoire_test_init_git',
    (exists_directory(TestDir) ->
        delete_directory_and_contents(TestDir), format('Deleted existing test dir ~w~n', [TestDir]) ;
        true),
    make_directory(TestDir),
    format('Created test dir ~w~n', [TestDir]),

    % Initialize git repo
    user:magic_cast(conjure(git(init(path(TestDir)))), ResultGit),
    format('Initialized git repo in ~w~n    Result: ~q~n', [TestDir, ResultGit]),
    assertion(ResultGit = ok(_OkGit)),

    % Run init
    user:magic_cast(conjure(project(init(folder(TestDir), options([])))), Result),
    format('Ran project init in ~w~n    Result: ~q~n', [TestDir, Result]),

    % Check git was detected
    assertion(Result = ok(initialized(entity(_Entity), path(_Path), detected(Features), skills(_Skills)))),
    assertion(member(git, Features)),

    % Verify .gitignore was updated
    directory_file_path(TestDir, '.gitignore', GitignorePath),
    assertion(exists_file(GitignorePath)),
    read_file_to_string(GitignorePath, GitignoreContent, []),
    assertion(sub_string(GitignoreContent, _, _, _, 'activity_log.jsonl')),

    % Cleanup Grimoire-tracked session
    user:magic_cast(conjure(session(delete(id(default)))), DeleteResult),
    assertion(DeleteResult = ok(_)).

% Test init fails when semantics.pl exists without force
test(init_fails_existing_semantics, [
    setup(setup_test_init_dir)
    % cleanup(cleanup_test_init_dir)
]) :-
    TestDir = '/tmp/grimoire_test_init',

    % Create existing semantics.pl
    directory_file_path(TestDir, 'semantics.pl', SemanticsPath),
    open(SemanticsPath, write, Stream),
    write(Stream, '% existing file\n'),
    close(Stream),

    % Run init without force - should fail
    user:magic_cast(conjure(project(init(folder(TestDir), options([])))), Result),

    assertion(Result = error(_)).

% Test init with force option overwrites existing
test(init_force_overwrites) :-
    TestDir = '/tmp/grimoire_test_init_force',
    (exists_directory(TestDir) -> delete_directory_and_contents(TestDir) ; true),
    make_directory(TestDir),

    % Create existing semantics.pl
    directory_file_path(TestDir, 'semantics.pl', SemanticsPath),
    open(SemanticsPath, write, Stream),
    write(Stream, '% existing file\n'),
    close(Stream),

    % Run init with force - should succeed
    user:magic_cast(conjure(project(init(folder(TestDir), options([force])))), Result),

    assertion(Result = ok(initialized(_, _, _, _))),

    % Verify new content was written
    read_file_to_string(SemanticsPath, NewContent, []),
    assertion(sub_string(NewContent, _, _, _, 'self_entity')),

    % Cleanup Grimoire-tracked session
    user:magic_cast(conjure(session(delete(id(default)))), DeleteResult),
    assertion(DeleteResult = ok(_)).

% Test entity name sanitization
test(init_sanitizes_entity_name, [
    setup((
        TestDir = '/tmp/grimoire-test-CAPS',
        (exists_directory(TestDir) -> delete_directory_and_contents(TestDir) ; true),
        make_directory(TestDir)
    )),
    cleanup((
        TestDir = '/tmp/grimoire-test-CAPS',
        (exists_directory(TestDir) -> delete_directory_and_contents(TestDir) ; true)
    ))
]) :-
    TestDir = '/tmp/grimoire-test-CAPS',

    % Run init
    user:magic_cast(conjure(project(init(folder(TestDir), options([])))), Result),

    % Entity name should be sanitized: grimoire-test-CAPS -> grimoire_test_caps
    Result = ok(initialized(entity(project(EntityName)), _, _, _)),
    assertion(EntityName = grimoire_test_caps),

    % Cleanup Grimoire-tracked session
    user:magic_cast(conjure(session(delete(id(default)))), DeleteResult),
    assertion(DeleteResult = ok(_)).

% Test session creation and focus
test(init_creates_default_session) :-
    TestDir = '/tmp/grimoire_test_init_session',
    (exists_directory(TestDir) -> delete_directory_and_contents(TestDir) ; true),
    make_directory(TestDir),

    % Run init
    user:magic_cast(conjure(project(init(folder(TestDir), options([])))), Result),
    assertion(Result = ok(initialized(entity(Entity), _, _, _))),

    % Verify default session exists and entity is focused
    user:magic_cast(perceive(session(status)), StatusResult),
    assertion(StatusResult = ok(session_status(id(default), _, focused(entity(Entity)), active(true)))),

    user:magic_cast(perceive(session(focused)), FocusResult),
    assertion(FocusResult = ok(focused_entity(entity(Entity), _, _))),

    % Cleanup default session
    user:magic_cast(conjure(session(delete(id(default)))), DeleteResult),
    assertion(DeleteResult = ok(_)).

:- end_tests(project).

% === SETUP/CLEANUP HELPERS ===

% Git test setup - copies test entities to /tmp and initializes git
setup_git_test :-
    cleanup_git_test,
    TestDir = '/tmp/grimoire_tests/project_entities',
    make_directory_path(TestDir),
    % Copy test entity file and ALL subdirectories using absolute path via GRIMOIRE_ROOT
    getenv('GRIMOIRE_ROOT', GrimoireRoot),
    directory_file_path(GrimoireRoot, 'src/project/tests', SourceDir),
    directory_file_path(SourceDir, 'semantics.pl', SourceFile),
    directory_file_path(TestDir, 'semantics.pl', DestFile),
    copy_file(SourceFile, DestFile),
    % Copy all test subdirectories (git is initialized inside each one that needs it)
    copy_test_directories(SourceDir, TestDir),
    % Load test entities from the copied directory
    load_entity(semantic(file(DestFile))).

% Helper to copy test entity subdirectories
copy_test_directories(SourceDir, DestDir) :-
    % Dirs that need git initialized
    GitDirs = [test_web_app, test_cli_tool, test_library,
               test_project_with_context, test_project_invalid_type],
    forall(member(Dir, GitDirs), copy_test_dir_with_git(SourceDir, DestDir, Dir)),
    % Dirs without git
    NoGitDirs = [test_project_no_git],
    forall(member(Dir, NoGitDirs), copy_test_dir_no_git(SourceDir, DestDir, Dir)).

copy_test_dir_with_git(SourceDir, DestDir, DirName) :-
    directory_file_path(SourceDir, DirName, SourcePath),
    directory_file_path(DestDir, DirName, DestPath),
    (exists_directory(SourcePath) ->
        (make_directory(DestPath),
         directory_file_path(SourcePath, 'semantics.pl', SourceSemFile),
         directory_file_path(DestPath, 'semantics.pl', DestSemFile),
         copy_file(SourceSemFile, DestSemFile),
         % Initialize git in this specific subdirectory
         init_git_in_dir(DestPath))
    ; true).

copy_test_dir_no_git(SourceDir, DestDir, DirName) :-
    directory_file_path(SourceDir, DirName, SourcePath),
    directory_file_path(DestDir, DirName, DestPath),
    (exists_directory(SourcePath) ->
        (make_directory(DestPath),
         directory_file_path(SourcePath, 'semantics.pl', SourceSemFile),
         directory_file_path(DestPath, 'semantics.pl', DestSemFile),
         copy_file(SourceSemFile, DestSemFile))
    ; true).

% Initialize git repository in a directory
init_git_in_dir(Dir) :-
    working_directory(OldCwd, Dir),
    user:magic_cast(conjure(git(init(path('.')))), InitResult),
    assertion(InitResult = ok(_)),
    user:magic_cast(conjure(git(config(git_root('.'), args(['user.name', 'Test'])))), NameResult),
    assertion(NameResult = ok(_)),
    user:magic_cast(conjure(git(config(git_root('.'), args(['user.email', 'test@test.com'])))), EmailResult),
    assertion(EmailResult = ok(_)),
    user:magic_cast(conjure(git(add(git_root('.'), paths(['.'])))), AddResult),
    assertion(AddResult = ok(_)),
    user:magic_cast(conjure(git(commit(git_root('.'), message('Initial test commit')))), CommitResult),
    assertion(CommitResult = ok(_)),
    working_directory(_, OldCwd).

cleanup_git_test :-
    delete_if_exists('/tmp/grimoire_tests/project_entities').

% Mkproject test cleanup
cleanup_mkproject_test :-
    delete_if_exists('/tmp/grimoire_tests/mkproject_basic'),
    delete_if_exists('/tmp/grimoire_tests/mkproject_git'),
    delete_if_exists('/tmp/grimoire_tests/mkproject_template').

delete_if_exists(Dir) :-
    (exists_directory(Dir) ->
        delete_directory_and_contents(Dir)
    ; true).

% Helper to create and cleanup test directory for init tests
setup_test_init_dir :-
    TestDir = '/tmp/grimoire_test_init',
    (exists_directory(TestDir) -> delete_directory_and_contents(TestDir) ; true),
    make_directory(TestDir).

cleanup_test_init_dir :-
    TestDir = '/tmp/grimoire_test_init',
    (exists_directory(TestDir) -> delete_directory_and_contents(TestDir) ; true).

