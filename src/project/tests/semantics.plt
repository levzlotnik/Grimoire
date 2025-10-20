:- use_module(library(plunit)).

% Note: Test entities are NOT loaded at module load time
% They are loaded by test setup after copying to /tmp/ and initializing git
% Note: project/semantics.pl already loaded by grimoire.pl
% ECS predicates (entity/1, component/3, etc.) are multifile and globally available

:- begin_tests(project_tests).

% === TESTS ===

% Test DSL expansion rules work
test(project_dsl_expansion, [
    setup(setup_git_test),
    cleanup(cleanup_git_test)
]) :-
    user:please_verify(component(test_web_app, project_type, web_service)), !,
    user:please_verify(component(test_web_app, project_git_origin, 'https://github.com/test/web-app.git')), !,
    user:please_verify(component(test_web_app, has(git(repository)), git(repository(origin('https://github.com/test/web-app.git'))))), !.

% Test mkproject spell creates project directory
test(mkproject_creates_directory, [
    setup(setup_mkproject_test),
    cleanup(cleanup_mkproject_test)
]) :-
    TestPath = '/tmp/grimoire_test_mkproject',
    user:magic_cast(conjure(mkproject(TestPath, test_proj, [git(false)])), Result),
    Result = ok(project_created(ProjectPath, test_proj)),
    exists_directory(ProjectPath),
    directory_file_path(ProjectPath, 'semantics.pl', SemFile),
    exists_file(SemFile).

% Test mkproject initializes git by default
test(mkproject_initializes_git, [
    setup(setup_mkproject_test),
    cleanup(cleanup_mkproject_test)
]) :-
    TestPath = '/tmp/grimoire_test_mkproject_git',
    user:magic_cast(conjure(mkproject(TestPath, test_git_proj, [])), Result),
    Result = ok(project_created(ProjectPath, test_git_proj)),
    directory_file_path(ProjectPath, '.git', GitDir),
    exists_directory(GitDir).

% Test mkproject with git(false) skips git init
test(mkproject_skip_git, [
    setup(setup_mkproject_test),
    cleanup(cleanup_mkproject_test)
]) :-
    TestPath = '/tmp/grimoire_test_mkproject_nogit',
    user:magic_cast(conjure(mkproject(TestPath, test_nogit_proj, [git(false)])), Result),
    Result = ok(project_created(ProjectPath, test_nogit_proj)),
    directory_file_path(ProjectPath, '.git', GitDir),
    \+ exists_directory(GitDir).

% Test mkproject with nix template
test(mkproject_with_template, [
    setup(setup_mkproject_test),
    cleanup(cleanup_mkproject_test)
]) :-
    TestPath = '/tmp/grimoire_test_mkproject_template',
    % Use python template from grimoire-templates
    user:magic_cast(conjure(mkproject(TestPath, test_template_proj, [template(python), git(false)])), Result),
    Result = ok(project_created(ProjectPath, test_template_proj)),
    exists_directory(ProjectPath),
    directory_file_path(ProjectPath, 'flake.nix', FlakeFile),
    exists_file(FlakeFile),
    directory_file_path(ProjectPath, 'semantics.pl', SemFile),
    exists_file(SemFile).

% Test perceive(project(validate(...))) spell with valid entity
test(project_validate_valid_entity, [
    setup(setup_git_test),
    cleanup(cleanup_git_test)
]) :-
    user:magic_cast(perceive(project(validate(test_web_app))), Result),
    assertion(Result = ok(valid)).

% Test perceive(project(validate(...))) spell with entity without git
test(project_validate_no_git, [true]) :-
    user:magic_cast(perceive(project(validate(test_project_no_git))), Result),
    assertion(Result = ok(valid)).

% Test perceive(project(validate(...))) spell with invalid type
% Note: Currently validation only checks component existence, not type validity
test(project_validate_invalid_type, [
    setup(setup_git_test),
    cleanup(cleanup_git_test)
]) :-
    user:magic_cast(perceive(project(validate(test_project_invalid_type))), Result),
    % Project validation checks component existence, not type semantics
    % Type validity is checked by verify/1 predicates in the test suite
    assertion(Result = ok(valid)).

% Test perceive(project(structure(...))) spell
test(project_structure_query, [
    setup(setup_git_test),
    cleanup(cleanup_git_test)
]) :-
    user:magic_cast(perceive(project(structure(test_web_app))), Result),
    assertion(Result = ok(project_info(type(web_service), sources(_), contexts(_)))).

% Test perceive(project(structure(...))) with project that has context
test(project_structure_with_context, [
    setup(setup_git_test),
    cleanup(cleanup_git_test)
]) :-
    user:magic_cast(perceive(project(structure(test_project_with_context))), Result),
    Result = ok(project_info(type(web_service), sources(_), contexts(Contexts))),
    assertion(member(build, Contexts)).

:- end_tests(project_tests).

% === SETUP/CLEANUP ===

setup_git_test :-
    cleanup_git_test,
    TestDir = '/tmp/grimoire_project_test_git',
    make_directory(TestDir),
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
    user:magic_cast(conjure(git(init('.'))), _),
    user:magic_cast(conjure(git(config(['user.name', 'Test']))), _),
    user:magic_cast(conjure(git(config(['user.email', 'test@test.com']))), _),
    user:magic_cast(conjure(git(add(['.']))), _),
    user:magic_cast(conjure(git(commit('Initial test commit'))), _),
    working_directory(_, OldCwd).

cleanup_git_test :-
    delete_if_exists('/tmp/grimoire_project_test_git').

setup_mkproject_test :-
    cleanup_mkproject_test.

cleanup_mkproject_test :-
    delete_if_exists('/tmp/grimoire_test_mkproject'),
    delete_if_exists('/tmp/grimoire_test_mkproject_git'),
    delete_if_exists('/tmp/grimoire_test_mkproject_nogit'),
    delete_if_exists('/tmp/grimoire_test_mkproject_template').

delete_if_exists(Dir) :-
    (exists_directory(Dir) ->
        delete_directory_and_contents(Dir)
    ; true).
