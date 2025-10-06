:- use_module(library(plunit)).
:- load_entity(semantic(file('@/src/tests/project_test_entities.pl'))).

% Note: project/semantics.pl already loaded by grimoire.pl
% ECS predicates (entity/1, component/3, etc.) are multifile and globally available

:- begin_tests(project).

% === VERIFY PREDICATES ===

% Verify project app component and validate type
verify(component(Entity, has(project(app)), project(app(Options)))) :-
    please_verify(component(Entity, project_type, Type)),
    member(Type, [web_service, cli_tool, library, package]).

% Verify project type is valid
verify(component(_Entity, project_type, Type)) :-
    member(Type, [web_service, cli_tool, library, package]).

% === TESTS ===

% Test basic project entity existence
test(project_entity_exists, [true]) :-
    entity(project), !.

test(mkproject_entity_exists, [true]) :-
    entity(mkproject), !.

test(conjure_mkproject_entity_exists, [true]) :-
    entity(conjure(mkproject)), !.

% Test project command constructors
test(project_command_constructors, [true]) :-
    please_verify(component(conjure, ctor, mkproject)), !.

% Test project context entities
test(project_contexts, [true]) :-
    entity(project(context(build))), !,
    entity(project(context(runtime))), !,
    entity(project(context(test))), !.

% Test project docstrings exist
test(project_docstrings_exist, [true]) :-
    docstring(project, _), !.

% Test mkproject implementation exists
test(mkproject_implementation, [true]) :-
    % Test that mkproject conjure exists
    please_verify(component(conjure, ctor, mkproject)), !.

% Test DSL expansion rules work
test(project_dsl_expansion, [true]) :-
    please_verify(component(test_web_app, project_type, web_service)), !,
    please_verify(component(test_web_app, project_git_origin, 'https://github.com/test/web-app.git')), !,
    please_verify(component(test_web_app, has(git(repository)), git(repository(origin('https://github.com/test/web-app.git'))))), !.

% Test mkproject spell creates project directory
test(mkproject_creates_directory, [
    setup(setup_mkproject_test),
    cleanup(cleanup_mkproject_test)
]) :-
    TestPath = '/tmp/grimoire_test_mkproject',
    magic_cast(conjure(mkproject(TestPath, test_proj, [git(false)])), Result),
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
    magic_cast(conjure(mkproject(TestPath, test_git_proj, [])), Result),
    Result = ok(project_created(ProjectPath, test_git_proj)),
    directory_file_path(ProjectPath, '.git', GitDir),
    exists_directory(GitDir).

% Test mkproject with git(false) skips git init
test(mkproject_skip_git, [
    setup(setup_mkproject_test),
    cleanup(cleanup_mkproject_test)
]) :-
    TestPath = '/tmp/grimoire_test_mkproject_nogit',
    magic_cast(conjure(mkproject(TestPath, test_nogit_proj, [git(false)])), Result),
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
    magic_cast(conjure(mkproject(TestPath, test_template_proj, [template(python), git(false)])), Result),
    Result = ok(project_created(ProjectPath, test_template_proj)),
    exists_directory(ProjectPath),
    directory_file_path(ProjectPath, 'flake.nix', FlakeFile),
    exists_file(FlakeFile),
    directory_file_path(ProjectPath, 'semantics.pl', SemFile),
    exists_file(SemFile).

:- end_tests(project).

% === SETUP/CLEANUP ===

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
