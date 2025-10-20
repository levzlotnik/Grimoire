% Project domain tests - Phase 3 behavioral testing

:- use_module(library(plunit)).

% Load domain semantics first
:- grimoire_ensure_loaded('@/src/project/semantics.pl').

% Load dependent domains (project uses git and nix spells)
:- grimoire_ensure_loaded('@/src/git.pl').
:- grimoire_ensure_loaded('@/src/nix/semantics.pl').

% Load test entities from project test subdirectories
:- load_entity(semantic(folder('@/src/project/tests/test_web_app'))).
:- load_entity(semantic(folder('@/src/project/tests/test_cli_tool'))).
:- load_entity(semantic(folder('@/src/project/tests/test_library'))).
:- load_entity(semantic(folder('@/src/project/tests/test_project_with_context'))).
:- load_entity(semantic(folder('@/src/project/tests/test_project_no_git'))).
:- load_entity(semantic(folder('@/src/project/tests/test_project_invalid_type'))).

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
    setup(setup_project_test),
    cleanup(cleanup_project_test)
]) :-
    user:please_verify(component(test_web_app, project_type, Type)),
    assertion(Type = web_service).

test(project_app_expands_to_git_origin, [
    setup(setup_project_test),
    cleanup(cleanup_project_test)
]) :-
    user:please_verify(component(test_web_app, project_git_origin, Origin)),
    assertion(Origin = 'https://github.com/test/web-app.git').

test(project_app_expands_to_nix_flake, [
    setup(setup_project_test),
    cleanup(cleanup_project_test)
]) :-
    user:please_verify(component(test_web_app, project_nix_flake, NixFlake)),
    assertion(NixFlake = nix(flake('.'))).

test(project_app_expands_to_fs_structure, [
    setup(setup_project_test),
    cleanup(cleanup_project_test)
]) :-
    user:please_verify(component(test_web_app, project_fs_structure, Structure)),
    assertion(is_list(Structure)).

% === REVERSE BRIDGE TESTS (Cross-domain triggering) ===

test(project_git_origin_triggers_git_repository, [
    setup(setup_project_test),
    cleanup(cleanup_project_test)
]) :-
    user:please_verify(component(test_web_app, has(git(repository)), _)).

test(project_nix_flake_triggers_nix_schema, [
    setup(setup_project_test),
    cleanup(cleanup_project_test)
]) :-
    user:please_verify(component(test_web_app, has(nix(flake)), _)).

% === COMPOSITE VERIFICATION TESTS ===

test(project_app_composite_verification, [
    setup(setup_project_test),
    cleanup(cleanup_project_test)
]) :-
    % This verifies all expanded components using please_verify composition
    user:please_verify(component(test_web_app, has(project(app)), _)).

% === LEAF VERIFICATION TESTS ===

test(project_type_verification_valid, [
    setup(setup_project_test),
    cleanup(cleanup_project_test)
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
    TestPath = '/tmp/test_mkproject_basic',
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
    TestPath = '/tmp/test_mkproject_git',
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
    TestPath = '/tmp/test_mkproject_template',
    (exists_directory(TestPath) -> delete_directory_and_contents(TestPath) ; true),
    make_directory_path(TestPath),
    user:magic_cast(conjure(mkproject(TestPath, test_template, [template(python), git(false)])), Result),
    assertion(Result = ok(project_created(_, test_template))),
    directory_file_path(TestPath, 'test_template', ProjectPath),
    directory_file_path(ProjectPath, 'flake.nix', FlakeFile),
    assertion(exists_file(FlakeFile)).

% Test project validate spell
test(spell_project_validate, [
    setup(setup_project_test),
    cleanup(cleanup_project_test)
]) :-
    user:magic_cast(perceive(project(validate(test_web_app))), Result),
    assertion(Result = ok(valid)).

% Test project structure spell
test(spell_project_structure, [
    setup(setup_project_test),
    cleanup(cleanup_project_test)
]) :-
    user:magic_cast(perceive(project(structure(test_web_app))), Result),
    assertion(Result = ok(project_info(type(web_service), sources(_), contexts(_)))).

:- end_tests(project).

% === SETUP/CLEANUP HELPERS ===

% Project test setup - create filesystem resources
setup_project_test :-
    TestPath = '/tmp/test_project_entities',
    (exists_directory(TestPath) -> delete_directory_and_contents(TestPath) ; true),
    make_directory_path(TestPath).

cleanup_project_test :-
    TestPath = '/tmp/test_project_entities',
    (exists_directory(TestPath) -> delete_directory_and_contents(TestPath) ; true).

% Mkproject test cleanup
cleanup_mkproject_test :-
    delete_if_exists('/tmp/test_mkproject_basic'),
    delete_if_exists('/tmp/test_mkproject_git'),
    delete_if_exists('/tmp/test_mkproject_template').

delete_if_exists(Dir) :-
    (exists_directory(Dir) ->
        delete_directory_and_contents(Dir)
    ; true).
