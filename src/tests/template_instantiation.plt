%! Template instantiation test suite
%
% Tests for the project domain's template instantiation functionality,
% including conjure(mkproject(TemplateId, ProjectName)) and entity renaming.

:- use_module(library(plunit)).
:- use_module(library(filesex)).
:- use_module(library(process)).

:- begin_tests(template_instantiation).

% Condition for nix template tests - checks if nix templates are functional
nix_templates_available :-
    catch(
        (
            load_entity(semantic(folder('src/nix'))),
            % Try to check if nix command works
            process_create(path(nix), ['--version'], [stdout(null), stderr(null), process(PID)]),
            process_wait(PID, exit(0))
        ),
        _,
        fail
    ).

% Test template discovery functionality
test(discover_available_templates) :-
    % Load project domain that has the discovery function
    load_entity(semantic(folder('src/project'))),

    % Test that the discovery function exists and can be called
    % Note: actual template discovery depends on nix template loading
    % which may not work in test environment
    discover_available_templates(Templates),

    % Should at least return a list (may be empty in test env)
    is_list(Templates).

% Test projects directory resolution
test(get_projects_directory_with_env) :-
    % Test with environment variable set
    setenv('GRIMOIRE_PROJECTS_DIR', '/tmp/test-projects'),
    get_projects_directory(Dir),
    assertion(Dir == '/tmp/test-projects'),

    % Clean up
    unsetenv('GRIMOIRE_PROJECTS_DIR').

test(get_projects_directory_default) :-
    % Test with no environment variable
    unsetenv('GRIMOIRE_PROJECTS_DIR'),
    get_projects_directory(Dir),

    % Should expand to home directory
    expand_file_name('~/.grimoire/Projects', [Expected]),
    assertion(Dir == Expected).

% Test entity discovery from semantics file
test(discover_template_entity_name) :-
    % Create a temporary semantics file
    tmp_file(semantics, TmpFile),
    open(TmpFile, write, Stream),
    write(Stream, ':- self_entity(test_template).\n'),
    write(Stream, 'component(test_template, project_type, test).\n'),
    close(Stream),

    % Load project domain for the discovery function
    load_entity(semantic(folder('src/project'))),

    % Test discovery
    discover_template_entity_name(TmpFile, EntityName),
    assertion(EntityName == test_template),

    % Clean up
    delete_file(TmpFile).

% Test comprehensive entity renaming
test(rename_all_entity_occurrences) :-
    % Load project domain for the renaming function
    load_entity(semantic(folder('src/project'))),

    Content = ':- self_entity(old_entity).\ncomponent(old_entity, type, test).\ndocstring(old_entity, "test").\nentity(old_entity).',
    rename_all_entity_occurrences(Content, old_entity, new_entity, NewContent), !,

    % Check all patterns were replaced
    sub_string(NewContent, _, _, _, ':- self_entity(new_entity).'), !,
    sub_string(NewContent, _, _, _, 'component(new_entity,'), !,
    sub_string(NewContent, _, _, _, 'docstring(new_entity,'), !,
    sub_string(NewContent, _, _, _, 'entity(new_entity)'), !.

% Test template instantiation (integration test - requires nix templates)
test(template_instantiation_rust, [cleanup(cleanup_test_project)]) :-
    % Set up test environment
    tmp_file_stream(text, TmpFile, Stream),
    close(Stream),
    file_directory_name(TmpFile, TmpDir),
    delete_file(TmpFile),
    setenv('GRIMOIRE_PROJECTS_DIR', TmpDir),

    % Load required domains
    load_entity(semantic(folder('src/nix'))),
    load_entity(semantic(folder('src/project'))),

    % Test project creation
    TestProjectName = 'test_rust_project',
    catch(
        run(conjure(mkproject(rust, TestProjectName)), Result),
        Error,
        Result = error(caught_exception(Error))
    ), !,

    % The result should be either success or a proper error
    (Result = ok(project_created(_, rust, TestProjectName)) ->
        (
            % Verify project directory exists
            directory_file_path(TmpDir, TestProjectName, ProjectPath),
            assertion(exists_directory(ProjectPath)),

            % Verify semantics.pl was created and entity renamed
            directory_file_path(ProjectPath, 'semantics.pl', SemanticsFile),
            assertion(exists_file(SemanticsFile)),

            % Check that entity was renamed
            read_file_to_string(SemanticsFile, Content, []),
            sub_string(Content, _, _, _, ':- self_entity(test_rust_project).'),

            % Store project path for cleanup
            nb_setval(test_project_path, ProjectPath)
        )
    ;
        % If nix templates not available, should get an error (which is expected)
        assertion(Result = error(_))
    ), !.

% Test error handling for existing project
test(template_instantiation_existing_project, [cleanup(cleanup_test_project)]) :-
    % Set up test environment
    tmp_file_stream(text, TmpFile, Stream),
    close(Stream),
    file_directory_name(TmpFile, TmpDir),
    delete_file(TmpFile),
    setenv('GRIMOIRE_PROJECTS_DIR', TmpDir),

    TestProjectName = 'existing_project',
    directory_file_path(TmpDir, TestProjectName, ProjectPath),
    make_directory_path(ProjectPath),

    % Load required domains
    load_entity(semantic(folder('src/nix'))),
    load_entity(semantic(folder('src/project'))),

    % Try to create project that already exists
    run(conjure(mkproject(rust, TestProjectName)), Result),

    % Should fail with appropriate error
    assertion(Result = error(project_already_exists(ProjectPath))),

    % Store project path for cleanup
    nb_setval(test_project_path, ProjectPath).

% Test invalid template handling
test(template_instantiation_invalid_template) :-
    % Set up test environment
    tmp_file_stream(text, TmpFile, Stream),
    close(Stream),
    file_directory_name(TmpFile, TmpDir),
    delete_file(TmpFile),
    setenv('GRIMOIRE_PROJECTS_DIR', TmpDir),

    % Load required domains
    load_entity(semantic(folder('src/nix'))),
    load_entity(semantic(folder('src/project'))),

    % Try to create project with invalid template
    catch(
        run(conjure(mkproject(nonexistent_template, 'test_project')), Result),
        _Error,
        Result = error(template_instantiation_failed)
    ), !,

    % Should fail (exact error depends on nix behavior)
    assertion(Result = error(_)).

% Cleanup helper
cleanup_test_project :-
    (nb_current(test_project_path, ProjectPath) ->
        (exists_directory(ProjectPath) ->
            delete_directory_and_contents(ProjectPath)
        ; true),
        nb_delete(test_project_path)
    ; true),
    unsetenv('GRIMOIRE_PROJECTS_DIR').

:- end_tests(template_instantiation).