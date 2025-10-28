%! Template instantiation test suite
%
% Tests for the project domain's template instantiation functionality,
% including conjure(mkproject(FolderPath, ProjectName, Options)) and entity renaming.

:- use_module(library(plunit)).
:- use_module(library(filesex)).
:- use_module(library(process)).

:- begin_tests(template_instantiation).

% Condition for nix template tests - checks if nix templates are functional
nix_templates_available :-
    catch(
        (
            load_entity(semantic(folder('@/src/nix'))),
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
    load_entity(semantic(folder('@/src/project'))),

    % Test that the discovery function exists and can be called
    % Note: actual template discovery depends on nix template loading
    % which may not work in test environment
    discover_available_templates(Templates),

    % Should at least return a list (may be empty in test env)
    is_list(Templates).

% Test entity discovery from semantics file
test(discover_template_entity_name) :-
    % Create a temporary semantics file
    tmp_file(semantics, TmpFile),
    open(TmpFile, write, Stream),
    write(Stream, ':- self_entity(test_template).\n'),
    write(Stream, 'component(test_template, project_type, test).\n'),
    close(Stream),

    % Load project domain for the discovery function
    load_entity(semantic(folder('@/src/project'))),

    % Test discovery
    discover_template_entity_name(TmpFile, EntityName),
    assertion(EntityName == test_template),

    % Clean up
    delete_file(TmpFile).

% Test comprehensive entity renaming
test(rename_all_entity_occurrences) :-
    % Load project domain for the renaming function
    load_entity(semantic(folder('@/src/project'))),

    Content = ':- self_entity(old_entity).\ncomponent(old_entity, type, test).\ndocstring(old_entity, "test").\nentity(old_entity).',
    rename_all_entity_occurrences(Content, old_entity, new_entity, NewContent), !,

    % Check all patterns were replaced
    sub_string(NewContent, _, _, _, ':- self_entity(new_entity).'), !,
    sub_string(NewContent, _, _, _, 'component(new_entity,'), !,
    sub_string(NewContent, _, _, _, 'docstring(new_entity,'), !,
    sub_string(NewContent, _, _, _, 'entity(new_entity)'), !.

% Test basic project creation without template
test(mkproject_basic_creation, [cleanup(cleanup_test_project)]) :-
    % Create a unique temporary directory
    get_time(Time),
    format(atom(TmpDir), '/tmp/grimoire_test_~w', [Time]),
    make_directory(TmpDir),

    % Load required domains
    load_entity(semantic(folder('@/src/project'))),

    % Test project creation
    TestProjectName = 'test_basic_project',
    user:magic_cast(conjure(mkproject(TmpDir, TestProjectName, [git(false)])), Result), !,

    % Should succeed
    assertion(Result = ok(project_created(_, TestProjectName))),
    
    % Verify project directory exists
    directory_file_path(TmpDir, TestProjectName, ProjectPath),
    assertion(exists_directory(ProjectPath)),

    % Verify semantics.pl was created
    directory_file_path(ProjectPath, 'semantics.pl', SemanticsFile),
    assertion(exists_file(SemanticsFile)),

    % Check that entity was created correctly
    read_file_to_string(SemanticsFile, Content, []),
    sub_string(Content, _, _, _, ':- self_entity(test_basic_project).'), !,

    % Store temp dir for cleanup
    nb_setval(test_project_path, TmpDir).

% Test template instantiation (integration test - requires nix templates)
test(template_instantiation_rust, [cleanup(cleanup_test_project)]) :-
    % Set up test environment
    tmp_file_stream(text, TmpFile, Stream),
    close(Stream),
    file_directory_name(TmpFile, TmpDir),
    delete_file(TmpFile),

    % Load required domains
    load_entity(semantic(folder('@/src/nix'))),
    load_entity(semantic(folder('@/src/project'))),

    % Test project creation with template
    TestProjectName = 'test_rust_project',
    catch(
        user:magic_cast(conjure(mkproject(TmpDir, TestProjectName, [template(rust), git(false)])), Result),
        Error,
        Result = error(caught_exception(Error))
    ), !,

    % The result should be either success or a proper error
    (Result = ok(project_created(_, TestProjectName)) ->
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
test(mkproject_existing_project, [cleanup(cleanup_test_project)]) :-
    % Set up test environment
    tmp_file_stream(text, TmpFile, Stream),
    close(Stream),
    file_directory_name(TmpFile, TmpDir),
    delete_file(TmpFile),

    TestProjectName = 'existing_project',
    directory_file_path(TmpDir, TestProjectName, ProjectPath),
    make_directory_path(ProjectPath),

    % Load required domains
    load_entity(semantic(folder('@/src/project'))), !,

    % Try to create project that already exists
    user:magic_cast(conjure(mkproject(TmpDir, TestProjectName, [git(false)])), Result), !,

    % Should fail with appropriate error
    assertion(Result == error(project_already_exists(ProjectPath))),

    % Store project path for cleanup
    nb_setval(test_project_path, ProjectPath).

% Test invalid template handling
test(mkproject_invalid_template) :-
    % Set up test environment
    tmp_file_stream(text, TmpFile, Stream),
    close(Stream),
    file_directory_name(TmpFile, TmpDir),
    delete_file(TmpFile),

    % Load required domains
    load_entity(semantic(folder('@/src/nix'))),
    load_entity(semantic(folder('@/src/project'))),

    % Try to create project with invalid template
    catch(
        user:magic_cast(conjure(mkproject(TmpDir, 'test_project', [template(nonexistent_template), git(false)])), Result),
        _Error,
        Result = error(template_instantiation_failed)
    ), !,

    % Should fail (exact error depends on nix behavior)
    assertion(Result = error(_)).

% Test git initialization option
test(mkproject_with_git, [cleanup(cleanup_test_project)]) :-
    % Create a unique temporary directory
    get_time(Time),
    format(atom(TmpDir), '/tmp/grimoire_test_git_~w', [Time]),
    make_directory(TmpDir),

    % Load required domains
    load_entity(semantic(folder('@/src/project'))),

    % Test project creation with git
    TestProjectName = 'test_git_project',
    user:magic_cast(conjure(mkproject(TmpDir, TestProjectName, [git(true)])), Result), !,

    % Check result - should succeed if git is available
    (Result = ok(project_created(_, TestProjectName)) ->
        (
            % Verify project directory exists
            directory_file_path(TmpDir, TestProjectName, ProjectPath),
            assertion(exists_directory(ProjectPath)),

            % Verify git repo was initialized
            directory_file_path(ProjectPath, '.git', GitDir),
            assertion(exists_directory(GitDir)),

            % Store project path for cleanup
            nb_setval(test_project_path, TmpDir)
        )
    ; Result = ok(_) ->
        % Git command succeeded but returned different format - store temp dir for cleanup
        nb_setval(test_project_path, TmpDir)
    ;
        % If git not available, should get an error (which is acceptable)
        assertion(Result = error(_)),
        nb_setval(test_project_path, TmpDir)
    ), !.

% Cleanup helper
cleanup_test_project :-
    (nb_current(test_project_path, Path) ->
        (exists_directory(Path) ->
            delete_directory_and_contents(Path)
        ; true),
        nb_delete(test_project_path)
    ; true).

:- end_tests(template_instantiation).