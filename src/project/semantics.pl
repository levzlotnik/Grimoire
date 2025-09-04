% Core entities
:- self_entity(project).

entity(package).
entity(application).
entity(environment).
entity(context).
entity(language).
entity(build).
entity(runtime).
entity(test).
entity(mkproject).
entity(conjure(mkproject)).

component(project, concept, application).
component(project, concept, context).
component(project, concept, environment).
component(project, concept, source).

component(application, ctor, program).
component(application, ctor, library).
component(application, ctor, service).
component(package, ctor, application).

component(application, requires, context(C)) :- component(context, ctor, C).
component(context, requires, environment).

component(context, ctor, build).
component(context, ctor, runtime).
component(context, ctor, test).
entity(context(C)) :- component(context, ctor, C).

% Config as a distinct entity
entity(config).
component(config, ctor, C) :- entity(context(C)).
entity(config(C)) :- component(config, ctor, C).

% Dependencies as a distinct entity
entity(deps).
component(deps, ctor, C) :- entity(context(C)).
entity(deps(C)) :- component(deps, ctor, C).

% Context-config relationships
component(context(C), requires, config(C)) :-
    entity(context(C)), entity(config(C)).

component(context(C), requires, deps(C)) :-
    entity(context(C)), entity(deps(C)).

component(context(build), requires, source(code)).
component(context(runtime), requires, source(data)).

component(source, ctor, code).
component(source, ctor, data).

entity(source(code)).
entity(source(data)).

docstring(source(code),
   {|string(_)||
   Represents source of programming language code.
   Format:
     source(code(lang(Lang), file(...))) - file as source code
     source(code(lang(Lang), folder(...))) - a folder full of source code
   |}).

docstring(source(data),
   {|string(_)||
   Represents a source of data.
   |}).

docstring(config,
    {|string(_)||
    Configuration entity for different project contexts.
    Configs determine how contexts behave during build and runtime.
    |}
).

docstring(deps,
    {|string(_)||
    Dependencies entity representing required resources.
    Build deps are source-level needs (compilers, etc).
    Runtime deps are execution needs (libraries, etc).
    |}
).

docstring(package,
    {|string(_)||
    Package entity representing a distributable unit of software.
    A package wraps a set of applications with their respective dependencies and configurations
    into a deployable form.
    |}).

docstring(application,
    {|string(_)||
    Application entity representing a software system's intent and behavior.
    Applications can both consume AND produce runtime dependencies:
    - Services consume and produce (e.g., API endpoints)
    - Frameworks primarily produce (e.g., web servers)
    - Libraries produce factories (e.g., database connections)
    |}).

docstring(project,
    {|string(_)||
    Project entity representing the development context of software.
    Projects provide structure for source code, build systems, and development
    tools. They materialize applications into concrete implementations.
    |}).

docstring(context(build),
    {|string(_)||
    Build context configuration for project compilation.
    Defines build-time environment, tools, and settings.
    Part of the project's three-phase context system.
    Requires config(build) and deps(build) for complete specification.
    |}).

docstring(context(runtime),
    {|string(_)||
    Runtime context configuration for project execution.
    Defines runtime environment, dependencies, and settings.
    Part of the project's three-phase context system.
    Requires config(runtime) and deps(runtime) for complete specification.
    |}).

docstring(context(test),
    {|string(_)||
    Test context configuration for project testing.
    Defines test environment, frameworks, and test-specific settings.
    Part of the project's three-phase context system.
    Requires config(test) and deps(test) for complete specification.
    |}).

docstring(config(build),
    {|string(_)||
    Build configuration settings and parameters.
    Stores build-specific configuration like compiler flags, optimization levels.
    Associated with context(build) for complete build specification.
    Provides declarative build configuration management.
    |}).

docstring(config(runtime),
    {|string(_)||
    Runtime configuration settings and parameters.
    Stores runtime-specific configuration like environment variables, resource limits.
    Associated with context(runtime) for complete runtime specification.
    Provides declarative runtime configuration management.
    |}).

docstring(config(test),
    {|string(_)||
    Test configuration settings and parameters.
    Stores test-specific configuration like test runners, coverage settings.
    Associated with context(test) for complete test specification.
    Provides declarative test configuration management.
    |}).

docstring(deps(build),
    {|string(_)||
    Build-time dependencies specification.
    Lists packages and tools required during compilation/build phase.
    Managed through package manager integrations like Nix.
    Provides declarative build dependency management.
    |}).

docstring(deps(runtime),
    {|string(_)||
    Runtime dependencies specification.
    Lists packages and libraries required during program execution.
    Managed through package manager integrations like Nix.
    Provides declarative runtime dependency management.
    |}).

docstring(deps(test),
    {|string(_)||
    Test dependencies specification.
    Lists test frameworks and tools required for testing.
    Managed through package manager integrations like Nix.
    Provides declarative test dependency management.
    |}).

docstring(environment,
    {|string(_)||
    Environment entity representing a complete set of resources for all the project's contexts.
    - Build environment (compilers, tools)
    - Runtime environment (libraries, data)
    - Testing environment (test frameworks, mock data)
    - Development environment (editors, debuggers)
    |}).

docstring(context,
    {|string(_)||
    Context entity representing a phase in the software lifecycle.
    Contexts define how software is built, run, and tested.
    Each context has its own configuration and dependency requirements.
    |}).

docstring(language,
    {|string(_)||
    Language entity representing a formal system for expressing computation.
    Languages appear in multiple forms:
    - Source language (how we write code)
    - Interface language (how components communicate)
    - Configuration language (how we describe behavior)
    |}).

docstring(build,
    {|string(_)||
    Build entity representing the transformation from source to artifacts.
    Build is a function from build dependencies and configuration
    to runtime dependencies (executables, libraries, resources).
    |}).

docstring(runtime,
    {|string(_)||
    Runtime entity representing program execution context.
    Runtime combines configuration with dependencies to create
    an environment where applications can execute.
    |}).

docstring(test,
    {|string(_)||
    Test entity representing verification contexts.
    Tests verify application behavior through:
    - Unit tests (component behavior)
    - Integration tests (component interaction)
    - System tests (complete application)
    |}).

% Template discovery and conjure integration
component(conjure, ctor, mkproject).

% Template discovery - query nix domain for available templates
discover_available_templates(Templates) :-
    findall(TemplateId,
            component(nix(flake(template)), instance, nix(flake(template(TemplateId)))),
            Templates).

docstring(mkproject,
    {|string(_)||
    Creates a new project directory with full initialization.
    Format: cast(conjure(mkproject(FolderPath, ProjectName, Options))).
    Parameters:
    - FolderPath: Base path where to create the project
    - ProjectName: Name of the project and entity ID for semantics.pl
    - Options: List of options:
      - git(bool): Initialize git repo (default: true)
      - template(Template): Flake template to use (default: none)
    |}).

docstring(conjure(mkproject), S) :- docstring(mkproject, S).

% Unified mkproject implementation
cast(conjure(mkproject(FolderPath, ProjectName, Options)), RetVal) :-
    catch(
        (
            % Create full project path
            directory_file_path(FolderPath, ProjectName, ProjectPath),

            % Ensure base folder exists
            (exists_directory(FolderPath) ->
                true
            ;
                make_directory_path(FolderPath)
            ),

            % Check if project already exists
            (exists_directory(ProjectPath) ->
                RetVal = error(project_already_exists(ProjectPath))
            ;
                % Create project directory
                make_directory(ProjectPath),

                % Apply template if specified
                (member(template(Template), Options) ->
                    cast(conjure(nix(flake(new(Template, ProjectPath)))), TemplateResult),
                    (TemplateResult = ok(_) ->
                        % Rename entities in generated files
                        rename_project_entities(ProjectPath, Template, ProjectName, RenameResult)
                    ;
                        RenameResult = TemplateResult
                    )
                ;
                    % Create basic semantics.pl if no template
                    create_basic_semantics_file(ProjectPath, ProjectName),
                    RenameResult = ok
                ),

                (RenameResult = ok ->
                    % Initialize git if requested
                    (member(git(false), Options) ->
                        GitResult = ok
                    ;
                        cast(conjure(git(init(ProjectPath))), GitResult)
                    ),

                    (GitResult = ok ->
                        RetVal = ok(project_created(ProjectPath, ProjectName))
                    ;
                        RetVal = GitResult
                    )
                ;
                    RetVal = error(entity_renaming_failed(RenameResult))
                )
            )
        ),
        Error,
        RetVal = error(conjure_failed(Error))
    ).

% Create basic semantics.pl file when no template is used
create_basic_semantics_file(ProjectPath, ProjectName) :-
    directory_file_path(ProjectPath, 'semantics.pl', SemanticsFile),
    open(SemanticsFile, write, Stream),
    format(Stream, ':- self_entity(~w).~n~n', [ProjectName]),
    format(Stream, 'docstring(~w,~n    {|string(_)||~n    ~w project entity.~n    |}).~n', [ProjectName, ProjectName]),
    close(Stream).

% Entity renaming in generated project files
rename_project_entities(ProjectPath, TemplateId, ProjectName, Result) :-
    % Discover the actual entity name from the generated semantics.pl file
    directory_file_path(ProjectPath, 'semantics.pl', SemanticsFile),
    (exists_file(SemanticsFile) ->
        discover_template_entity_name(SemanticsFile, TemplateEntityName)
    ;
        % Fallback: assume template_id naming pattern
        format(atom(TemplateEntityName), '~w_template', [TemplateId])
    ),

    % Find semantics.pl and semantics.plt files in project directory
    findall(File, (
        member(FileName, ['semantics.pl', 'semantics.plt']),
        directory_file_path(ProjectPath, FileName, File),
        exists_file(File)
    ), SemanticsFiles),

    % Rename entities in each file
    maplist(rename_entities_in_file(TemplateEntityName, ProjectName), SemanticsFiles, Results),

    % Check if all renamings succeeded
    (member(error(_), Results) ->
        Result = error(Results)
    ;
        Result = ok
    ).

% Rename entities in a single file
rename_entities_in_file(OldEntityName, NewEntityName, FilePath, Result) :-
    catch(
        (
            % Read the entire file
            read_file_to_string(FilePath, Content, []),

            % Perform comprehensive entity renaming
            rename_all_entity_occurrences(Content, OldEntityName, NewEntityName, NewContent),

            % Write back to file
            open(FilePath, write, Stream),
            write(Stream, NewContent),
            close(Stream),

            Result = ok
        ),
        Error,
        Result = error(Error)
    ).

% Comprehensive entity renaming in content
rename_all_entity_occurrences(Content, OldEntity, NewEntity, NewContent) :-
    % Create atom names for replacement
    atom_string(OldEntityAtom, OldEntity),
    atom_string(NewEntityAtom, NewEntity),

    % Replace various patterns:
    % 1. :- self_entity(OldEntity).
    format(string(OldSelfEntity), ':- self_entity(~w).', [OldEntityAtom]),
    format(string(NewSelfEntity), ':- self_entity(~w).', [NewEntityAtom]),

    % 2. component(OldEntity, ...)
    format(string(OldComponentPattern), 'component(~w,', [OldEntityAtom]),
    format(string(NewComponentPattern), 'component(~w,', [NewEntityAtom]),

    % 3. entity(OldEntity)
    format(string(OldEntityPattern), 'entity(~w)', [OldEntityAtom]),
    format(string(NewEntityPattern), 'entity(~w)', [NewEntityAtom]),

    % 4. docstring(OldEntity, ...)
    format(string(OldDocPattern), 'docstring(~w,', [OldEntityAtom]),
    format(string(NewDocPattern), 'docstring(~w,', [NewEntityAtom]),

    % Apply all replacements using literal replacement
    re_replace(OldSelfEntity, NewSelfEntity, Content, Content1, [literal, global]),
    re_replace(OldComponentPattern, NewComponentPattern, Content1, Content2, [literal, global]),
    re_replace(OldEntityPattern, NewEntityPattern, Content2, Content3, [literal, global]),
    re_replace(OldDocPattern, NewDocPattern, Content3, NewContent, [literal, global]).

% Discover the entity name from a semantics.pl file
discover_template_entity_name(FilePath, EntityName) :-
    catch(
        (
            read_file_to_string(FilePath, Content, []),
            % Look for :- self_entity(EntityName).
            re_matchsub(':- self_entity\\(([^)]+)\\)\\.', Content, Match, []),
            get_dict(1, Match, EntityNameString),
            atom_string(EntityName, EntityNameString)
        ),
        _Error,
        fail
    ).

% Project artifact discovery utilities
:- use_module(library(filesex)).
:- use_module(library(strings)).

% Project utility commands
component(project, utility, discover_project_artifacts).
component(project, utility, discover_core_artifacts).
component(project, utility, discover_nix_targets).
component(project, utility, validate_project_structure).

% Core project artifact types (non-negotiable)
component(project, core_artifact, nix(flake)).
component(project, core_artifact, git).
component(project, core_artifact, readme).
component(project, core_artifact, sources).

% Main project discovery predicate with options
discover_project_artifacts(Entity, Options) :-
    % Discover core artifacts first (always included)
    discover_core_artifacts(Entity),
    % Discover Nix targets and expose them as components
    discover_nix_targets(Entity),
    % Then discover custom filesystem patterns
    (member(fs_patterns(IncludePatterns, ExcludePatterns), Options) ->
        Patterns = [IncludePatterns, ExcludePatterns]
    ;
        Patterns = []
    ),
    (Patterns \= [] ->
        discover_custom_artifacts(Entity, Patterns)
    ;
        true
    ).

% Default discovery without custom patterns
discover_project_artifacts(Entity) :-
    discover_core_artifacts(Entity),
    discover_nix_targets(Entity).

% Discover core project artifacts (non-negotiable)
discover_core_artifacts(Entity) :-
    working_directory(CurrentDir, CurrentDir),
    % Discover flake.nix (every project must have one)
    discover_flake_artifact(Entity, CurrentDir),
    % Discover git repository (every project must be in git)
    discover_git_artifact(Entity, CurrentDir),
    % Discover README (every project must have documentation)
    discover_readme_artifact(Entity, CurrentDir),
    % Discover source files (inferred from git)
    discover_sources_artifact(Entity, CurrentDir).

% Discover Nix targets and expose them as project components
discover_nix_targets(Entity) :-
    working_directory(CurrentDir, CurrentDir),
    FlakePath = 'flake.nix',
    directory_file_path(CurrentDir, FlakePath, FlakeFullPath),
    (exists_file(FlakeFullPath) ->
        % Use nix flake show to get available targets
        process_create(path(nix), ['flake', 'show', '--json'], [
            stdout(pipe(Out)),
            cwd(CurrentDir)
        ]),
        read_string(Out, _, JsonString),
        close(Out),
        atom_string(JsonAtom, JsonString),
        atom_json_term(JsonAtom, JsonDict, []),
        % Extract apps and expose as nix_target components
        extract_nix_apps(Entity, JsonDict, CurrentDir)
    ;
        true  % No flake, skip Nix target discovery
    ).

% Extract Nix apps from flake show JSON and create components
extract_nix_apps(Entity, JsonDict, FlakeRef) :-
    (get_dict(apps, JsonDict, Apps) ->
        dict_pairs(Apps, _, SystemPairs),
        member(System-SystemApps, SystemPairs),
        dict_pairs(SystemApps, _, AppPairs),
        member(AppName-_, AppPairs),
        format(atom(AttrPath), 'apps.~w.~w', [System, AppName]),
        assertz(component(Entity, nix_target, app(FlakeRef, AttrPath, AppName)))
    ;
        true  % No apps section
    ).


% Discover flake.nix artifact
discover_flake_artifact(Entity, BaseDir) :-
    FlakePath = 'flake.nix',
    directory_file_path(BaseDir, FlakePath, FlakeFullPath),
    (exists_file(FlakeFullPath) ->
        assertz(component(Entity, core_artifact, nix(flake(FlakeFullPath))))
    ;
        assertz(component(Entity, missing_core_artifact, nix(flake)))
    ).

% Discover git repository artifact
discover_git_artifact(Entity, BaseDir) :-
    GitDir = '.git',
    directory_file_path(BaseDir, GitDir, GitFullPath),
    (exists_directory(GitFullPath) ->
        assertz(component(Entity, core_artifact, git(repository(BaseDir))))
    ;
        assertz(component(Entity, missing_core_artifact, git))
    ).

% Discover README artifact
discover_readme_artifact(Entity, BaseDir) :-
    ReadmePatterns = ['README.md', 'README.rst', 'README.txt', 'README'],
    find_first_existing_file(BaseDir, ReadmePatterns, ReadmePath),
    (ReadmePath \= none ->
        assertz(component(Entity, core_artifact, readme(ReadmePath)))
    ;
        assertz(component(Entity, missing_core_artifact, readme))
    ).

% Find first existing file from pattern list
find_first_existing_file(_, [], none).
find_first_existing_file(BaseDir, [Pattern|Rest], Result) :-
    directory_file_path(BaseDir, Pattern, FullPath),
    (exists_file(FullPath) ->
        Result = FullPath
    ;
        find_first_existing_file(BaseDir, Rest, Result)
    ).

% Discover source files (git-tracked files)
discover_sources_artifact(Entity, BaseDir) :-
    process_create(path(git), ['ls-files'], [
        stdout(pipe(Out)),
        cwd(BaseDir)
    ]),
    read_lines_from_stream(Out, Lines),
    close(Out),
    maplist(assert_source_file(Entity, BaseDir), Lines).

% Read all lines from a stream
read_lines_from_stream(Stream, Lines) :-
    read_line_to_codes(Stream, Codes),
    (Codes == end_of_file ->
        Lines = []
    ;
        atom_codes(Line, Codes),
        Lines = [Line|RestLines],
        read_lines_from_stream(Stream, RestLines)
    ).

% Assert source file component
assert_source_file(Entity, BaseDir, RelativePath) :-
    directory_file_path(BaseDir, RelativePath, FullPath),
    (exists_file(FullPath) ->
        assertz(component(Entity, source_file, file(RelativePath)))
    ;
        true  % Skip non-existent files
    ).

% Discover custom filesystem artifacts based on patterns
discover_custom_artifacts(Entity, [IncludePatterns, ExcludePatterns]) :-
    working_directory(CurrentDir, CurrentDir),
    discover_fs_components(Entity, CurrentDir,
        [include(IncludePatterns), exclude(ExcludePatterns)],
        Components),
    maplist(assertz, Components).
