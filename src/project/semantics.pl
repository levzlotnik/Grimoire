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

docstring(environment,
    {|string(_)||
    Environment entity representing a complete set of resources for all the project's contexts.
    - Build environment (compilers, tools)
    - Runtime environment (libraries, data)
    - Testing environment ()
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
component(command, ctor, mkproject).

% Template discovery - query nix domain for available templates
discover_available_templates(Templates) :-
    findall(TemplateId,
            component(nix(flake(template)), instance, nix(flake(template(TemplateId)))),
            Templates).

% Helper to get project directory from environment or default
get_projects_directory(ProjectsDir) :-
    (getenv('GRIMOIRE_PROJECTS_DIR', EnvDir) ->
        ProjectsDir = EnvDir
    ;
        expand_file_name('~/.grimoire/Projects', [ProjectsDir])
    ).

docstring(mkproject,
    {|string(_)||
    Creates a new project directory with full initialization.
    Format: command(mkproject(+Path, +Options)) or cast(conjure(mkproject(TemplateId, ProjectName))).
    Options:
    - git(bool)          : Initialize git repo (default: true)
    - template(Template) : Flake template to use (default: none)
    - lang(Language)     : Programming language (affects template)

    Conjure variant uses GRIMOIRE_PROJECTS_DIR environment variable or defaults to ~/.grimoire/Projects/
    |}
).

% Conjure implementation for mkproject
run(conjure(mkproject(TemplateId, ProjectName)), RetVal) :-
    catch(
        (
            % Get projects directory
            get_projects_directory(ProjectsDir),
            directory_file_path(ProjectsDir, ProjectName, DestPath),

            % Ensure projects directory exists
            (exists_directory(ProjectsDir) ->
                true
            ;
                make_directory_path(ProjectsDir)
            ),

            % Check if project already exists
            (exists_directory(DestPath) ->
                RetVal = error(project_already_exists(DestPath))
            ;
                % Use nix flake new to instantiate template
                catch(
                    run(command(nix(flake(new(TemplateId, DestPath)))), NixResult),
                    NixError,
                    NixResult = error(nix_command_failed(NixError))
                ),
                (NixResult = ok(_) ->
                    % Post-process: rename entities in semantics files
                    rename_project_entities(DestPath, TemplateId, ProjectName, RenameResult),
                    (RenameResult = ok ->
                        RetVal = ok(project_created(DestPath, TemplateId, ProjectName))
                    ;
                        RetVal = error(entity_renaming_failed(RenameResult))
                    )
                ;
                    RetVal = NixResult
                )
            )
        ),
        Error,
        RetVal = error(conjure_failed(Error))
    ).

% Entity renaming in generated project files
rename_project_entities(ProjectPath, TemplateId, ProjectName, Result) :-
    % Discover the actual entity name from the generated semantics.pl file
    directory_file_path(ProjectPath, 'semantics.pl', SemanticsFile),
    (exists_file(SemanticsFile) ->
        discover_template_entity_name(SemanticsFile, TemplateEntityName)
    ;
        % Fallback: assume template_id naming pattern, but this will evolve
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

% Helper predicate for string replacement
string_replace_all(String, Old, New, Result) :-
    re_replace(Old, New, String, Result, [global]).

% DB entity for project database
entity(db).
component(db, source, source(semantic(folder("db")))).

run(command(mkproject(Path, Options)), RetVal) :-
    % Create project directory with semantics
    run(command(mkdir(Path)), RetVal0),
    (RetVal0 = error(_) -> RetVal = RetVal0
    ;
        % Create .mypaos and initialize agent DB
        directory_file_path(Path, ".mypaos", MypaosDir),
        run(command(mkdir(MypaosDir)), RetVal1),
        (RetVal1 = error(_) -> RetVal = RetVal1
        ;
            % Initialize git if requested (before DB so we can track .mypaos)
            (option(git(false), Options) -> RetVal2 = ok("")
            ;
                run(command(git(init(Path))), RetVal2)
            ),
            (RetVal2 = error(_) -> RetVal = RetVal2
            ;
                % Initialize DB using db_semantics module
                directory_file_path(MypaosDir, "agent.db", DbPath),
                catch(
                    init_agent_db(DbPath),
                    db_error(E),
                    (RetVal = error(E), !, fail)
                ),
                % Apply template if specified
                (option(template(Template), Options, none) ->
                    run(command(nix(flake(init(Template)))), RetVal4)
                ; RetVal4 = ok("")),
                (RetVal4 = error(_) -> RetVal = RetVal4
                ;
                    % Language setup stub for now
                    RetVal = ok("Project created successfully")
                )
            )
        )
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

% General project command implementations using discovered Nix targets
% These work for any project entity that has discovered Nix targets

% Build command - uses nix build
run(command(Cmd), RetVal) :-
    Cmd =.. [Entity, build],
    entity(Entity),
    component(Entity, nix_target, app(FlakeRef, _, build)),
    !,
    run(command(nix(build(FlakeRef))), RetVal).

run(command(Cmd), RetVal) :-
    Cmd =.. [Entity, build],
    entity(Entity),
    % Fallback: use current directory if no specific build target
    run(command(nix(build("."))), RetVal).

% Test command - uses nix check or test target
run(command(Cmd), RetVal) :-
    Cmd =.. [Entity, test],
    entity(Entity),
    component(Entity, nix_target, app(FlakeRef, _, test)),
    !,
    format(atom(Target), '~w#test', [FlakeRef]),
    run(command(nix(run(Target))), RetVal).

run(command(Cmd), RetVal) :-
    Cmd =.. [Entity, test],
    entity(Entity),
    % Fallback: use nix flake check
    run(command(nix(check("."))), RetVal).

% Run command - uses nix run
run(command(Cmd), RetVal) :-
    Cmd =.. [Entity, run],
    entity(Entity),
    component(Entity, nix_target, app(FlakeRef, _, run)),
    !,
    format(atom(Target), '~w#run', [FlakeRef]),
    run(command(nix(run(Target))), RetVal).

run(command(Cmd), RetVal) :-
    Cmd =.. [Entity, run],
    entity(Entity),
    component(Entity, nix_target, app(FlakeRef, _, default)),
    !,
    run(command(nix(run(FlakeRef))), RetVal).

% Generic command for any discovered Nix target
run(command(Cmd), RetVal) :-
    Cmd =.. [Entity, Command],
    entity(Entity),
    component(Entity, nix_target, app(FlakeRef, _, Command)),
    format(atom(Target), '~w#~w', [FlakeRef, Command]),
    run(command(nix(run(Target))), RetVal).

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
    maplist(assert_component, Components).

% Assert discovered component
assert_component(Component) :-
    assertz(Component).

