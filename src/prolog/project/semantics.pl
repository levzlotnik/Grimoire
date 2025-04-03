% Dynamic declarations
:- dynamic entity/1.
:- dynamic component/3.

% Core entities
entity(package).
entity(application).
entity(project).
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

component(command, ctor, mkproject).

docstring(mkproject,
    {|string(_)||
    Creates a new project directory with full initialization.
    Format: command(mkproject(+Path, +Options)).
    Options:
    - git(bool)          % Initialize git repo (default: true)
    - template(Template) % Flake template to use (default: none)
    - lang(Language)     % Programming language (affects template)
    |}
).

% Load DB initialization module
entity(db).
component(db, source, source(semantic(folder("db")))) :- !.
:- load_entity_source(db).

run(command(mkproject(Path, Options)), RetVal) :-
    % Create project directory with semantics
    run(command(mkdir(Path)), RetVal0),
    (RetVal0 = error(E) -> RetVal = RetVal0
    ;
        % Create .mypaos and initialize agent DB
        directory_file_path(Path, ".mypaos", MypaosDir),
        run(command(mkdir(MypaosDir)), RetVal1),
        (RetVal1 = error(E) -> RetVal = RetVal1
        ;
            % Initialize git if requested (before DB so we can track .mypaos)
            (option(git(false), Options) -> RetVal2 = ok("")
            ;
                run(command(git(init(Path))), RetVal2)
            ),
            (RetVal2 = error(E) -> RetVal = RetVal2
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
                (RetVal4 = error(E) -> RetVal = RetVal4
                ;
                    % Language setup stub for now
                    RetVal = ok("Project created successfully")
                )
            )
        )
    ).

