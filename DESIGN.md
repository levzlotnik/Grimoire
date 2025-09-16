# Grimoire System Design

## Project Overview

Grimoire is a knowledge-based operating system built on Entity-Component-System (ECS) architecture. It treats semantic knowledge as first-class entities stored in distributed Prolog files, integrating domain-specific tools through common patterns while maintaining their natural interfaces. The system currently runs with Prolog backend and Janus Python integration at version 0.1.0.

**Current Functional Status**: Production-ready core with all subsystems operational. The system passes comprehensive test suites covering ECS semantics, spell execution, domain integrations, session management, and multi-frontend interfaces. Git-backed session management provides atomic operations with rollback capabilities.

**Vision and Scope**: Grimoire aims to be a reproducible, declarative development environment where knowledge, tools, and processes are semantically interconnected. Unlike traditional operating systems that manage files and processes, Grimoire manages semantic relationships and knowledge transformations. The system enables developers to work with projects, templates, and tools through a unified semantic layer while maintaining full reproducibility through Nix integration.

---

## Core ECS Architecture

### Fundamental Predicates

The ECS system rests on three foundational predicates that form the semantic backbone:

**`entity/1`** - Declares atomic identifiers for things that exist in the system. Entities represent concepts, tools, files, projects, or any meaningful unit within Grimoire's knowledge space.

```prolog
entity(system).
entity(git).
entity(nix(flake(template(rust)))).
entity(database(session_commands)).
```

**`component/3`** - Expresses typed relationships between entities using the pattern `component(Entity, Type, Value)`. This predicate enables flexible, extensible data modeling where new relationship types can be introduced without schema changes.

```prolog
component(system, concept, spell).
component(git, subcommand, commit).
component(conjure, ctor, mkproject).
component(nix(flake(template)), instance, nix(flake(template(rust)))).
```

**`docstring/2`** - Documents entities and components with human-readable descriptions. Documentation is embedded directly in the semantic layer, ensuring it remains synchronized with implementation.

```prolog
docstring(spell, "The fundamental magic system of Grimoire").
docstring(conjure, "Conjuration spells that modify system state").
```

### Component Type Patterns

Grimoire uses a rich vocabulary of component types to model different kinds of relationships:

**`ctor`** - Constructor types for sum types and extensible enumerations. Used extensively in the spell system to define available operations.

```prolog
component(conjure, ctor, git(commit)).
component(perceive, ctor, git(status)).
component(edit_file, ctor, insert).
```

**`subcommand`** - Command hierarchies within domain entities. Provides structured command namespacing.

```prolog
component(git, subcommand, clone).
component(session, subcommand, start).
component(interface, subcommand, entities).
```

**`concept`** - High-level conceptual relationships expressing domain knowledge.

```prolog
component(system, concept, transaction).
component(nix, concept, nix(flake)).
component(project, concept, application).
```

**`self`** - Self-referential components indicating entity location and metadata.

```prolog
component(git, self, semantic(file("git.pl"))).
component(nix, self, semantic(folder("nix"))).
```

**`source`** - Source code and knowledge provenance tracking.

```prolog
component(system, source, source(semantic(file("grimoire.pl")))).
component(rust_template, source, source(semantic(file("semantics.pl")))).
```

**`subsystem`** - Major architectural components of Grimoire.

```prolog
component(system, subsystem, git).
component(system, subsystem, nix).
component(system, subsystem, session).
```

### Entity and Loading Patterns

**`:- self_entity(EntityName).`** - Declares the primary entity that a semantic file represents. This pattern enables automatic entity discovery and loading.

```prolog
:- self_entity(git).
:- self_entity(nix).
:- self_entity(rust_template).
```

**`:- load_entity(semantic(Source)).`** - Single-arity loading API that resolves and loads semantic knowledge from files or folders. The `@/` prefix resolves to `GRIMOIRE_ROOT`.

```prolog
:- load_entity(semantic(file("@/src/git.pl"))).
:- load_entity(semantic(folder("@/src/nix"))).
:- load_entity(semantic(folder("@/src/project"))).
```

### ECS Relationship Modeling

The ECS system enables complex relationship modeling through component composition:

```prolog
% Hierarchical relationships
entity(nix(target)).
component(nix(target), ctor, package).
component(nix(target), ctor, app).
entity(nix(target(package))).
entity(nix(target(app))).

% Cross-domain relationships  
component(context(build), requires, config(build)).
component(context(build), requires, deps(build)).
component(application, requires, context(C)) :- component(context, ctor, C).

% Dynamic discovery relationships
component(database(Db), table, table(TableName)) :-
    registered_db_table(database(Db), table(TableName)).
```

### Semantics and Testing System

Each domain follows the `.pl/.plt` pattern where `semantics.pl` contains the logic and `semantics.plt` contains comprehensive tests. This ensures semantic knowledge remains verifiable and correct.

```prolog
% In semantics.pl
component(git, subcommand, commit).
cast(conjure(git(commit(Message))), RetVal) :-
    phrase(git_args(commit(Message)), Args),
    cast(conjure(executable_program(git, Args)), RetVal).

% In semantics.plt  
test(git_commit_entity_exists) :-
    entity(git(commit)).
test(git_commit_conjure_ctor_exists) :-
    component(conjure, ctor, git(commit)).
```

---

## Spell System Implementation

The spell system provides a fantasy-themed interface that separates query operations (perception) from mutation operations (conjuration), ensuring clear distinction between safe and potentially dangerous operations.

### Conjure/Perceive Architecture

**Conjure Spells** - Mutable operations that modify system state. All conjure operations must be executed through the `cast/2` predicate for safety and transaction support.

```prolog
% Must use cast/2 wrapper
cast(conjure(git(commit("Initial commit"))), Result).
cast(conjure(mkproject("/projects", "myapp", [template(rust)])), Result).
cast(conjure(session(start("experimental"))), Result).
```

**Perceive Spells** - Query operations that read system state without modification. Called directly with variable unification for results.

```prolog
% Direct calls with unification
perceive(git(status(Branch, Status, Files))).
perceive(entities(EntityList)).
perceive(nix(flake(show(FlakeRef, Apps, Packages, DevShells)))).
```

### Cast/2 Safety Mechanism

The `cast/2` predicate provides essential safety features for mutable operations:

**Transaction Support** - Enables atomic operations and rollback capabilities through session integration.

**Error Handling** - Standardized error reporting with structured `ok(Result)` and `error(Details)` returns.

**Command Logging** - Automatic logging to session databases for audit trails and replay capabilities.

**Process Isolation** - Safe execution of external programs with proper output capture and error handling.

```prolog
cast(conjure(executable_program(Program, Args)), RetVal) :-
    setup_call_cleanup(
        process_create(path(Program), Args, [
            stdout(pipe(Out)), 
            stderr(pipe(Err)), 
            process(PID)
        ]),
        (read_string(Out, _, Stdout),
         read_string(Err, _, Stderr),
         process_wait(PID, exit(ExitCode))),
        (close(Out), close(Err))
    ),
    (ExitCode = 0 ->
        RetVal = ok(result(Stdout, Stderr))
    ;
        RetVal = error(process_error(Program, exit(ExitCode), Stdout, Stderr))
    ).
```

### Direct Predicate Calls for Queries

Perceive operations bypass the cast mechanism for efficiency, allowing direct predicate calls with Prolog's native unification:

```prolog
% Core perceive implementations
perceive(entities(Entities)) :-
    findall(Entity, entity(Entity), Entities).

perceive(git(status(Branch, WorkingStatus, Files))) :-
    cast(conjure(git(branch(['--show-current']))), BranchResult),
    (BranchResult = ok(result(BranchOutput, _)) ->
        string_concat(BranchStr, "\n", BranchOutput),
        atom_string(Branch, BranchStr)
    ;
        Branch = unknown
    ),
    cast(conjure(git(status(['--porcelain']))), StatusResult),
    (StatusResult = ok(result(StatusOutput, _)) ->
        (StatusOutput = "" ->
            WorkingStatus = clean, Files = []
        ;
            WorkingStatus = dirty,
            parse_git_status_output(StatusOutput, Files)
        )
    ;
        WorkingStatus = unknown, Files = []
    ).
```

### Ritual System for Atomic Operations

The `ritual/1` constructor enables atomic execution of multiple conjure spells, ensuring all-or-nothing semantics:

```prolog
cast(ritual([
    conjure(mkdir('/project/new-feature')),
    conjure(mkfile('/project/new-feature/main.rs')),
    conjure(git(add(['/project/new-feature']))),
    conjure(git(commit("Add new feature scaffolding")))
]), Result).
```

---

## Git Domain

The Git domain provides version control operations with session integration and structured data parsing. It models Git as both a tool for code versioning and a foundation for Grimoire's transactional session system.

### Version Control Operations

Git operations are split between conjure (state-changing) and perceive (query) operations:

**Conjure Operations:**
- `git(clone(Url, Path))` - Clone remote repositories
- `git(init(Path))` - Initialize new repositories  
- `git(add(Paths))` - Stage files for commit
- `git(commit(Message))` - Record changes
- `git(push(Remote, Branch))` - Push to remote
- `git(pull(Remote, Branch))` - Pull from remote
- `git(checkout(Branch))` - Switch branches
- `git(reset(Args))` - Reset repository state
- `git(merge(Args))` - Merge branches

**Perceive Operations:**
- `git(status(Branch, WorkingStatus, Files))` - Structured repository status
- `git(diff)` - Show changes
- `git(log)` - Commit history
- `git(branch)` - Branch listing
- `git(rev_parse(Args))` - Parse revision information

### Command Modeling and Execution

Git commands follow a consistent pattern using DCG (Definite Clause Grammar) for argument construction:

```prolog
% Command argument generation
git_args(clone(Url, Path)) --> ["clone", Url, Path].
git_args(commit(Message)) --> ["commit", "-m", Message].
git_args(add(all_tracked)) --> ["add", "-u"].
git_args(add(Paths)) --> ["add"|Paths].

% Execution through conjure system
cast(conjure(git(Term)), RetVal) :-
    functor(Term, SubCmdType, _),
    component(git, subcommand, SubCmdType),
    phrase(git_args(Term), Args),
    cast(conjure(executable_program(git, Args)), RetVal).
```

### Session Integration and Transaction Support

Git provides the transactional foundation for Grimoire's session system. Each session corresponds to a Git branch, enabling atomic commits and rollbacks:

```prolog
% Session-based git workflow
perceive(git(status(Branch, WorkingStatus, Files))) :-
    % Current branch detection for session correlation
    cast(conjure(git(branch(['--show-current']))), BranchResult),
    % Parse porcelain output for structured file status
    cast(conjure(git(status(['--porcelain']))), StatusResult),
    parse_git_status_output(StatusOutput, Files).

% Structured file status parsing
parse_status_line(Line, FileStatus) :-
    atom_codes(Line, [S1, S2, 32 | FileCodes]),
    atom_codes(File, FileCodes),
    status_code_to_term([S1, S2], File, FileStatus).

status_code_to_term([32, 77], File, modified(File)).     % " M"
status_code_to_term([77, 32], File, staged(File)).       % "M "  
status_code_to_term([63, 63], File, created(File)).      % "??"
status_code_to_term([65, 32], File, created(File)).      % "A "
status_code_to_term([68, 32], File, deleted(File)).      % "D "
```

---

## Nix Domain

The Nix domain manages package dependencies, build environments, and the template system through flake-based reproducible builds. It provides the foundation for Grimoire's reproducible development environments.

### Package Management and Flakes

Nix operations center around flakes for reproducible package management:

**Core Concepts:**
- `nix(store)` - Immutable package store management
- `nix(flake)` - Reproducible project specifications
- `nix(target)` - Buildable outputs (packages, apps, devShells)
- `nix(derivation)` - Build specifications

**Conjure Operations:**
- `nix(build(Installable))` - Build packages or flakes
- `nix(develop(Options))` - Enter development environments
- `nix(run(Installable, Args))` - Execute applications
- `nix(store(gc))` - Garbage collect unused packages
- `nix(flake(new(Template, Path)))` - Create projects from templates

**Perceive Operations:**
- `nix(flake(show(FlakeRef, Apps, Packages, DevShells)))` - Discover flake outputs
- `nix(search(Query, Results))` - Search package repositories
- `nix(log(Installable, BuildLog))` - View build logs
- `nix(why_depends(Pkg1, Pkg2, Trace))` - Dependency analysis

### Build System Integration

Nix commands integrate with Grimoire's execution system while preserving Nix's reproducibility guarantees:

```prolog
% Build operations with artifact tracking
cast(conjure(nix(build(Installable))), RetVal) :-
    Args = ["nix", "build", Installable],
    cast(conjure(shell(Args)), RetVal),
    (RetVal = ok(Output) ->
        assertz(entity(nix(derivation(Output)))),
        assertz(component(build_result, output_path, Output))
    ; true).

% Development environments with phase control
cast(conjure(nix(develop(Options))), RetVal) :-
    findall(OptionArgs, (
        member(Option, Options),
        (Option = shell_cmd(Args) -> OptionArgs = ["--command" | Args]
        ;Option = phase(Phase) -> OptionArgs = ["--phase", Phase]
        )
    ), AllOptionArgs),
    flatten([["nix", "develop"] | AllOptionArgs], Args),
    cast(conjure(shell(Args, interactive)), RetVal).
```

### Template Framework Architecture

The template system provides project scaffolding with automatic discovery and instantiation:

**Template Discovery:**
```prolog
% Automatic template discovery from flake outputs
nix_flake_templates(Pairs) :-
    nix_templates_path(TemplatePath),
    format(string(PathExpr), "path:~w", [TemplatePath]),
    process_create(path(nix), ["flake","show","--json",PathExpr], [stdout(pipe(Out))]),
    json_read_dict(Out, JsonDict),
    close(Out),
    dict_pairs(JsonDict.templates, _Tag, Pairs).

% Dynamic template entities
entity(nix(flake(template(Id)))) :-
    nix_flake_templates(Pairs),
    member(Id-_, Pairs).

% Component instance discovery
component(nix(flake(template)), instance, nix(flake(template(Id)))) :-
    nix_flake_templates(Pairs),
    member(Id-_V, Pairs).
```

**Available Templates:**
- `rust` - Rust project with Cargo integration
- `python` - Python project with Poetry/pip integration  
- `cpp` - C++ project with CMake build system
- `haskell` - Haskell project with Cabal/Stack support
- `lean4` - Lean 4 theorem prover projects
- `mkdocs` - Documentation sites with MkDocs
- `python-bridge-pattern` - Python-Prolog integration example
- `python-rest-api` - REST API service template
- `database` - Database project with SQLite integration

### Dynamic Target Discovery

Nix flakes expose their outputs dynamically, which Grimoire discovers and models as ECS entities:

```prolog
% Flake target discovery through nix flake show
get_nix_flake_targets(FlakeRef, Targets) :-
    process_create(path(nix), ["flake", "show", "--json", FlakeRef], [stdout(pipe(Out))]),
    json_read_dict(Out, JsonDict),
    close(Out),
    findall(Target, extract_flake_target(JsonDict, FlakeRef, Target), Targets).

% Extract different target types
extract_flake_target(JsonDict, FlakeRef, Target) :-
    get_dict(packages, JsonDict, Packages),
    dict_pairs(Packages, _, SystemPairs),
    member(System-SystemPackages, SystemPairs),
    dict_pairs(SystemPackages, _, PackagePairs),
    member(PackageName-_, PackagePairs),
    format(atom(AttrPath), 'packages.~w.~w', [System, PackageName]),
    Target = nix(target(package(FlakeRef, AttrPath))).

% Dynamic entity generation
entity(nix(target(package(FlakeRef, AttrPath)))) :-
    entity(nix(flake(_, FlakeRef))),
    get_nix_flake_targets(FlakeRef, Targets),
    member(nix(target(package(FlakeRef, AttrPath))), Targets).
```

---

## Session Domain

The Session domain provides workspace management with SQLite command logging and Git-backed transactionality. Sessions enable atomic operations across multiple tools while maintaining complete audit trails.

### Workspaces and Command Logging

Each session creates an isolated workspace with dedicated SQLite database for command logging:

**Session Structure:**
- `${GRIMOIRE_DATA}/sessions/{SessionId}/` - Workspace directory
- `commands.db` - SQLite database with command log
- `commands.schema.sql` - Database schema definition  
- `state.pl` - Persistent Prolog state file

**Command Logging Schema:**
```sql
CREATE TABLE commands (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp TEXT NOT NULL,
    command_type TEXT NOT NULL,
    command_term TEXT NOT NULL,
    result TEXT,
    source TEXT DEFAULT 'user'
);
```

### SQLite Integration and State Persistence

Session commands are automatically logged to SQLite for replay and audit capabilities:

```prolog
% Automatic command logging for session operations
log_command_to_session_db(SessionId, CommandType, CommandTerm, Result) :-
    session_commands_db_path(SessionId, DbPath),
    (exists_file(DbPath) ->
        catch(
            (get_time(TimeStamp),
             format_time(atom(FormattedTime), '%Y-%m-%d %H:%M:%S', TimeStamp),
             term_string(CommandTerm, CommandTermStr),
             escape_sql_string(CommandTermStr, EscapedCommandTerm),
             term_string(Result, ResultStr),
             escape_sql_string(ResultStr, EscapedResult),
             format(atom(InsertSQL),
                'INSERT INTO commands (timestamp, command_type, command_term, result) VALUES (\'~w\', \'~w\', \'~w\', \'~w\')',
                [FormattedTime, CommandType, EscapedCommandTerm, EscapedResult]),
             sqlite3_exec(DbPath, InsertSQL)),
            Error,
            format('Warning: Failed to log command: ~w~n', [Error])
        )
    ; true).
```

### Transaction Model and Git Integration

Sessions map to Git branches, providing atomic commit/rollback semantics:

**Session Operations:**
- `session(start)` - Create new session workspace and Git branch
- `session(switch(SessionId))` - Change active session context
- `session(commit(Message))` - Commit accumulated changes to Git
- `session(rollback)` - Discard changes and reset to branch start
- `session(delete(SessionId))` - Remove session and workspace completely

**Session State Management:**
```prolog
% Dynamic session tracking
:- dynamic current_session_id/1.

get_current_session_id(SessionId) :-
    (current_session_id(SessionId) -> true ; SessionId = main).

% Session workspace initialization
initialize_session_workspace(SessionId) :-
    session_workspace_path(SessionId, WorkspacePath),
    (exists_directory(WorkspacePath) -> true ; make_directory_path(WorkspacePath)).

initialize_session_database(SessionId) :-
    session_commands_db_path(SessionId, DbPath),
    session_workspace_path(SessionId, WorkspacePath),
    format(atom(SchemaFile), '~w/commands.schema.sql', [WorkspacePath]),
    format(atom(SessionDbId), 'session_~w', [SessionId]),
    cast(conjure(db(create(SessionDbId, DbPath, schema(file(SchemaFile))))), _).
```

### Think Command for AI Integration

The `think` command provides thought logging for AI agent interactions:

```prolog
cast(conjure(think(ThoughtString)), RetVal) :-
    string(ThoughtString),
    log_command_to_session(think, ThoughtString, thought_recorded),
    RetVal = ok(thought_recorded(ThoughtString)).
```

---

## Database Domain

The Database domain provides SQLite integration with automatic schema discovery and ECS component mapping. It enables databases to be treated as first-class entities within Grimoire's semantic system.

### SQLite Integration and Schema Tracking

Databases are registered and discovered dynamically through the registration system:

```prolog
% Dynamic database registration
:- dynamic registered_db/3.

% Registration pattern: registered_db(database(Id), data(file(DbPath)), schema(file(SchemaPath)))
registered_db(database(session_commands), 
              data(file('/home/user/.grimoire/sessions/main/commands.db')),
              schema(file('/home/user/.grimoire/sessions/main/commands.schema.sql'))).

% Automatic entity generation
entity(database(Db)) :-
    registered_db(database(Db), data(file(_)), schema(file(_))).
```

### ECS Component Mapping for Database Structure

Database tables and columns become queryable ECS components through reverse proxy patterns:

```prolog
% Table discovery through SQLite introspection
component(database(Db), table, table(TableName)) :-
    registered_db_table(database(Db), table(TableName)).

registered_db_table(database(Db), table(TableName)) :-
    registered_db(database(Db), data(file(DbFile)), schema(file(_))),
    exists_file(DbFile),
    get_database_tables(DbFile, TableStrings),
    member(TableString, TableStrings),
    atom_string(TableName, TableString).

% Column discovery with type information
component(database(Db), column, column(TableName, ColumnName, ColumnInfo)) :-
    registered_db_column(database(Db), column(TableName, ColumnName, ColumnInfo)).

registered_db_column(database(Db), column(TableName, ColumnName, ColumnInfo)) :-
    registered_db(database(Db), data(file(DbFile)), schema(file(_))),
    get_table_columns(DbFile, TableName, Columns),
    member(ColumnInfo, Columns),
    ColumnInfo = column(ColumnNameString, _, _, _, _),
    atom_string(ColumnName, ColumnNameString).
```

### Database Creation and Schema Management

Database creation integrates with the conjure system for safe database operations:

```prolog
% Database creation from schema file
cast(conjure(db(create(DbId, DbPath, schema(file(SchemaFile))))), RetVal) :-
    sqlite3_init_db(DbPath, SchemaFile),
    assertz(registered_db(database(DbId), data(file(DbPath)), schema(file(SchemaFile)))),
    RetVal = ok(database_created(DbId, DbPath)).

% Database creation from SQL string
cast(conjure(db(create(DbId, DbPath, schema(sql(SchemaSQL))))), RetVal) :-
    file_name_extension(DbBaseName, db, DbPath),
    format(atom(SchemaFile), '~w.schema.sql', [DbBaseName]),
    open(SchemaFile, write, Stream),
    write(Stream, SchemaSQL),
    close(Stream),
    sqlite3_init_db(DbPath, SchemaFile),
    assertz(registered_db(database(DbId), data(file(DbPath)), schema(file(SchemaFile)))),
    RetVal = ok(database_created(DbId, DbPath)).
```

### Automatic Structure Exposure Through Templates

The database template demonstrates automatic discovery patterns:

```prolog
% Example database with automatic artifact discovery
cast(conjure(example_db(inspect)), RetVal) :-
    catch(
        (discover_database_artifacts(example_db),
         findall(Table, component(example_db, table, Table), Tables),
         findall(table_info(Table, Columns), (
             member(Table, Tables),
             TableEntity =.. [example_db, table(Table)],
             component(TableEntity, columns, Columns)
         ), TableInfos),
         RetVal = ok(database_structure(TableInfos))),
        Error,
        RetVal = error(inspect_failed(Error))
    ).
```

---

## Project Domain

The Project domain handles artifact discovery, template instantiation, and build context management. It provides the organizational framework for software development within Grimoire.

### Artifact Discovery and Build Contexts

Projects organize software development through three primary contexts:

**Context Types:**
- `context(build)` - Compilation and build-time environment
- `context(runtime)` - Execution environment and dependencies
- `context(test)` - Testing framework and test-specific dependencies

**Context Requirements:**
```prolog
component(context(build), requires, config(build)).
component(context(build), requires, deps(build)).
component(context(build), requires, source(code)).

component(context(runtime), requires, config(runtime)).
component(context(runtime), requires, deps(runtime)).
component(context(runtime), requires, source(data)).

component(context(test), requires, config(test)).
component(context(test), requires, deps(test)).
```

### Template Instantiation Process

The `mkproject` conjure operation provides comprehensive project creation with template support:

```prolog
cast(conjure(mkproject(FolderPath, ProjectName, Options)), RetVal) :-
    catch(
        (directory_file_path(FolderPath, ProjectName, ProjectPath),
         (exists_directory(FolderPath) -> true ; make_directory_path(FolderPath)),
         (exists_directory(ProjectPath) ->
             RetVal = error(project_already_exists(ProjectPath))
         ;
             make_directory(ProjectPath),
             % Apply template if specified
             (member(template(Template), Options) ->
                 cast(conjure(nix(flake(new(Template, ProjectPath)))), TemplateResult),
                 (TemplateResult = ok(_) ->
                     rename_project_entities(ProjectPath, Template, ProjectName, RenameResult)
                 ;
                     RenameResult = TemplateResult
                 )
             ;
                 create_basic_semantics_file(ProjectPath, ProjectName),
                 RenameResult = ok
             ),
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
         )),
        Error,
        RetVal = error(conjure_failed(Error))
    ).
```

### Build Context Discovery

Projects automatically discover their build artifacts and dependencies:

```prolog
discover_project_artifacts(Entity, Options) :-
    discover_core_artifacts(Entity),
    discover_nix_targets(Entity),
    (member(fs_patterns(IncludePatterns, ExcludePatterns), Options) ->
        discover_custom_artifacts(Entity, [IncludePatterns, ExcludePatterns])
    ;
        true
    ).

discover_core_artifacts(Entity) :-
    working_directory(CurrentDir, CurrentDir),
    discover_flake_artifact(Entity, CurrentDir),
    discover_git_artifact(Entity, CurrentDir),
    discover_readme_artifact(Entity, CurrentDir),
    discover_sources_artifact(Entity, CurrentDir).

discover_nix_targets(Entity) :-
    working_directory(CurrentDir, CurrentDir),
    process_create(path(nix), ['flake', 'show', '--json'], [
        stdout(pipe(Out)),
        cwd(CurrentDir)
    ]),
    read_string(Out, _, JsonString),
    close(Out),
    atom_string(JsonAtom, JsonString),
    atom_json_term(JsonAtom, JsonDict, []),
    extract_nix_apps(Entity, JsonDict, CurrentDir).
```

### Multi-Template Project Composition

Projects can compose multiple templates for full-stack development:

```prolog
% Example: Backend + Frontend composition
cast(conjure(mkproject("/projects", "webapp", [
    template(python-rest-api),  % Backend API
    template(rust),             % Frontend WASM
    git(true),
    composition(fullstack)
])), Result).
```

---

## Golems Domain

The Golems domain implements an AI agent framework with Python-Prolog bridge integration. It provides autonomous AI agents built on ECS architecture with structured input/output validation and multi-provider LLM support.

### AI Agent Framework Architecture

Golems are defined as ECS entities with role, configuration, and capability components:

**Available Golems:**
- `golem(code_assistant)` - Code generation and refactoring assistance
- `golem(project_manager)` - Project organization and planning
- `golem(test_runner)` - Automated testing and validation
- `golem(documentation)` - Documentation generation and maintenance

**Golem Components:**
```prolog
component(golem(code_assistant), role, "Assists with code generation, refactoring, and debugging").
component(golem(code_assistant), llm_config, _{provider: anthropic, model: "claude-3-sonnet"}).
component(golem(code_assistant), input, code_context).
component(golem(code_assistant), input, user_request).
component(golem(code_assistant), output, code_changes).
component(golem(code_assistant), output, explanation).
```

### Python Bridge Integration and Provider Abstraction

The Python bridge enables seamless integration with AI providers while maintaining Prolog's logical foundation:

```prolog
% Import prolog-safe predicates from python bridge
:- use_module('python_bridge.pl', [
    get_golem_tools/2,
    execute_golem_task/7,
    get_golem_python_instance/2,
    log_thought_to_session/2
]).

% Execute golem task with full ECS configuration
cast(conjure(golem_task(golem(Id), Inputs)), RetVal) :-
    component(golem(Id), llm_config, LLMConfigDict),
    component(golem(Id), role, Role),
    findall(I, component(golem(Id), input, I), InputSchema),
    findall(O, component(golem(Id), output, O), OutputSchema),
    execute_golem_task(Id, LLMConfigDict, Role, InputSchema, OutputSchema, Inputs, RetVal).
```

### Provider Factory and Multi-LLM Support

The system supports multiple LLM providers through a unified interface:

**Supported Providers:**
- `anthropic` - Claude models (Claude-3 Sonnet, Haiku, Opus)
- `openai` - GPT models (GPT-4, GPT-3.5-turbo)
- `ollama` - Local models (Llama 2, Code Llama, Mistral)
- `groq` - High-speed inference (Llama 2, Mixtral)

**Configuration Examples:**
```prolog
component(golem(fast_coder), llm_config, _{
    provider: groq,
    model: "llama2-70b-4096",
    temperature: 0.1,
    max_tokens: 2048
}).

component(golem(thoughtful_architect), llm_config, _{
    provider: anthropic,
    model: "claude-3-opus-20240229",
    temperature: 0.3,
    max_tokens: 4096
}).
```

### Structured Input/Output Validation

Golems enforce structured schemas for reliable AI interactions:

```prolog
% Input validation example
component(golem(test_runner), input, test_files).
component(golem(test_runner), input, test_framework).
component(golem(test_runner), input, coverage_threshold).

% Output validation example  
component(golem(test_runner), output, test_results).
component(golem(test_runner), output, coverage_report).
component(golem(test_runner), output, recommendations).
```

### Thought Logging and Session Integration

Golems integrate with the session system for complete audit trails:

```prolog
% Thought logging for debugging and audit
cast(conjure(thought(Content)), RetVal) :-
    log_thought_to_session(Content, RetVal).

% Dynamic docstring generation with live configuration
docstring(golem(Id), DocString) :-
    component(golem(Id), role, Role),
    component(golem(Id), llm_config, Config),
    findall(input(I), component(golem(Id), input, I), Inputs),
    findall(output(O), component(golem(Id), output, O), Outputs),
    (catch(get_golem_tools(golem(Id), Tools), _, Tools = []) -> true; Tools = []),
    format_golem_docstring(Role, Config, Tools, Inputs, Outputs, DocString).
```

---

## Interface Domain

The Interface domain provides CLI, API, and MCP multi-frontend access to Grimoire functionality. It maintains consistency across different interaction modalities while adapting to each medium's conventions.

### CLI/API/MCP Multi-Frontend Architecture

The interface layer provides unified access through three primary frontends:

**Command Line Interface (CLI)** - Direct command execution through `./grimoire` script
**REST API** - HTTP-based access for web integration  
**Model Context Protocol (MCP)** - Integration with AI development tools

**Interface Commands:**
- `interface(entities)` - List all entities in the system
- `interface(compt(Entity))` - Show component types for entity
- `interface(comp(Entity, Type))` - List components of specific type
- `interface(doc(Entity))` - Show entity documentation
- `interface(status)` - Display session and system status
- `interface(conjure(Spell))` - Execute conjuration spells
- `interface(perceive(Query))` - Execute perception queries

### Command Routing and Context Management

The interface layer provides automatic context detection and entity resolution:

```prolog
% Auto-detect current working context
current_entity(Entity) :-
    (exists_file('./semantics.pl') ->
        working_directory(Cwd, Cwd),
        file_base_name(Cwd, DirName),
        atom_string(Entity, DirName),
        ensure_local_project_loaded(Entity)
    ;
        Entity = system
    ).

% Entity path resolution with shortcuts
resolve_entity_path(PathStr, Entity) :-
    (PathStr = "/" ->
        Entity = system
    ;
        catch(
            (load_entity(semantic(folder(PathStr))),
             file_base_name(PathStr, DirName),
             atom_string(Entity, DirName)),
            _,
            atom_string(Entity, PathStr)
        )
    ).
```

### Session State Integration

Interface operations integrate with session state for persistent entity loading:

```prolog
% Load entity into current session state
load_entity_in_session(Entity) :-
    get_current_session_id(SessionId),
    entity_to_semantic_spec(Entity, EntitySpec),
    (validate_entity_exists(Entity, EntitySpec) ->
        (SessionId \= main ->
            add_entity_load_to_session(SessionId, EntitySpec)
        ;
            true
        )
    ;
        format(atom(ErrorMsg), 'Entity ~w not found', [Entity]),
        throw(entity_load_failed(Entity, EntitySpec, ErrorMsg))
    ).

% Ensure session state is loaded before operations
ensure_session_state_loaded :-
    get_current_session_id(SessionId),
    (SessionId \= main ->
        load_session_state_file(SessionId)
    ;
        true
    ).
```

### Python Interface Support for MCP Integration

The interface provides Python-friendly data conversion for MCP protocol compatibility:

```prolog
% Python-specific cast that converts Prolog terms to Python dictionaries
python_cast(conjure(ConjureStruct), PyResult) :-
    (atom(ConjureStruct) ->
        atom_to_term(ConjureStruct, Term, [])
    ;
        Term = ConjureStruct
    ),
    cast(conjure(Term), Result),
    term_struct_to_python_dict(Result, PyResult).

% Recursive term to dictionary conversion
term_struct_to_python_dict(Term, Dict) :-
    (atomic(Term) ->
        (atom(Term) ->
            Dict = _{type: "atom", value: Term}
        ; string(Term) ->
            Dict = _{type: "string", value: Term}
        ; number(Term) ->
            Dict = _{type: "int", value: Term}
        )
    ; is_list(Term) ->
        maplist(term_struct_to_python_dict, Term, Elements),
        Dict = _{type: "list", elements: Elements}
    ; compound(Term) ->
        compound_name_arity(Term, Functor, Arity),
        Term =.. [Functor|Args],
        maplist(term_struct_to_python_dict, Args, ConvertedArgs),
        Dict = _{type: "term_struct", functor: Functor, arity: Arity, args: ConvertedArgs}
    ).
```

---

## Template System

The template system provides scaffolding for nine different project types, each with Nix-based build systems and flake app integration. Templates enable rapid project creation with reproducible development environments.

### Complete Template Catalog

**Rust Template (`rust`):**
- Build system: Nix + Cargo integration
- Commands: `build`, `test`, `run`, `check`, `clippy`, `fmt`
- Structure: `src/main.rs`, `Cargo.toml`, `flake.nix`
- Usage: High-performance systems programming

**Python Template (`python`):**
- Build system: Nix + pip/Poetry integration  
- Commands: `run`, `build`, `develop`
- Structure: `main.py`, `pyproject.toml`, `flake.nix`
- Usage: General-purpose scripting and applications

**C++ Template (`cpp`):**
- Build system: Nix + CMake integration
- Commands: `run`, `build`, `develop`
- Structure: `src/main.cpp`, `CMakeLists.txt`, `include/`
- Usage: High-performance native applications

**Haskell Template (`haskell`):**
- Build system: Nix + Cabal/Stack integration
- Commands: `build`, `test`, `run`, `ghci`, `hlint`
- Structure: `*.hs`, `*.cabal`, `stack.yaml`
- Usage: Functional programming and theorem proving

**Lean 4 Template (`lean4`):**
- Build system: Nix + Lake integration
- Commands: `build`, `test`, `prove`
- Structure: `*.lean`, `lakefile.lean`
- Usage: Formal verification and mathematical proofs

**MkDocs Template (`mkdocs`):**
- Build system: Nix + MkDocs integration
- Commands: `serve`, `build`, `deploy`
- Structure: `docs/`, `mkdocs.yml`
- Usage: Documentation websites and technical writing

**Python Bridge Pattern Template (`python-bridge-pattern`):**
- Build system: Nix + Python-Prolog integration
- Commands: Domain-specific operations through bridge
- Structure: `semantics.pl`, `python_bridge.pl`
- Usage: Hybrid logic/computational systems

**Python REST API Template (`python-rest-api`):**
- Build system: Nix + FastAPI/Flask integration
- Commands: `serve`, `test`, `deploy`
- Structure: API endpoints, models, database integration
- Usage: Web service backends and microservices

**Database Template (`database`):**
- Build system: Nix + SQLite integration
- Commands: `setup`, `populate`, `inspect`
- Structure: `schema.sql`, database discovery patterns
- Usage: Data-centric applications with ECS integration

### Template Structure and Instantiation Process

All templates follow consistent patterns while providing language-specific optimizations:

```prolog
% Template instantiation through mkproject
cast(conjure(mkproject(FolderPath, ProjectName, Options)), RetVal) :-
    (member(template(Template), Options) ->
        cast(conjure(nix(flake(new(Template, ProjectPath)))), TemplateResult),
        (TemplateResult = ok(_) ->
            rename_project_entities(ProjectPath, Template, ProjectName, RenameResult)
        ;
            RenameResult = TemplateResult
        )
    ;
        create_basic_semantics_file(ProjectPath, ProjectName),
        RenameResult = ok
    ).

% Entity renaming for project customization
rename_project_entities(ProjectPath, TemplateId, ProjectName, Result) :-
    directory_file_path(ProjectPath, 'semantics.pl', SemanticsFile),
    (exists_file(SemanticsFile) ->
        discover_template_entity_name(SemanticsFile, TemplateEntityName)
    ;
        format(atom(TemplateEntityName), '~w_template', [TemplateId])
    ),
    findall(File, (
        member(FileName, ['semantics.pl', 'semantics.plt']),
        directory_file_path(ProjectPath, FileName, File),
        exists_file(File)
    ), SemanticsFiles),
    maplist(rename_entities_in_file(TemplateEntityName, ProjectName), SemanticsFiles, Results),
    (member(error(_), Results) -> Result = error(Results) ; Result = ok).
```

### Nix Flake Apps Integration

Templates integrate with Nix flake apps for reproducible command execution:

```prolog
% Example: Rust template command implementations
cast(conjure(rust_template(build)), RetVal) :-
    cast(conjure(nix(build(['.']))), RetVal).

cast(conjure(rust_template(test)), RetVal) :-
    cast(conjure(nix(flake(['check']))), RetVal).

cast(conjure(rust_template(run)), RetVal) :-
    cast(conjure(nix(run(['.#run']))), RetVal).

% Python template with development environment
cast(conjure(python_template(develop)), RetVal) :-
    cast(conjure(nix(develop(['.']))), RetVal).
```

### Multi-Template Composition for Full-Stack Projects

Templates can be composed to create complex project structures:

**Backend + Frontend Composition:**
```prolog
% Create full-stack web application
cast(conjure(mkproject("/projects", "webapp", [
    template(python-rest-api),    % Backend API
    template(rust),               % Frontend WASM
    template(database),           % Data layer
    composition(webapp)
])), Result).
```

**Documentation + Code Composition:**
```prolog
% Create documented library project
cast(conjure(mkproject("/projects", "mathlib", [
    template(lean4),              % Formal proofs
    template(mkdocs),             % Documentation
    template(haskell),            % Reference implementation
    composition(formal_library)
])), Result).
```

### Template Discovery and Validation

Templates are automatically discovered from the Nix flake outputs:

```prolog
% Template availability check
discover_available_templates(Templates) :-
    findall(TemplateId,
            component(nix(flake(template)), instance, nix(flake(template(TemplateId)))),
            Templates).

% Template validation before instantiation
validate_template_exists(TemplateId) :-
    component(nix(flake(template)), instance, nix(flake(template(TemplateId)))).
```

---

## Implementation Details

### File Structure Conventions

Grimoire follows consistent file organization patterns across all domains:

**Core Structure:**
```
/home/levz/Projects/Grimoire/
├── src/
│   ├── grimoire.pl           # Core system bootstrap
│   ├── git.pl               # Git domain semantics
│   ├── session.pl           # Session management
│   ├── fs.pl                # Filesystem operations
│   ├── nix/
│   │   ├── semantics.pl     # Nix domain core
│   │   └── templates/       # Project templates
│   │       ├── rust/
│   │       ├── python/
│   │       └── ...
│   ├── project/
│   │   └── semantics.pl     # Project management
│   ├── db/
│   │   ├── semantics.pl     # Database integration
│   │   └── sqlite3.pl       # SQLite interface
│   ├── golems/
│   │   ├── semantics.pl     # AI agent framework
│   │   └── python_bridge.pl # Python integration
│   ├── interface/
│   │   └── semantics.pl     # Multi-frontend interface
│   └── tests/
│       ├── *.plt            # PLUnit test suites
│       └── run_tests.pl     # Test runner
├── flake.nix                # Nix package specification
├── grimoire                 # CLI entry point script
└── DESIGN.md               # This document
```

**Domain Pattern:**
Each domain follows the `semantics.pl/semantics.plt` pattern:
- `semantics.pl` - Core logic and entity definitions
- `semantics.plt` - Comprehensive test coverage
- `:- self_entity(DomainName).` - Entity self-declaration
- Component definitions with docstrings

### Session Management Internals

Session management integrates Git branches with workspace isolation:

**Workspace Isolation:**
```prolog
% Session paths relative to GRIMOIRE_DATA
grimoire_data_directory(DataDir) :-
    (getenv('GRIMOIRE_DATA', CustomDataDir) ->
        DataDir = CustomDataDir
    ;
        getenv('HOME', HomeDir),
        format(atom(DataDir), '~w/.grimoire', [HomeDir])
    ).

session_workspace_path(SessionId, WorkspacePath) :-
    grimoire_data_directory(DataDir),
    format(atom(WorkspacePath), '~w/sessions/~w', [DataDir, SessionId]).
```

**Command Logging Schema:**
```sql
CREATE TABLE commands (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp TEXT NOT NULL,
    command_type TEXT NOT NULL,
    command_term TEXT NOT NULL,
    result TEXT,
    source TEXT DEFAULT 'user'
);
CREATE INDEX idx_timestamp ON commands(timestamp);
CREATE INDEX idx_command_type ON commands(command_type);
CREATE INDEX idx_source ON commands(source);
```

**State File Management:**
```prolog
initialize_session_state_file(SessionId) :-
    session_state_file_path(SessionId, StatePath),
    (exists_file(StatePath) ->
        true
    ;
        open(StatePath, write, Stream),
        format(Stream, '%% Session state file for session ~w~n', [SessionId]),
        format(Stream, '%% Entity loads and persistent state~n~n', []),
        close(Stream)
    ).
```

### Database Integration Patterns

Database entities use reverse proxy patterns for dynamic component generation:

**Registration Pattern:**
```prolog
% Dynamic database registration
:- dynamic registered_db/3.
registered_db(database(Db), data(file(DbPath)), schema(file(SchemaPath))).

% Automatic entity generation
entity(database(Db)) :-
    registered_db(database(Db), data(file(_)), schema(file(_))).

% Component proxy patterns
component(database(Db), table, table(TableName)) :-
    registered_db_table(database(Db), table(TableName)).

component(database(Db), column, column(TableName, ColumnName, ColumnInfo)) :-
    registered_db_column(database(Db), column(TableName, ColumnName, ColumnInfo)).
```

**SQLite Introspection:**
```prolog
% Table discovery through PRAGMA statements
get_database_tables(DbFile, TableNames) :-
    sqlite3_query(DbFile, "SELECT name FROM sqlite_master WHERE type='table'", Results),
    maplist(extract_table_name, Results, TableNames).

% Column information with types
get_table_columns(DbFile, TableName, Columns) :-
    format(atom(Query), 'PRAGMA table_info(~w)', [TableName]),
    sqlite3_query(DbFile, Query, Results),
    maplist(parse_column_info, Results, Columns).
```

### Multi-Frontend Interface Architecture

The interface layer abstracts Grimoire operations for different frontends:

**Frontend Abstraction:**
```prolog
% CLI interface through ./grimoire script
% REST API through HTTP endpoints  
% MCP through Model Context Protocol

% Unified interface command pattern
cast(conjure(interface(Command)), RetVal) :-
    ensure_session_state_loaded,
    interface_command_dispatch(Command, RetVal).

% Context-aware entity resolution
current_entity(Entity) :-
    (exists_file('./semantics.pl') ->
        working_directory(Cwd, Cwd),
        file_base_name(Cwd, DirName),
        atom_string(Entity, DirName),
        ensure_local_project_loaded(Entity)
    ;
        Entity = system
    ).
```

**Python Compatibility Layer:**
```prolog
% Convert Prolog terms to Python-friendly structures
term_struct_to_python_dict(Term, Dict) :-
    (compound(Term) ->
        compound_name_arity(Term, Functor, Arity),
        Term =.. [Functor|Args],
        maplist(term_struct_to_python_dict, Args, ConvertedArgs),
        Dict = _{type: "term_struct", functor: Functor, arity: Arity, args: ConvertedArgs}
    ; is_list(Term) ->
        maplist(term_struct_to_python_dict, Term, Elements),
        Dict = _{type: "list", elements: Elements}
    ; atomic(Term) ->
        Dict = _{type: "atom", value: Term}
    ).
```

### Environment Variable Integration

Grimoire uses environment variables for flexible configuration:

**Core Environment Variables:**
- `GRIMOIRE_ROOT` - Project directory (where `./grimoire` script runs)
- `GRIMOIRE_DATA` - Data storage directory (defaults to `$HOME/.grimoire`)

**Path Resolution:**
```prolog
grimoire_root_directory(RootDir) :-
    (getenv('GRIMOIRE_ROOT', CustomRoot) ->
        RootDir = CustomRoot
    ;
        working_directory(CurrentDir, CurrentDir),
        RootDir = CurrentDir
    ).

% Semantic path resolution with @/ prefix
grimoire_resolve_path('@/src/git.pl', '/home/levz/Projects/Grimoire/src/git.pl').
```

This comprehensive design document captures Grimoire's current architecture and implementation status. The system demonstrates a novel approach to semantic computing where knowledge, tools, and processes are unified through Entity-Component-System patterns while maintaining reproducibility through Nix integration and transactionality through Git-backed sessions.