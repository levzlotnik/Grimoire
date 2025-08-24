# Grimoire

A Knowledge-Based Operating System built on Entity-Component-System (ECS) architecture using Prolog for semantic knowledge management and Nix for system configuration.

Note: The repository is still named "MyPAOS" while the project has been renamed to "Grimoire".

## Core Philosophy

Grimoire treats the operating system as a knowledge management system where:
- Semantic knowledge lives in Prolog files (`semantics.pl`)
- System state is managed through Nix
- Each domain uses its natural tools while integrating through common patterns
- Knowledge is local to where it belongs (git-like distributed patterns)
- No runtime asserts – all knowledge lives in files

## Architecture

### Entity-Component-System (ECS)
- `entity/1` to declare things that exist
- `component/3` to express relationships between things
- Extensible sum types via `component(Type, ctor, ...)`
- Relation meta-patterns via `component(Type, relation_pattern, Pattern)`
- Clean composition across domains

### Explicit Entities + Simple Loading
- All semantic files declare their entities explicitly
- Single-arity loading API (prefer folders): `load_entity(semantic(folder(...)))`
  Folder mode expects a `semantics.pl` entrypoint inside the folder.
  `semantic(file(...))` is supported when a domain is a single file
- External projects can be loaded on-demand via `passive_load/2`

Example:
```prolog
% Core system loading (prefer folders)
:- load_entity(semantic(folder("src/nix"))).
:- load_entity(semantic(folder("src/project"))).
% Load other core domains that are single files
:- load_entity(semantic(file("src/git.pl"))).
:- load_entity(semantic(file("src/fs.pl"))).

% Template loading
?- load_entity(semantic(folder("templates/rust"))).
```

### Domain Integration
- **Nix**: system configuration, packages, flakes
- **Git**: version control operations
- **Prolog**: semantic knowledge and query interface
- **SQLite**: Database entity system with automatic ECS discovery, schema tracking, and command logging
- **Session**: File-based workspaces with SQLite command logging and ECS database discovery

## Nix-Centric Build Operations

All language templates use Nix flake apps instead of language-native commands, ensuring reproducibility and consistency.

- Canonical pattern: `nix run .#<command>` (with discovery of available apps)
- Benefits:
  - Reproducible builds and runs across environments
  - Single command surface per language
  - Zero toolchain drift – Nix provides toolchains

Templates (Phase 5.4 complete):
- Rust: `nix run .#build|test|run|check|clippy|fmt`
- Python: `nix run .#run|build` and `nix develop`
- C++: `nix run .#run|build` and `nix develop`
- Haskell: `nix run .#build|test|run|ghci|hlint`
- Lean4: `nix run .#build|check|clean|doc`
- MkDocs: `nix run .#build|serve|deploy|new`

## Passive Loading and Discovery

Passive loading lets you mount external project semantics without side effects, and run discovery on demand.

- Passive load:
  ```prolog
  % Load a project semantics on-demand (no side effects)
  ?- passive_load(my_project, semantic(folder("project"))).
  ```
- Run discovery:
  ```prolog
  % Discover core artifacts and Nix targets for the project
  ?- discover_project_artifacts(my_project).
  ```

Discovery flow:
- Core artifacts: `discover_core_artifacts/1` finds
  - flake.nix → `component(Entity, core_artifact, nix(flake(Path)))`
  - Git repo → `component(Entity, core_artifact, git(repository(Dir)))`
  - README → `component(Entity, core_artifact, readme(Path))`
  - Sources from git-tracked files → `component(Entity, source_file, file(RelPath))`
- Nix targets: `discover_nix_targets/1` runs `nix flake show --json` and emits
  - `component(Entity, nix_target, app(FlakeRef, AttrPath, AppName))`
- Custom filesystem patterns: `discover_custom_artifacts/2` with include/exclude patterns via the fs domain

Persistence model:
- Discovery produces derived, session-scoped components for querying and commands.
- Persistent knowledge remains in `semantics.pl` files; database persistence is deferred.

## Database and Session System

### Database System (.db extension with schema tracking)

The database system provides SQLite3 CLI integration with automatic ECS discovery:

```prolog
% Create database from SQL string (creates schema file)
?- cast(conjure(db(create(mydb, 'test.db', schema(sql("CREATE TABLE users (id INTEGER PRIMARY KEY);"))))), Result).
% Result = ok(database_created(mydb, 'test.db'))

% Create database from schema file
?- cast(conjure(db(create(mydb2, 'test2.db', schema(file('schema.sql'))))), Result).

% Database registration with schema tracking
?- component(registered_db, Component, Value).
% Component = database(mydb), Value = data(file('test.db'))
% Component = database(mydb), Value = schema(file('test.schema.sql'))
```

### Session System (File-based workspaces)

Sessions create isolated workspaces with SQLite command logging and ECS discovery:

```prolog
% Start a session (creates workspace directory with commands.db and state.pl)
?- cast(conjure(session(start('my-work'))), Result).
% Result = ok(session_started('my-work'))
% Creates: ${GRIMOIRE_DATA}/sessions/my-work/commands.db
% Creates: ${GRIMOIRE_DATA}/sessions/my-work/state.pl
% Creates: ${GRIMOIRE_DATA}/sessions/my-work/commands.schema.sql

% Record thoughts and commands (logged to session database)
?- cast(conjure(think("Working on database integration")), Result).
% Result = ok(thought_recorded("Working on database integration"))

% View session command history
?- perceive(session(history(Commands))).
% Shows accumulated commands from session database

% Session database becomes ECS entity with auto-discovered structure
?- component(session_my_work_db, table, Table).
% Table = commands

?- component(session_my_work_db(table(commands)), columns, Columns).
% Columns = [column(id,'INTEGER',false,null,true), column(timestamp,'TEXT',true,null,false), ...]
```

## Current Status

- All 116/128 tests passing (90.6% pass rate) - Core + Template + Spell + Session + Database systems
- **Database System Complete** - SQLite3 CLI integration with reverse proxy predicating and schema tracking
  - 3-parameter database registration: `registered_db(database(Id), data(file(DbPath)), schema(file(SchemaPath)))`
  - Command-based database creation: `command(db(create(DbId, DbPath, schema(file|sql))))`
  - Automatic schema file generation and ECS component discovery
- **Session System Complete** - File-based workspaces with SQLite command logging (.db extension)
  - Session workspaces under `${GRIMOIRE_DATA}/sessions/` with commands.db and state.pl files
  - ECS database integration with automatic table/column component discovery
  - Session state persistence for entity loads across operations
- **Spell System Complete** - Fantasy-themed command/query separation with `conjure`/`perceive`
- **Interface Layer Complete** - Multi-frontend interface system (`./grimoire` CLI, API, MCP ready)
- Simplified entity loading system implemented (`load_entity/1`)
- Core subsystems (git, nix, fs, project, session, db) load on boot
- External projects use `passive_load/2` for on-demand loading
- All language templates now use Nix-provided canonical commands

### Completed Phases
- Phase 1: Foundation cleanup – ECS architecture and semantic mounting
- Phase 2: Nix CLI mastery – flake-first operations with dynamic discovery
- Phase 3: Testing infrastructure – PLUnit suite and cross-domain coverage
- Phase 4: Simplified entity loading – explicit entities + `load_entity/1`
- Phase 5.1–5.4: Nix-centric template revolution – 6 templates completed
- **Phase 6: Session System** – File-based session management with SQLite logging (.db extension) and database ECS discovery
- **Database System**: SQLite3 CLI integration with reverse proxy predicating, schema tracking, and automatic ECS discovery
- **Interface Layer**: Multi-frontend ECS-native interface (`./grimoire` CLI, API/MCP ready)
- **Spell System**: Fantasy-themed conjure/perceive command separation with clean CLI

### Implemented Features
- Core ECS system with semantic mounting ✅
- **Spell system**: Fantasy-themed conjure/perceive command separation ✅
- **Database system**: SQLite3 CLI integration with reverse proxy predicating and schema tracking ✅
  - 3-parameter database registration with schema file tracking
  - Command-based database creation with automatic schema file generation
  - Automatic ECS component discovery for database tables and columns
- Nix integration with dynamic target discovery and JSON introspection ✅
- Git integration with command modeling and DCG parsing ✅
- Project management: context/config/dependency handling ✅
- **Session management**: File-based sessions (.db extension) with SQLite command logging and database ECS discovery ✅
  - Session workspaces with commands.db and state.pl files
  - ECS database integration with automatic table/column discovery
  - Session state persistence for entity loads across operations
- **Interface layer**: Multi-frontend ECS-native interface system ✅
- Testing infrastructure: PLUnit-based comprehensive tests ✅
- Python bridge: Prolog-Python interface ✅ (available, not required for core)

### Technical Achievements
- **Fantasy-themed spell system** with conjure/perceive command separation and clean CLI
- **Database system** with reverse proxy predicating, schema tracking, and automatic ECS discovery
  - SQLite3 CLI integration replacing broken prosqlite library
  - 3-parameter database registration: `registered_db(database(Id), data(file(DbPath)), schema(file(SchemaPath)))`
  - Command-based database creation with automatic schema file generation
- Dynamic flake target discovery (`nix flake show --json`) with memoization
- Clean sum-type command modeling across domains
- Cross-domain integration (Git/Nix/Project/Session/Database) with semantic mounting
- **ECS-native interface system** with auto-generated CLI usage and multi-frontend support
- **File-based session management** with SQLite command logging (.db extension) and database ECS discovery
  - Session workspaces with commands.db and state.pl files
  - ECS database integration with automatic table/column component discovery
- Robust test coverage (116/128 passing, 90.6% pass rate) across all subsystems

## Next Phases

- Phase 7: LLM Integration in Prolog
  - ECS command discovery via `component(command, ctor, _)`
  - Transaction construction and approval workflow
  - SWI-Prolog HTTP client for OpenAI
  - Agent entity framework

- Phase 8: Composite Project Architecture & Enhanced Discovery
  - Multi-project composition and orchestration
  - Enhanced filesystem discovery and project type inference
  - Cross-language analysis and dependencies

- Phase 9: Knowledge Evolution Layer
  - Enhanced database layer with knowledge evolution tracking
  - Transaction logging and learning from user interactions

## Recent Updates (Aug 4, 2025)
- Phase 5.4 complete – all 6 language templates implemented
- Removed C# template; focusing on Rust, Python, C++, Haskell, Lean4, MkDocs
- 41 total tests passing
- Future: Dart/Flutter template planned

## CLI Interface

Grimoire provides a unified CLI interface (`./grimoire`) that serves as a context-aware gateway to the ECS system:

```bash
./grimoire status       # Show session/transaction status
./grimoire compt        # List component types for current entity
./grimoire comp <type>  # List components of specific type
./grimoire doc [entity] # Show documentation
./grimoire repl         # Start interactive REPL
./grimoire test         # Run test suite

# Spell System - Fantasy-themed command separation
./grimoire cast "conjure(git(commit('message')))"    # Execute mutations
./grimoire perceive "git(status(Branch, Status, Files))" # Query with variable bindings
```

### Context-Aware Operation

The CLI automatically detects project context:
- **Global context**: When run outside a project, operates on the system entity
- **Project context**: When run in a directory with `semantics.pl`, loads and operates on that project entity

### Interface Architecture

- **Multi-Frontend Design**: `src/interface.pl` provides structured return values for CLI, API, and MCP frontends
- **ECS Integration**: Interface commands are registered as ECS entities with proper namespacing
- **Auto-Generated Usage**: CLI usage is dynamically generated from ECS component definitions

## Getting Started

### CLI Usage
```bash
# Check system status
./grimoire status

# Explore available entities and components
./grimoire compt system
./grimoire comp subcommand git

# View documentation
./grimoire doc git
./grimoire doc interface

# Run tests
./grimoire test
```

### Programmatic Usage
- Load core domains:
  ```prolog
  :- load_entity(semantic(folder("src/nix"))).
  :- load_entity(semantic(folder("src/project"))).
  :- load_entity(semantic(file("src/git.pl"))).
  :- load_entity(semantic(file("src/fs.pl"))).
  ```
