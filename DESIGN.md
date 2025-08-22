# Grimoire Design Document

## Project Evolution: MyPAOS → Grimoire

### Vision Statement
Grimoire is a knowledge-based operating system built on Entity-Component-System (ECS) architecture with Prolog as the semantic knowledge layer and Nix as the system configuration foundation.

## Core System Domains

### Nix Domain - System Configuration Foundation
- **Purpose**: System configuration, packages, and flake management
- **Paradigm**: Declarative system configuration with reproducible builds
- **Key Features**:
  - Dynamic target discovery via `nix flake show --json` introspection
  - Memoized performance optimization
  - Template system with Nix-centric commands
  - All language templates use `nix run .#command` pattern for canonical operations
- **Revolutionary Design**: All 6 language templates (Rust, Python, C++, Haskell, Lean4, MkDocs) use Nix flake apps instead of language-native commands

### Database Domain - SQLite ECS Integration
- **Purpose**: Database creation, management, and automatic ECS discovery
- **Paradigm**: Reverse proxy predicating with schema tracking and command-based creation
- **Key Features**:
  - Schema-aware database registration: `registered_db(database(Id), data(file(DbPath)), schema(file(SchemaPath)))`
  - Command-based database creation: `command(db(create(DbId, DbPath, schema(file|sql))))`
  - Automatic schema file generation for inline SQL
  - ECS component discovery for database tables and columns
  - Integration with session command logging system

### Git Domain - Version Control
- **Purpose**: Version control operations and repository management
- **Paradigm**: Git-native operations with command modeling
- **Key Features**:
  - Command modeling for all Git operations
  - Repository state introspection and management
  - Integration with project discovery system

### Session Domain - File-Based Workspace Management
- **Purpose**: Logical work contexts with file-based command logging and ECS database discovery
- **Paradigm**: Session → Workspace directory with commands.db (.db extension) and state.pl files
- **Key Features**:
  - File-based session workspaces under `${GRIMOIRE_ROOT}/sessions/`
  - SQLite command logging (.db extension) with automatic ECS discovery and schema tracking
  - Session database registration: `registered_db(database(session_SessionId), data(file(DbPath)), schema(file(SchemaPath)))`
  - Session state management via state.pl files for persistent entity loads
  - **ECS Database Integration**: Session commands database becomes discoverable ECS entity
    - Automatic table/column component discovery for session database
    - Schema files automatically created: `{session_workspace}/commands.schema.sql`
    - Database creation via command system: `command(db(create(SessionDbId, DbPath, schema(file(SchemaFile)))))`
  - **Stateful Interface System**: Session-owned Prolog files for persistent entity state
    - Entity loads persist across operations within sessions
    - Interface operations automatically load session state for stateful behavior
    - Session isolation: different sessions maintain separate entity state
    - CLI integration: `./grimoire load entity` persists entities for session-scoped access

### Filesystem Domain - Project Discovery
- **Purpose**: Filesystem introspection and artifact discovery
- **Paradigm**: Pattern-based discovery with configurable traversal
- **Key Features**:
  - `discover_fs_components` utility for filesystem scanning
  - Glob pattern matching for file type identification
  - Integration with project template systems

### Project Domain - Artifact Management
- **Purpose**: Project structure analysis and artifact discovery
- **Paradigm**: Template-based project analysis with core artifact requirements
- **Key Features**:
  - Core artifacts: `nix(flake)`, `git`, `readme`, `sources`
  - `discover_project_artifacts` with configurable patterns
  - Template composition for multi-project architectures
  - Language-specific project type identification

## Core Design Principles

### 1. Knowledge Locality
- Each domain maintains its knowledge in local `semantics.pl` files
- Git-like distributed knowledge pattern
- Semantic mounting system for runtime knowledge extension
- No runtime asserts - all knowledge lives in files

### 2. ECS Architecture
- `entity/1` - Things that exist in the system
- `component/3` - Relationships between things
- **Component Relation Patterns**:
  - `component(Type, ctor, Constructor)` - Extensible sum types (variants)
  - `component(Type, relation_pattern, Pattern)` - Meta-patterns for defining new relation types
- **Enhanced Spell System**: Fantasy-themed command/query separation
  - `component(conjure, ctor, ...)` - Mutation operations (state-changing)
  - `component(perceive, ctor, ...)` - Query operations (read-only)
  - `cast/2` - Execute conjure spells with explicit results
  - `perceive/1` - Enhanced multifile predicate for domain queries
    - Collects all solutions through backtracking
    - Displays multiple solutions with semicolon separation
    - Direct domain queries: `git(status(...))` works without wrapper
- Clean composition and extension patterns

### 3. Domain Natural Paradigms
- **Nix**: System configuration, packages, flakes
- **Git**: Version control operations
- **Prolog**: Semantic queries and knowledge representation
- **SQLite**: Database entity system with automatic ECS discovery
- Each tool speaks its natural language

### 4. Multi-Frontend Interface System

The interface layer (`src/interface.pl`) provides a clean separation between the ECS system and various frontends (CLI, API, MCP):

**ECS-Native Interface Pattern**: Interface as ECS entity with subcommands that promote to command constructors

**Structured Return Values**: Interface returns data structures, not terminal output - CLI formats the structured data for display

**Context-Aware Operation**: Auto-detect current working context and ensure local project loading

**Stateful Session Integration**: Interface operations automatically load session state when in session context

**Benefits**:
- **Multi-Frontend Ready**: Same interface layer supports CLI, API, and MCP
- **ECS Integration**: Interface commands follow established ECS patterns
- **Context Awareness**: Automatically detects and loads local project semantics
- **Structured Data**: Returns data structures, not formatted output
- **Auto-Generated Usage**: CLI usage generated from ECS component definitions
- **Session State Persistence**: Entity loads persist across operations within sessions
- **Session Isolation**: Different sessions maintain separate interface state

## Entity Management System

### Entity Declaration Conventions
The system uses explicit entity declarations for all semantic files:

**Explicit Entity Declarations**: All semantic files declare their entities directly
```prolog
% git.pl - Core git domain
:- self_entity(git).
component(git, source, file("git.pl")).

% rust/semantics.pl - Rust project template
:- self_entity(rust_template).
component(rust_template, project_type, rust).
component(command, ctor, rust_template).
```
- Use for: All entities (core domains, templates, projects)
- Characteristics: Explicit naming, no ambiguity, predictable behavior
- Loading: Simple `load_entity(semantic(file/folder))` pattern

This explicit approach ensures clarity, maintainability, and eliminates complexity.

### Self-entity declaration

To make semantics files self-describing and avoid repeating source paths, we recommend using ``:- self_entity(Entity).`` at the top of a semantics file instead of a plain ``entity(Entity).`` header.

Behavior:
- Asserts ``entity(Entity)`` for the file being loaded.
- Adds a ``component(Entity, self, semantic(...))`` component pointing to the file or folder that defined the entity. If the file is named ``semantics.pl`` the component will be ``semantic(folder(Dir))``, otherwise ``semantic(file(Path))``.

Example usage:
```prolog
% In src/git.pl
:- self_entity(git).
% component(git, self, semantic(file("src/git.pl"))).

% In a template's semantics.pl
:- self_entity(rust_template).
% component(rust_template, self, semantic(folder("src/nix/templates/rust"))).
```

### Load Entity API Design

The system implements a simple single-arity API for loading semantic entities:

**load_entity/1 - Semantic Entity Loading**
```prolog
% Load semantic files directly
load_entity(semantic(file("src/git.pl"))).
load_entity(semantic(folder("src/nix"))).
load_entity(semantic(file("templates/rust/semantics.pl"))).
```
- Use for: All entity loading (core domains, templates, projects)
- Pattern: Direct semantic source specification
- Behavior: Loads and asserts all facts from the semantic source

**API Benefits:**
- **Simplicity**: Single pattern for all entity loading
- **Clarity**: Source location explicit in call
- **Predictability**: No transformation or binding complexity
- **Maintainability**: Easy to understand and debug

### Clean Entity Loading
Simple pattern for loading domain knowledge:
```prolog
% In core_rules.pl
load_entity(semantic(Source)) :-
    mount_semantic(Source).

% In grimoire.pl - core system loading
:- load_entity(semantic(file("src/git.pl"))).
:- load_entity(semantic(folder("src/nix"))).
:- load_entity(semantic(file("src/fs.pl"))).
:- load_entity(semantic(folder("src/project"))).

% Template loading
?- load_entity(semantic(file("templates/rust/semantics.pl"))).
% Now rust_template entity is available

% Project loading via passive loading
?- passive_load(my_project, semantic(file("project/semantics.pl"))).
```

Benefits:
- **Simple single-arity API** with clear semantics
- **Explicit source specification** - no magic or transformation
- **Maintains ECS purity** - no asserts in semantic files
- **Predictable behavior** - what you load is what you get
- **Easy debugging** - clear loading patterns

## Current System Status

**System State: Functional**
- 94 tests passing across all subsystems
- Core ECS architecture with knowledge locality
- Spell system with multiple-solution perceive queries
- Git-backed session management with stateful interface system
- Multi-frontend interface layer with session-persistent entity state
- 6 language templates with Nix-centric commands

**CLI Commands:**
- `./grimoire perceive "query"` - Multiple solutions with semicolon separation
- `./grimoire conjure "operation"` - Mutation operations
- `./grimoire exec "query"` - Direct Prolog query execution
- `./grimoire session switch/list/commit/rollback` - Transaction management
- `./grimoire load entity` - Load entity into current session for persistent access
- Domain queries work directly: `git(status(...))`, `session(current(...))`

### Completed Phases Overview

**Phase 1-6 + Interface Layer + Stateful Sessions Complete**: Foundation cleanup, Nix CLI mastery, testing infrastructure, simplified entity loading system, complete Nix-centric template revolution, Git-backed session system, multi-frontend interface layer, and stateful interface session system - **94 tests passing**

**All 6 Language Templates Complete**: Rust, Python, C++, Haskell (with full test suites), Lean4, MkDocs (templates appropriate to their domains)

**Current System Features**:
- Pure Prolog Implementation: All Python components removed, system now runs entirely in SWI-Prolog
- Spell System: Fantasy-themed conjure/perceive with multiple-solution query capabilities
- Session System: DB backed session system with persistent state `state.pl`
- Interface Layer: Multi-frontend system (`src/interface.pl`) with structured returns
- Stateful Interface Sessions: Entity loads persist across operations within sessions using session-owned Prolog files
- CLI Tool: Context-aware `./grimoire` with `conjure`/`perceive`/`exec`/`session`/`load` commands
- Templates: 6 language templates with Nix-centric commands and test coverage
- Core Systems: git, nix, fs, project, session domains loaded and tested

## Current Priorities

**Phase 7: Pure Prolog LLM Integration**:
1. **ECS Command Discovery**: Agents query `component(command, ctor, _)` to find available operations
2. **Transaction Construction**: LLM builds `transaction([command(...), ...])` from natural language
3. **OpenAI HTTP Client**: Direct API integration using SWI-Prolog HTTP library (no Python bridge needed)
4. **User Approval Workflow**: Present transaction plan before execution
5. **Agent Entity Framework**: Domain-specific agents (coding_assistant, project_manager)

**Phase 8: Composite Project Architecture & Enhanced Discovery**:
1. **Multi-Project Composition**: Combine templates in subdirectories with top-level orchestration
2. **Semantic Composition**: Load subproject semantics via `load_entity` in parent `semantics.pl`
3. **Nix Flake Composition**: Import subproject flakes as inputs in parent `flake.nix`
4. **Command Orchestration**: Implement composite commands that coordinate subproject operations
5. **Enhanced Discovery System**: Implement filesystem pattern matching and automatic project type inference

**Phase 9: Knowledge Evolution Layer**:
- Reactivate database infrastructure (`src/db/`) for persistent knowledge
- Transaction logging and knowledge base evolution tracking
- Learning from user interactions and command patterns

---

# Implementation Details

## Template System Architecture

### Nix-Centric Build Operations
**Revolutionary Design**: All language templates use Nix flake apps instead of language-native commands, ensuring reproducibility and consistency across the system.

**Template Coverage** (Phase 5.3-5.4 Complete):
- ✅ **Rust Template**: `nix run .#build|test|run|check|clippy|fmt` + full test suite
- ✅ **Python Template**: `nix run .#run|build` + `nix develop` + full test suite
- ✅ **C++ Template**: `nix run .#run|build` + `nix develop` + full test suite
- ✅ **Haskell Template**: `nix run .#build|test|run|ghci|hlint` + full test suite
- ✅ **Lean4 Template**: `nix run .#build|check|clean|doc` (no tests - formal verification focused)
- ✅ **MkDocs Template**: `nix run .#build|serve|deploy|new` (no tests - documentation generation)

**Benefits**:
- **Reproducible builds** across all environments
- **Canonical operations** - one way to build/run/test per language
- **System consistency** - all templates use the same Nix patterns
- **Zero dependency management** - Nix handles all toolchain dependencies
- **Cross-platform compatibility** - works on any Nix-supported system

## Current Architecture Assessment

### Strengths to Preserve - IMPLEMENTED
1. **Core ECS System** (`core_rules.pl`)
   - Entity-component abstractions
   - Transaction-based operations

2. **Nix Integration** (`src/nix/semantics.pl`) - COMPLETED
   - **Dynamic Target Discovery**: Memoized `nix flake show --json` introspection
   - **JSON Integration**: Flake metadata parsing

3. **Git Integration** (`src/git.pl`)
   - Command modeling
   - Domain extension patterns

4. **Interface Layer** (`src/interface.pl`)
   - Multi-frontend interface system
   - CLI, API, and MCP support

### Recent Major Improvements
1. **Pure Prolog Implementation** - Removed all Python infrastructure, system now runs entirely in SWI-Prolog
2. **Simplified Entity Loading** - Single-arity `load_entity/1` with explicit entity declarations, removed self-binding complexity
3. **Nix Semantics Overhaul** - Removed unnecessary complexity, memoized performance
4. **Polyglot Foundation** - Language-native introspection patterns, 500-1000 token contexts → 50-100 tokens
5. **Passive Loading System** - `passive_load/2` for on-demand loading, 66 tests passing
6. **Template System** - All 6 templates implemented with explicit entity declarations and discovery integration

### Components to Defer (Not Remove)
1. **Database Infrastructure** - `src/db/` SQLite logging system for knowledge evolution

**Rationale**: Focus on Prolog-native LLM integration first, then add database persistence as needed. Python infrastructure has been removed in favor of pure Prolog implementation.

## Detailed Phase History

**Phase 1: Foundation Cleanup** - Project renamed MyPAOS → Grimoire, Python infrastructure removed, core ECS architecture established

**Phase 2: Nix CLI Mastery** - Comprehensive Nix integration with flake-first approach, dynamic target discovery via JSON introspection, seven language template domains implemented

**Phase 3: Testing Infrastructure** - 28 comprehensive tests, PLUnit framework with setup/cleanup patterns, cross-domain integration validation

**Phase 4: Simplified Entity Loading** - Single-arity `load_entity/1` implementation, explicit entity declarations, clean semantic mounting

**Phase 5.1-5.4 Complete**: **Nix-Centric Template Revolution** - Implemented system domains and **completely redesigned language templates to use Nix flake apps instead of language-native commands**. All 6 templates (Rust, Python, C++, Haskell, Lean4, MkDocs) now use `nix run .#command` pattern for canonical, reproducible operations.

**Interface Layer Complete**: **Multi-Frontend Interface System** - Implemented `src/interface.pl` as ECS-native interface layer with structured return values. Transformed `./grimoire` from bash script to Prolog CLI wrapper with context-aware operation and auto-generated usage.

## Discovery-Based Template Implementation Details

### New System Domains
`fs` domain provides filesystem discovery utilities, `project` domain handles project artifact discovery.

### Core Project Discovery Architecture
Every project entity must have these non-negotiable core artifacts:
- `nix(flake)` - every project must have a flake
- `git` - every project must be in git
- `readme` - every project must have documentation
- `sources` - every project must have source files (inferred from git)

### Current Directory Structure
After restructuring, the system follows this clean organization:
```
src/
├── core_rules.pl         # Core ECS system
├── grimoire.pl          # System initialization
├── interface.pl         # Multi-frontend interface layer
├── git.pl              # Git domain semantics
├── fs.pl               # Filesystem utilities
├── session.pl          # Session management
├── project.pl          # Project semantics
├── db/                 # Database infrastructure
│   ├── schema.sql
│   └── semantics.pl
├── nix/                # Nix domain and templates
│   ├── semantics.pl
│   └── templates/
│       ├── rust/
│       ├── python/
│       ├── cpp/
│       ├── haskell/
│       ├── lean4/
│       └── mkdocs/
├── project/            # Project domain
│   ├── semantics.pl
│   └── semantics.plt
└── tests/              # Test suites
    ├── run_tests.pl
    ├── core_rules.plt
    ├── integration.plt
    └── [other test files...]
```

### Template Structure
All templates follow a standardized structure:
```
src/nix/templates/{language}/
├── semantics.pl          # Generative context with discovery
├── semantics.plt         # Discriminative test suite
└── [existing template files...]
```

### Template Requirements
- Project type and build tool identification
- Build system subcommand integration
- Docstrings for all commands
- Filesystem pattern matching for language-specific files
- Discovery system integration
- Test suite with 5+ discriminative tests

## Recent Updates

**August 19, 2025**:
- **Pure Prolog Implementation**: Completely removed Python infrastructure
  - All components now run in SWI-Prolog
  - No external Python dependencies
  - Simplified architecture with single runtime
- **Repository Restructuring**: Moved all Prolog files from `src/prolog/` to `src/`
  - Clean directory structure
  - All paths updated throughout system
  - Git history preserved for all moves
- Enhanced Perceive System implemented:
  - Multiple choicepoint collection through backtracking
  - Semicolon-separated solutions like normal Prolog queries
  - Direct domain queries: `./grimoire perceive "git(status(...))"` works without wrapper
  - Proper fallback: tries `perceive/1` first, then `call/1`
  - Clean error handling for failed queries
- `./grimoire exec "query"` command for arbitrary Prolog queries
- 66 total tests passing - Core (27) + Templates (14) + Spell System (21) + Session CLI (4)
- All previous systems maintained:
  - Phase 6: Git-backed session system with clean ontology
  - Interface Layer: Multi-frontend ECS-native interface system
  - All 6 language templates with Nix-centric commands
  - Complete CLI interface with session management


# Detailed System Designs

## Session + Transaction System Design

### File-Based Session Management with Database ECS Discovery

**Core Architecture**: Sessions create workspace directories with SQLite command logging and automatic ECS discovery.

**Session**: A logical work context that creates a workspace directory with commands.db and state.pl
**Database Integration**: Session commands database becomes an ECS entity with auto-discovered table structures

### Session Workspace Management

```prolog
entity(session).
entity(session_commands_db).  % ECS entity for session database

% Session workspace structure
component(session, workspace_path, path(WorkspacePath)) :-
    session_workspace_path(SessionId, WorkspacePath).

% Session database as ECS entity with auto-discovery
component(session_commands_db, data, data(file(DbPath))) :-
    session_commands_db_path(SessionId, DbPath).

% Auto-discovered database structure
entity(session_commands_db(table(commands))).
component(session_commands_db(table(commands)), columns, [
    column(id, 'INTEGER', false, null, true),
    column(timestamp, 'TEXT', true, null, false),
    column(command_type, 'TEXT', true, null, false),
    column(command_term, 'TEXT', true, null, false),
    column(result, 'TEXT', false, null, false),
    column(source, 'TEXT', false, 'user', false)
]).

% Create session workspace with database discovery
start_session(SessionId, Result) :-
    uuid(SessionId),
    create_session_workspace(SessionId),
    % Discover database structure as ECS components
    discover_database_artifacts(session_commands_db),
    Result = ok(session_started(SessionId)).
```

### Command Logging with Database ECS Integration

```prolog
% Log commands to session database with ECS discovery
log_command_to_session(Command, CommandType, Result, Source) :-
    get_current_session_id(SessionId),
    (SessionId = main ->
        true  % Don't log commands in main session
    ;
        log_command_to_session_db(SessionId, Command, CommandType, Result, Source),
        % Update ECS components after logging
        discover_database_artifacts(session_commands_db)
    ).

% Query session history via ECS components
get_session_command_history(SessionId, Commands) :-
    (SessionId = main ->
        Commands = []  % Main session has no accumulated commands
    ;
        % Query via database entity
        component(session_commands_db, data, data(file(DbPath))),
        sqlite3_query(DbPath,
            'SELECT timestamp, command_type, command_term, result, source FROM commands ORDER BY timestamp DESC',
            QueryResults),
        maplist(json_to_command_entry, QueryResults, Commands)
    ).

% ECS-based database introspection
inspect_session_database_structure(SessionId) :-
    discover_database_artifacts(session_commands_db),
    findall(Table, component(session_commands_db, table, Table), Tables),
    forall(member(TableName, Tables), (
        TableEntity =.. [session_commands_db, table(TableName)],
        component(TableEntity, columns, Columns),
        format('Table ~w: ~w~n', [TableName, Columns])
    )).
```

### Session Management Patterns

```prolog
% Session closure strategies
close_session(SessionId, CloseStrategy, Result) :-
    active_session(SessionId, BranchName, StartCommit),

    (CloseStrategy = merge_to_main ->
        run(command(git(checkout(['main']))), _),
        run(command(git(merge(['--no-ff', BranchName]))), _),
        run(command(git(branch(['-d', BranchName]))), _)
    ; CloseStrategy = abandon ->
        run(command(git(checkout(['main']))), _),
        run(command(git(branch(['-D', BranchName]))), _)
    ; CloseStrategy = keep_branch ->
        true  % Keep branch for later reference
    ),

    retract(active_session(SessionId, BranchName, StartCommit)).

% Convenience workflow patterns
with_session(BranchStrategy, Commands, Result) :-
    start_session(SessionId, BranchStrategy, _),
    execute_transaction(SessionId, Commands, TransactionResult),
    close_session(SessionId, merge_to_main, CloseResult),
    Result = session_completed(SessionId, TransactionResult, CloseResult).

% Multi-transaction workflows
session_workflow(SessionId, BranchStrategy, TransactionsList, Result) :-
    start_session(SessionId, BranchStrategy, _),
    execute_session_transactions(SessionId, TransactionsList, Results),
    close_session(SessionId, merge_to_main, CloseResult),
    Result = session_workflow_completed(SessionId, Results, CloseResult).
```

### Session Context and History

```prolog
% Session introspection
list_active_sessions(Sessions) :-
    findall(session(Id, Branch, Start), active_session(Id, Branch, Start), Sessions).

list_session_transactions(SessionId, Transactions) :-
    findall(transaction(TId, Commands, State),
            session_transaction(SessionId, TId, Commands, State),
            Transactions).

% Git-native session history
show_session_history(SessionId) :-
    active_session(SessionId, BranchName, _),
    run(command(git(log(['--oneline', '--graph', BranchName]))), ok(History)),
    format("Session ~w history:~n~w~n", [SessionId, History]).
```

### Integration with Existing System

```prolog
% Backwards compatibility - sessions are optional
execute(transaction(Commands), Result) :-
    (active_session(SessionId, _, _) ->
        execute_transaction(SessionId, Commands, Result)
    ;
        % Legacy direct execution
        execute_commands(Commands, Results),
        (member(error(E), Results) -> Result = error(E) ; Result = ok(Results))
    ).
```

## Stateful Interface Session System

### Overview

The stateful interface session system provides persistent entity state across operations within sessions using session-owned Prolog files. This enables workflows where entity loads persist for the duration of a session, providing a stateful context for interface operations.

### Architecture

**Session-Owned Prolog Files**: Each session creates a persistent state file at `${GRIMOIRE_ROOT:-$HOME/.grimoire}/session-{uuid}.pl` containing entity load directives.

**Automatic State Loading**: Interface operations automatically load session state when operating within a session context.

**Session Isolation**: Different sessions maintain completely separate entity state via independent state files.

### Core Components

#### Session State File Management

```prolog
% Session state file path resolution
session_state_file_path(SessionId, FilePath) :-
    session_storage_strategy(Strategy),
    session_state_file_path(SessionId, Strategy, FilePath).

% Global storage: ${GRIMOIRE_ROOT:-$HOME/.grimoire}/session-{uuid}.pl
session_state_file_path(SessionId, global, FilePath) :-
    grimoire_root_directory(GrimoireRoot),
    format(atom(FileName), 'session-~w.pl', [SessionId]),
    format(atom(FilePath), '~w/~w', [GrimoireRoot, FileName]).

% Initialize empty session state file
initialize_session_state_file(SessionId) :-
    ensure_session_state_directory(SessionId),
    session_state_file_path(SessionId, FilePath),
    open(FilePath, write, Stream),
    format(Stream, '% Session state file for session ~w~n', [SessionId]),
    format(Stream, '% Auto-generated by Grimoire session system~n~n', []),
    close(Stream).
```

#### Entity Loading with Session Persistence

```prolog
% Load entity into current session state
load_entity_in_session(Entity) :-
    get_current_session_id(SessionId),
    entity_to_semantic_spec(Entity, EntitySpec),
    % Validate that the entity exists before adding to session
    (validate_entity_exists(Entity, EntitySpec) ->
        (SessionId \= main ->
            add_entity_load_to_session(SessionId, EntitySpec)
        ;
            true  % In main session, no persistent storage
        )
    ;
        format(atom(ErrorMsg), 'Entity ~w not found', [Entity]),
        throw(entity_load_failed(Entity, EntitySpec, ErrorMsg))
    ).

% Add entity load to session state file
add_entity_load_to_session(SessionId, EntitySpec) :-
    ensure_session_state_directory(SessionId),
    session_state_file_path(SessionId, FilePath),
    open(FilePath, append, Stream),
    format(Stream, '% Entity loaded: ~w~n', [EntitySpec]),
    format(Stream, ':- load_entity(~q).~n~n', [EntitySpec]),
    close(Stream).
```

#### Interface Operation Integration

```prolog
% Interface operations automatically load session state
run(command(interface(compt)), RetVal) :-
    ensure_session_state_loaded,
    current_entity(Entity),
    interface_compt(Entity, Types),
    RetVal = ok(component_types(Entity, Types)).

% Ensure session state is loaded before interface operations
ensure_session_state_loaded :-
    get_current_session_id(SessionId),
    (SessionId \= main ->
        load_session_state_file(SessionId)
    ;
        true  % In main session, no persistent state to load
    ).
```

### CLI Integration

The `./grimoire load` command provides CLI access to the stateful interface system:

```bash
# Load entity into current session for persistent access
./grimoire load system
./grimoire load interface
./grimoire load .

# Entities persist across operations within the same session
./grimoire compt system  # Uses loaded session state
./grimoire comp system concept  # Uses loaded session state
```

### Session Workflow Example

```prolog
% Start a session
?- start_session('my-work-session', Result).

% Load entities for persistent access
?- run(command(interface(load('system'))), LoadResult).
% Session file now contains: :- load_entity(semantic(entity(system))).

% Switch to another session temporarily
?- start_session('temp-session', _).

% Return to original session
?- run(command(git(checkout(['session-my-work-session']))), _).

% Interface operations automatically reload session state
?- run(command(interface(compt)), Result).
% Automatically loads session state including system entity

% Close session with cleanup
?- close_session('my-work-session', merge_to_main, _).
% Session state file is cleaned up
```

### Testing

The system includes comprehensive test coverage:

- **Session State File Lifecycle**: Creation, persistence, and cleanup
- **Entity Load Validation**: Proper error handling for non-existent entities
- **Session Isolation**: Different sessions maintain separate state
- **Interface Integration**: Automatic state loading in interface operations
- **Semantic Specification Mapping**: Correct entity-to-semantic mappings

All 94 tests pass, including 10 specific interface session integration tests that validate the complete stateful interface workflow.

### Key Design Benefits

1. **Session Isolation**: Concurrent work streams in separate Git branches
2. **Atomic Transactions**: Git commits provide natural transaction boundaries
3. **Git-Native History**: Full audit trail via Git log and branching
4. **Flexible Workflows**: Support for experimental, feature, and iterative development
5. **Clean Integration**: Sessions enhance but don't break existing transaction system
6. **Rollback Support**: Git reset provides transaction-level rollback capability
7. **LLM Context**: Sessions provide natural boundaries for agent work contexts

## LLM Integration Design

### ECS-Driven Agent Workflow

The core LLM agent workflow leverages the existing ECS and transaction systems:

1. **ECS Exploration**: Agent queries the ECS to discover available commands for its task
2. **Transaction Construction**: Agent builds a complete transaction with discovered commands
3. **User Approval**: Present transaction plan to user (with auto-execute option)
4. **Transaction Execution**: Run the complete transaction using existing `execute/2`

### OpenAI API Integration

Direct HTTP integration for OpenAI-compatible endpoints using SWI-Prolog's HTTP library:

```prolog
entity(llm_agent).
component(llm_agent, provider, openai).
component(llm_agent, model, "gpt-4o").
component(llm_agent, api_endpoint, "https://api.openai.com/v1/chat/completions").

% Send request to LLM provider
llm_request(Agent, Messages, Response) :-
    component(Agent, api_endpoint, Endpoint),
    component(Agent, model, Model),
    http_post(Endpoint,
        json(_{model: Model, messages: Messages}),
        Response,
        [authorization(bearer(ApiKey)),
         content_type('application/json')]).
```

### ECS Command Discovery

Agents explore the ECS to find relevant commands for their task:

```prolog
% Discover available commands for agent planning
discover_commands_for_task(Task, AvailableCommands) :-
    findall(Command, component(command, ctor, Command), AllCommands),
    filter_relevant_commands(Task, AllCommands, AvailableCommands).

% Get command documentation for agent context
get_command_context(Commands, Context) :-
    maplist(get_command_doc, Commands, Docs),
    atomic_list_concat(Docs, '\n\n', Context).

get_command_doc(Command, Doc) :-
    (docstring(Command, CommandDoc) ->
        format(string(Doc), "~w: ~w", [Command, CommandDoc])
    ;
        format(string(Doc), "~w: No documentation available", [Command])
    ).
```

### Transaction Construction Agent

Core agent that constructs transactions from natural language tasks:

```prolog
entity(transaction_planner).
component(transaction_planner, agent_type, llm_agent).
component(transaction_planner, model, "gpt-4o").
component(transaction_planner, role, transaction_construction).

% Main agent workflow: Task -> Transaction
plan_transaction(Agent, UserTask, Transaction) :-
    % 1. Discover relevant commands
    discover_commands_for_task(UserTask, Commands),
    get_command_context(Commands, CommandContext),

    % 2. Build LLM prompt with ECS context
    format(string(Prompt),
        "Task: ~w~n~nAvailable Commands:~n~w~n~nConstruct a transaction (list of commands) to complete this task. Return only the Prolog transaction term.",
        [UserTask, CommandContext]),

    % 3. Get LLM response
    llm_request(Agent, [_{role: "user", content: Prompt}], Response),
    parse_transaction_response(Response, Transaction).

% Parse LLM response into transaction term
parse_transaction_response(Response, transaction(Commands)) :-
    _{choices: [_{message: _{content: Content}}]} :< Response,
    % Parse Prolog term from LLM response
    read_term_from_atom(Content, transaction(Commands), []).
```

### Agent Transaction Execution

Execute agent-constructed transactions with optional user approval:

```prolog
% Execute agent task with user approval
execute_agent_task(Agent, Task, Result) :-
    plan_transaction(Agent, Task, Transaction),

    % Present transaction to user
    format("Agent proposes transaction:~n~w~n~nExecute? (y/n/auto): ", [Transaction]),
    read_line_to_string(user_input, Response),

    ( Response = "y" ; Response = "auto" ->
        execute(Transaction, Result)
    ; Response = "n" ->
        Result = cancelled("User rejected transaction")
    ;
        Result = error("Invalid response")
    ).

% Auto-execute mode for brave users
execute_agent_task_auto(Agent, Task, Result) :-
    plan_transaction(Agent, Task, Transaction),
    format("Agent executing: ~w~n", [Transaction]),
    execute(Transaction, Result).
```

### Agent Entity Framework

Define specialized agents for different domains:

```prolog
entity(coding_assistant).
component(coding_assistant, agent_type, transaction_planner).
component(coding_assistant, domain, [rust_template, python_template, git, nix]).

entity(project_manager).
component(project_manager, agent_type, transaction_planner).
component(project_manager, domain, [fs, project, git]).

% Domain-specific command filtering
filter_relevant_commands(Task, AllCommands, FilteredCommands) :-
    % Simple keyword matching - could be enhanced with embeddings
    findall(Cmd, (
        member(Cmd, AllCommands),
        command_relevant_to_task(Cmd, Task)
    ), FilteredCommands).
```

This design leverages the existing ECS system for command discovery and the current transaction system for execution, creating a natural bridge between LLM planning and Grimoire's operational capabilities.

## Composite Project Architecture Design

### Multi-Project Composition Pattern

Templates can be composed by instantiating multiple templates in subdirectories and orchestrating them at the parent level:

```
fullstack-app/
├── flake.nix              # Orchestrator flake with subproject inputs
├── semantics.pl           # Composite semantics loading subprojects
├── frontend/              # Template instance (e.g., React, Vue)
│   ├── flake.nix
│   └── semantics.pl
└── backend/               # Template instance (e.g., Rust, Python)
    ├── flake.nix
    └── semantics.pl
```

### Semantic Composition

Parent `semantics.pl` loads subproject semantics and defines orchestration commands:

```prolog
entity(fullstack_app).
component(fullstack_app, project_type, composite).

% Load subproject semantics
:- load_entity(semantic(file("frontend/semantics.pl"))).
:- load_entity(semantic(file("backend/semantics.pl"))).

% Composite commands
component(command, ctor, fullstack_app).
component(fullstack_app, subcommand, dev).
component(fullstack_app, subcommand, build).

run(command(fullstack_app(dev)), RetVal) :-
    run(command(nix(run(['.#dev']))), RetVal).
```

### Nix Flake Composition

Parent `flake.nix` imports subproject flakes as inputs and composes their outputs:

```nix
{
  inputs = {
    frontend.url = "path:./frontend";
    backend.url = "path:./backend";
  };

  outputs = { self, frontend, backend }: {
    apps.x86_64-linux.dev = {
      type = "app";
      program = pkgs.writeShellScript "dev" ''
        ${frontend.apps.x86_64-linux.dev.program} &
        ${backend.apps.x86_64-linux.dev.program} &
        wait
      '';
    };
  };
}
```

### Command Orchestration Patterns

- **Parallel execution**: Development servers running simultaneously
- **Sequential builds**: Backend first, then frontend with backend artifacts
- **Conditional execution**: Skip frontend build if backend fails
- **Service coordination**: Backend provides API endpoint, frontend consumes it

This approach maintains template reusability - no modifications needed to existing templates when used as subprojects.
