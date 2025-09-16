# GRIMOIRE: Knowledge-Based Operating System

Grimoire is an Entity-Component-System (ECS) architecture that serves as both an evolving knowledge base and an operating system. Every subsystem recursively implements the same ECS patterns, creating a fractal architecture of knowledge organization.

## Core Philosophy

### Everything is Knowledge
All system state, configuration, and operations are represented as semantic knowledge encoded in Prolog. There is no distinction between "code" and "data" - everything is declarative knowledge that can be reasoned about.

### File-Based Truth
All knowledge lives in files (`semantics.pl`), following the principle of immutability. There are no runtime assertions that modify the knowledge base - all knowledge is loaded from files and remains constant during execution. This ensures reproducibility and enables git-like versioning.

### Distributed Patterns
Knowledge is organized locally - each domain (git, nix, filesystem, etc.) maintains its own `semantics.pl` file that declares entities and components relevant to that domain. These can be composed together through the `load_entity(semantic(...))` mechanism.

### Entity-Component-System Architecture
- **Entities** (`entity/1`): Things that exist - from concrete objects like files to abstract concepts like transactions
- **Components** (`component/3`): Relationships and properties that connect entities
- **Systems**: Operations that act on entities with specific components

### Recursive Subsystems
Each domain implements the same ECS patterns recursively. For example, `nix` is an entity with its own entities like `nix(flake)`, which has its own entities like `nix(flake(template))`. This fractal structure allows unlimited depth while maintaining consistent patterns.

## Knowledge Organization

### Semantic Sources
- `semantic(file("path/to/file.pl"))`: Individual knowledge files
- `semantic(folder("path/to/dir"))`: Folders containing `semantics.pl`

Each semantic source is self-contained and can be loaded independently.

### Test Files (*.plt)
Discriminative context that validates the generative knowledge. While `.pl` files define what exists (generative), `.plt` files verify correctness (discriminative). Tests ensure the knowledge base remains consistent and operations behave as expected.

### Operation Model
- **Perceive**: Query operations that observe without modification
- **Conjure**: Mutable operations that change system state
- **Cast**: The mechanism to execute spells with proper error handling

All operations are structured as Prolog terms, maintaining semantic clarity and enabling reasoning about operations themselves.

## Core Concepts

### Spell System
The fundamental magic system of Grimoire. Spells are divided into two categories:
- **conjure**: Mutable operations that change system state
- **perceive**: Perception operations that query system state

Format: `spell(conjure(...))` or `spell(perceive(...))`

#### Conjuration Spells
Conjuration spells modify system state and must be cast using `cast/2` predicate for safety.

Format: `cast(conjure(operation(...)), Result)`

Examples:
- `cast(conjure(git(commit('message'))), Result)`
- `cast(conjure(mkdir('path')), Result)`

#### Perception Spells
Perception spells query system state without modification. Called directly, with variables unified to results.

Format: `perceive(query(Var1, Var2, ...))`

Examples:
- `perceive(git(status(Branch, Ahead, Files)))`
- `perceive(nix(flake(show(Apps, Packages, DevShells))))`

### Spell Examples

| Operation | Spell Type | Usage | Description |
|-----------|------------|-------|-------------|
| Read File | Perceive | `perceive(read_file(FilePath, LineNumbers, Content))` | Read specific lines from files |
| Edit File | Conjure | `cast(conjure(edit_file(file(Path), Edits)), Result)` | Modify file contents with structured operations |
| Git Status | Perceive | `perceive(git(status))` | Query repository working tree status |
| Git Clone | Conjure | `cast(conjure(git(clone(Url, Path))), Result)` | Clone repository to directory |
| Nix Flake Info | Perceive | `perceive(nix(flake(show(Apps, Packages, DevShells))))` | Show flake outputs and metadata |
| Nix Run | Conjure | `cast(conjure(nix(run(Target, Args))), Result)` | Execute nix applications |
| List Entities | Perceive | `perceive(entities(EntityList))` | Get all loaded entities |
| Create Project | Conjure | `cast(conjure(mkproject(Path, Name, Options)), Result)` | Initialize new project structure |

### Interface Exploration

The system provides interface commands for ECS exploration:

- `interface(entities)` - List all entities in the system
- `interface(compt(Entity))` - List component types for an entity
- `interface(comp(Entity, Type))` - List components of specific type
- `interface(doc(Entity))` - Show entity documentation
- `interface(status)` - Show session/transaction status

### Core System Concepts

- **Transaction**: Atomic units of change with rollback capability through git-backed session management
- **Hardware**: Hardware abstraction layer and system resource management
- **Execute**: Execution contexts and runtime environments within Grimoire
- **Source**: Source code and data representation with semantic tracking
- **Project**: Organizational unit bundling source code, configuration, and resources
- **Interface**: Command interfaces for system interaction (CLI, REST API, MCP protocols)
- **Git**: Knowledge evolution tracking and version control subsystem
- **Nix**: Symbolic configuration, package management, and build subsystem
- **Session**: Transaction and workspace management with SQLite command logging