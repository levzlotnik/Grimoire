# Grimoire

A knowledge-based operating system built on Entity-Component-System (ECS) architecture. Grimoire treats semantic knowledge as first-class citizens, storing it in Prolog files and integrating domain-specific tools through common patterns.

## Installation

**Primary Method: Nix Develop**
```bash
# Clone and enter development environment
git clone <repository-url>
cd Grimoire
nix develop

# Or directly from flake
nix develop github:your-org/grimoire
```

**MCP Server Setup**
Add to your MCP configuration (`.mcp.json` or similar):
```json
{
  "mcpServers": {
    "Grimoire": {
      "type": "stdio", 
      "command": "./grimoire",
      "args": ["mcp"],
      "env": {
        "GRIMOIRE_ROOT": "/path/to/grimoire"
      }
    }
  }
}
```

## Quick Start

**Basic CLI Commands**
```bash
# Interactive REPL with system loaded
./grimoire exec

# Run full test suite
./grimoire test

# Run specific tests
./grimoire test session_management

# Cast conjuration spells (mutations)
./grimoire conjure "git(commit('fix: update documentation'))"
./grimoire conjure "mkproject"
./grimoire conjure "session(start('my-session'))"

# Cast perception spells (queries)
./grimoire perceive "git(status)"
./grimoire perceive "entities"
./grimoire perceive "session(current)"
```

**Environment Variables**
- `GRIMOIRE_ROOT` - Project directory (auto-detected from script location)
- `GRIMOIRE_DATA` - Data storage directory (defaults to `$HOME/.grimoire`)

## Core Philosophy

**ECS Architecture**
```prolog
% Entities - things that exist
entity(something).

% Components - relationships and properties  
component(entity_name, component_type, value).

% Documentation - self-documenting knowledge
docstring(entity_name, "Description of the entity").
```

**Component Type Diversity**
The system uses various component types beyond just constructors:
- `ctor` - Constructors (what can be created/called)
- `subcommand` - Available subcommands
- `concept` - Conceptual relationships
- `self` - Self-referential metadata
- `source` - Source code and data references
- `subsystem` - System subsystem relationships

**Self-Entity Declaration Pattern**
```prolog
:- self_entity(entity_name).
```

**Loading System**
```prolog
:- load_entity(semantic(folder("path/to/semantics"))).
```

**Semantics + Testing Duality**
- `semantics.pl` - Contains logic and knowledge
- `semantics.plt` - Contains tests for the semantics
- This pattern ensures every semantic domain has corresponding tests

## Spell System

Grimoire uses a magic-inspired command system with two categories:

| Operation Type | Description | Example Commands |
|----------------|-------------|------------------|
| **Conjure (Mutations)** | Require `cast/2` for safety | `conjure(git(commit('message')))` |
| | File operations | `conjure(edit_file(file('readme.txt'), [replace(1, 'New content')]))` |
| | Git operations | `conjure(git(clone('https://github.com/user/repo', './repo')))` |
| | Nix operations | `conjure(nix(run('.')))` |
| | Project creation | `conjure(mkproject)` |
| **Perceive (Queries)** | Direct calls, read-only | `perceive(git(status))` |
| | File reading | `perceive(read_file('file.txt', [1,2,3]))` |
| | System exploration | `perceive(entities)` |
| | Nix queries | `perceive(nix(flake(show)))` |

**Conjure Examples (Mutations)**
```prolog
% Git operations
cast(conjure(git(commit('message'))), Result)
cast(conjure(git(add(all_tracked))), Result)
cast(conjure(git(push(origin, main))), Result)

% Project creation
cast(conjure(mkproject), Result)

% File operations
cast(conjure(mkdir('new-directory')), Result)
cast(conjure(mkfile('filename.txt')), Result)

% Session management
cast(conjure(session(start('session-name'))), Result)
cast(conjure(session(commit)), Result)

% Nix operations
cast(conjure(nix(build('.'))), Result)
cast(conjure(nix(develop)), Result)
```

**Perceive Examples (Queries)**
```prolog
% Git queries
perceive(git(status))
perceive(git(diff))
perceive(git(log))
perceive(git(branch))

% System exploration
perceive(entities)
perceive(read_file)
perceive(search_regex)

% Nix queries
perceive(nix(flake(show)))
perceive(nix(search))

% Session queries
perceive(session(current))
perceive(session(list))
perceive(session(status))
```

## CLI/MCP Usage

**Interface Command Patterns**
All interface operations follow the pattern: `interface(command(...))`

```bash
# List all entities in the system
./grimoire entities

# Show component types for an entity
./grimoire compt system

# List components of specific type
./grimoire comp system subsystem

# Get entity documentation
./grimoire doc git

# Show system status
./grimoire status

# Load entity into session
./grimoire load semantic(folder/file)

# Read file with line numbers
./grimoire read_file "README.md" 1 20

# Session management
./grimoire session start my-session
./grimoire session list
./grimoire session commit
```

**System Exploration Workflow**
1. Start with `./grimoire entities` to see what exists
2. Use `./grimoire compt EntityName` to see component types  
3. Use `./grimoire comp EntityName ctor` to see available operations
4. Use `./grimoire doc EntityName` for detailed documentation

## System Overview

**Core Subsystems**
- `git` - Version control and knowledge evolution tracking
- `nix` - Package management and reproducible environments  
- `fs` - Filesystem operations and pattern matching
- `project` - Project structure and dependency management
- `session` - Transaction and workspace management with SQLite logging
- `agent` - AI-powered task automation and assistance

**Available Templates** (via `nix run .#<template>`)
- `cpp` - C++ projects with CMake
- `haskell` - Haskell projects with Cabal
- `lean4` - Lean 4 theorem proving projects
- `mkdocs` - Documentation sites with MkDocs
- `python` - Python projects with modern tooling
- `python-rest-api` - REST API projects
- `python-bridge-pattern` - Bridge pattern implementations
- `rust` - Rust projects with Cargo
- `database` - Database projects with schema management

**File Structure Conventions**
- `semantics.pl/semantics.plt` - Logic and tests pattern
- `:- self_entity(EntityName).` - Self-entity declarations  
- Single-arity loading API: `load_entity(semantic(folder/file(...)))`
- Explicit entity declarations, no magic binding
- Git-like distributed knowledge patterns

**Session-Based Workflows**
- Each session is a git branch with workspace directory
- SQLite command logging for transaction history
- Atomic commit/rollback operations
- File-based workspaces under `${GRIMOIRE_DATA}/sessions/`

**Documentation Structure**
For deeper technical details:
- `CLAUDE.md` - Core philosophy and development patterns
- `DESIGN.md` - System architecture and ECS design
- `src/*/semantics.pl` - Domain-specific knowledge and APIs
- `src/nix/templates/*/semantics.pl` - Template-specific semantics

**Key Features**
- No runtime asserts - all knowledge persists in files
- Reproducible environments via Nix flakes
- Self-documenting system through `docstring/2` predicates
- Integration of domain tools through common ECS patterns
- Transaction safety through git-backed session management