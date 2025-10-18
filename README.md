# Grimoire

A knowledge-based operating system that lets you declare what you want and verifies it works.

## Philosophy

Grimoire operates on a simple principle: **declare what you want, Grimoire ensures it's real**.

Instead of writing imperative scripts that might fail silently, you declare facts about your system:
```prolog
component(my_project, has(git(repository)), git(repository([
    remote(origin, 'https://github.com/user/repo'),
    branch(main)
]))).
```

Grimoire automatically:
1. **Expands** your declarations into queryable properties
2. **Verifies** they match reality (the git repo actually exists and is accessible)
3. **Provides spells** to manipulate them (commit, push, deploy)

## Installation

### Nix Flakes (Recommended)

```bash
git clone <repository-url>
cd Grimoire
nix develop
```

Or directly from the flake:
```bash
nix develop github:levzlotnik/Grimoire
```

### Docker

**Docker-based installation coming soon.**

## Usage

### Interactive Mode

```bash
./grimoire exec
```

Enters an interactive Prolog REPL where you can:
- Query the knowledge base: `component(Entity, Type, Value).`
- Cast spells: `magic_cast(conjure(git(commit("message"))), Result).`
- Explore entities: `entity(X).`

### Spell System

Grimoire provides **spells** - validated, composable operations:

```prolog
% Cast a spell (type-checked, verified, logged)
?- magic_cast(conjure(git(commit("Fix bug"))), Result).
Result = ok(committed(hash("abc123"))).

% Spells compose
?- magic_cast(conjure(git(commit("Feature"))), R1),
   magic_cast(conjure(git(push(origin, main))), R2).
```

**Spell Types:**
- `conjure(...)` - Creates/modifies state (git commit, database insert, file write)
- `perceive(...)` - Queries state (git status, database query, file read)

### Testing

```bash
# Run all tests
./grimoire test

# Run specific domain tests
./grimoire test git

# Run by test name pattern
./grimoire test 'git_commit'
```

### MCP Server Integration

Grimoire can run as an MCP (Model Context Protocol) server for AI assistants.

Add to your MCP configuration (`.mcp.json` or `claude_desktop_config.json`):

```json
{
  "mcpServers": {
    "Grimoire": {
      "type": "stdio",
      "command": "/path/to/grimoire/grimoire",
      "args": ["mcp"],
      "env": {
        "GRIMOIRE_ROOT": "/path/to/grimoire"
      }
    }
  }
}
```

The AI can then:
- Query your project structure
- Execute validated operations
- Compose complex workflows
- All with verification guarantees

## Core Concepts

### Entities and Components

Everything in Grimoire is an **entity** with **components**:

```prolog
% Query what entities exist
?- entity(E).
E = git ;
E = fs ;
E = my_project.

% Query components of an entity
?- component(my_project, ComponentType, Value).
ComponentType = git_repository_root,
Value = '/home/user/my_project' ;
ComponentType = git_repository_branch,
Value = main.
```

### Declarative Specification

Declare high-level facts with `has(domain(...))`:

```prolog
component(my_app, has(git(repository)), git(repository([
    root('/home/user/my_app'),
    remote(origin, 'https://github.com/user/my_app'),
    branch(main)
]))).

component(my_app, has(nix(flake)), nix(flake([
    file('/home/user/my_app/flake.nix')
]))).
```

Grimoire automatically expands these into queryable properties and verifies they match reality.

### Verification

Everything gets verified:

```prolog
% Verify a component exists and matches reality
?- please_verify(component(my_app, git_repository_root, Root)).
Root = '/home/user/my_app'.  % Succeeds only if directory exists and is a git repo

% Throws if verification fails
?- please_verify(component(fake_project, git_repository_root, _)).
ERROR: Component verification failed: git repository not found
```

## Available Domains

- **git** - Repository management, commits, branches, remotes
- **fs** - File system operations with atomic guarantees
- **db** - SQLite database management with schema validation
- **nix** - Nix flake and derivation management
- **utils** - Common utilities (validation, transformation, hierarchy building)
- **project** - Multi-domain project composition (git + nix + db)
- **session** - Session state management and persistence
- **interface** - CLI and API interfaces
- **golems** - AI agent workflows
- **protocol_clients** - MCP, HTTP, and other protocol handlers

## Examples

### Git Workflow
```prolog
% Stage and commit
?- magic_cast(conjure(git(add([file1, file2]))), R1),
   magic_cast(conjure(git(commit("Update files"))), R2).

% Create and push branch
?- magic_cast(conjure(git(branch(create(feature)))), R1),
   magic_cast(conjure(git(checkout(feature))), R2),
   magic_cast(conjure(git(push(origin, feature))), R3).
```

### Database Operations
```prolog
% Create database with schema
?- magic_cast(conjure(db(create(
    database_id(mydb),
    file('/tmp/mydb.db'),
    schema(file('schema.sql'))
))), Result).

% Query
?- magic_cast(perceive(db(query(
    database_id(mydb),
    sql("SELECT * FROM users WHERE age > 18")
))), Result).
```

### File System
```prolog
% Atomic file write
?- magic_cast(conjure(fs(write(
    path('/tmp/config.json'),
    content('{"key": "value"}')
))), Result).

% Directory operations
?- magic_cast(conjure(fs(mkdir(
    path('/tmp/project'),
    recursive(true)
))), Result).
```

## Development

See `CLAUDE.md` for development patterns and conventions.

## License

[Your License Here]
