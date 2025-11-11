# Grimoire

A knowledge-based operating system built on declarative Prolog. Declare what you want, Grimoire ensures it's real.

## Philosophy

Traditional scripts fail silently. Configuration drift happens. State becomes inconsistent.

Grimoire treats **everything as verifiable knowledge**:

```prolog
% Declare what you want
component(my_project, has(git(repository)), git(repository([
    root('/home/user/my_project'),
    branch(main),
    remote(origin, 'https://github.com/user/repo')
]))).
```

Grimoire automatically:
1. **Expands** declarations into queryable properties
2. **Verifies** they match OS reality
3. **Provides spells** to manipulate them

All knowledge lives in files. No runtime assertions. Fully reproducible.

## Quick Start

### Installation

**Nix (Recommended):**
```bash
git clone https://github.com/levzlotnik/Grimoire
cd Grimoire
nix develop
```

### Run Tests

```bash
./grimoire test
```

All tests should pass.

## Core Concepts

### Entity-Component-System (ECS)

Everything is an **entity** with typed **components**:

```prolog
entity(my_project).

component(my_project, git_repository_root, '/home/user/my_project').
component(my_project, git_repository_branch, main).
component(my_project, project_name, 'MyProject').
```

### Declarative Expansion (`==>`)

High-level declarations expand to queryable properties:

```prolog
% You write this
component(E, has(git(repository)), git(repository(Spec)))
    ==> (component(E, git_repository_branch, Branch) :-
            member(branch(Branch), Spec)).

% System auto-generates expansion rules
```

### Verification (`::`)

Components are verified against OS reality:

```prolog
% Declare verification constraints
component(_E, git_repository_root, Root)
    :: exists_directory(Root).
```

The `::` operator auto-generates verification code at compile time.

### Spells: Type-Safe Operations

Spells manipulate system state:

```prolog
% Conjure (state-changing)
magic_cast(conjure(git(commit(git_root("/path"), message("Fix")))), Result).
% Result = ok(committed(hash("abc123")))

% Perceive (read-only)
magic_cast(perceive(git(status(git_root("/path")))), Result).
% Result = ok(status_info(branch(main), working_status(clean), files([])))
```

## Building and Testing

### Build

```bash
# Enter development environment
nix develop

# Verify installation
./grimoire test
```

### Run Tests

```bash
# All tests
./grimoire test

# Specific domain
./grimoire test git

# Specific test pattern
./grimoire test 'commit'

# List available tests
./grimoire test --list
```

## Using Grimoire

### CLI

**List entities:**

```bash
$ ./grimoire entities
```

Output (showing first 20 of 100+ entities):
```
component
ctor
fact_schema
option
system
source
semantic
project
spell
conjure
perceive
git
git(clone)
git(init)
git(add)
git(commit)
git(push)
git(pull)
git(checkout)
git(reset)
...
```

**List component types for an entity:**

```bash
$ ./grimoire compt git
```

Output:
```
ctor
defined
docstring
self
subcommand
```

**List components of a specific type:**

```bash
$ ./grimoire comp git ctor
```

Output:
```
diff
log
branch
rev_parse
ls_files
status
current_branch
clone
init
add
commit
push
pull
checkout
reset
merge
config
remote
```

**Show documentation:**

```bash
$ ./grimoire doc git
```

Output:
```
Knowledge evolution tracking and version control subsystem.
Manages version history, branching, and transactional rollback through git.
Provides session management where each session is a git branch,
enabling atomic changes and experimental workflows with easy rollback.
Core to Grimoire's immutable knowledge architecture.
```

**List available spells:**

```bash
$ ./grimoire conjure --list
```

Output (partial):
```
Available conjure spells:
  executable_program
  git(clone)
  git(init)
  git(add)
  git(commit)
  git(push)
  ...
```

**Cast conjure spell:**

```bash
$ ./grimoire conjure "git(commit(git_root(Root), message(Message)))" \
    --Root="/path/to/repo" \
    --Message="Update files"
```

Output:
```
ok(committed(hash("a1b2c3d4")))
```

**Execute perceive query:**

```bash
$ ./grimoire perceive "git(status(git_root(Root)))" \
    --Root="/path/to/repo"
```

Output:
```
ok(status_info(
    branch(main),
    working_status(clean),
    files([])
))
```

**Get help for a spell:**

```bash
$ ./grimoire doc "conjure(git(commit))"
```

Output:
```
# Spell: `conjure(git(commit))`

Create a git commit with the given message

Input Format: git(commit(git_root('Root'), message('Message')))
Output Format: either(ok(committed(hash('Hash'))), error(git_error('Reason')))
```

**Query components (context matters):**

Without any component context:
```bash
$ ./grimoire exec "component(git, ctor, X)"
```

Output:
```
X = diff ;
X = log ;
X = branch ;
X = rev_parse ;
X = ls_files ;
X = status ;
X = current_branch ;
X = clone ;
X = init ;
X = add ;
X = commit ;
X = push ;
X = pull ;
X = checkout ;
X = reset ;
X = merge ;
X = config ;
X = remote.
```

With component context (e.g., after declaring a project entity):
```bash
# First, components are declared in a .pl file or session
$ ./grimoire exec "component(my_project, git_repository_branch, Branch)"
```

Output:
```
Branch = main.
```

The query result depends on what component facts exist in the loaded knowledge base.

### MCP Server

Grimoire can run as an MCP (Model Context Protocol) server for AI assistants.

**Claude Desktop Configuration:**

Add to `~/.config/claude/claude_desktop_config.json`:

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

**Available MCP Tools:**
- `entities()` - List all entities
- `component_types(entity)` - List component types
- `components(entity, type)` - Get components
- `docstring(entity)` - Get documentation
- `list_spells()` - List available spells
- `conjure(spell_sig, args)` - Execute conjure spell
- `perceive(query_sig, args)` - Execute perceive query
- `test(args)` - Run tests
- `prove_it(entity, type, value)` - Component provenance
- `sauce_me(spell_ctor)` - Spell metadata
- `exec_query(query_str)` - Execute arbitrary Prolog query

### HTTP API

Start the REST API server:

```bash
./grimoire serve --host 0.0.0.0 --port 8000
```

**Endpoints:**

```bash
# List entities
curl http://localhost:8000/entities

# Get component types
curl http://localhost:8000/component_types/git

# Get components
curl http://localhost:8000/components/git/ctor

# Get docstring
curl http://localhost:8000/docstring/git

# List spells
curl http://localhost:8000/spells

# Execute perceive query
curl -X POST http://localhost:8000/perceive \
  -H "Content-Type: application/json" \
  -d '{"query_sig": "git(status(git_root(Root)))", "args": {"Root": "/path"}}'

# Execute conjure spell
curl -X POST http://localhost:8000/conjure \
  -H "Content-Type: application/json" \
  -d '{"spell_sig": "git(commit(git_root(Root), message(Message)))", "args": {"Root": "/path", "Message": "Update"}}'

# Health check
curl http://localhost:8000/health
```

## Available Domains

- **git** - Repository management (commit, push, branch, remote)
- **fs** - Filesystem operations (read, write, edit, mkdir)
- **db** - SQLite integration with schema validation
- **nix** - Nix flake and derivation management
- **utils** - Common utilities (validation, transformations)
- **project** - Multi-domain composition (git + nix + fs)
- **interface** - CLI, HTTP, and MCP interfaces
- **session** - Session state management and persistence
- **golems** - AI agent workflows with PydanticAI
- **protocol_clients** - External service clients (HTTP, MCP)

## Examples

### Git Workflow

```prolog
% Clone repository
magic_cast(conjure(git(clone(
    url('https://github.com/user/repo'),
    path('/tmp/repo')
))), R1).

% Stage and commit
magic_cast(conjure(git(add(
    git_root("/tmp/repo"),
    paths([file1, file2])
))), R1),
magic_cast(conjure(git(commit(
    git_root("/tmp/repo"),
    message("Update files")
))), R2).

% Check status
magic_cast(perceive(git(status(git_root("/tmp/repo")))), Result).
```

### Database Operations

```prolog
% Create database with schema
magic_cast(conjure(db(create(
    file('/tmp/mydb.db'),
    schema(file('schema.sql'))
))), Result).

% Execute query
magic_cast(perceive(db(query(
    database('/tmp/mydb.db'),
    sql("SELECT * FROM users WHERE age > 18")
))), Result).
```

### Filesystem Operations

```prolog
% Read file
magic_cast(perceive(fs(read_file(
    path('/tmp/config.txt'),
    start(1),
    end(10)
))), Result).

% Edit file
magic_cast(conjure(fs(edit_file(
    file('/tmp/file.txt'),
    edits([insert(line(1), content("New line"))])
))), Result).

% Create directory
magic_cast(conjure(fs(mkdir(
    path('/tmp/project'),
    options([git(auto)])
))), Result).
```

## Debugging and Development

### Core Dumps

Emit entire system state for debugging:

```prolog
core_dump(grimoire).
```

Exports all entities, components, and relationships.

### Session Management

Sessions track spell execution history:

```bash
# Create session
./grimoire session create my_session

# Switch to session
./grimoire session switch my_session

# Export session
./grimoire session export my_session /tmp/sessions/

# Import session
./grimoire session import /tmp/sessions/my_session.tar.gz

# Delete session
./grimoire session delete my_session
```

Session state includes:
- Executed spells
- Component changes
- Transaction history

### Development Patterns

**Pattern 1: Component Expansion (`==>`)**

```prolog
component(Entity, has(domain(concept)), domain(concept(Spec)))
    ==> component(Entity, domain_property, Value) :-
            member(prop(Value), Spec).
```

**Pattern 2: Verification (`::`)**

```prolog
component(_E, domain_property, Value)
    :: atom(Value),
       validate_value(Value).
```

**Pattern 3: Spell Registration**

```prolog
register_spell(
    conjure(domain(action)),
    input(domain(action(arg('Arg')))),
    output(either(ok(result('Result')), error(failed('Reason')))),
    "Perform domain action",
    [],
    implementation(conjure(domain(action(Arg))), Result, (
        validate_arg(Arg),
        perform_action(Arg, Output),
        Result = ok(result(Output))
    ))
).
```

**Never write manually:**
- Verification clauses (use `::` operator)
- Spell implementation wrappers (use `register_spell`)
- Runtime assertions/retracts (use file-based entities)

### Testing

Tests use file-based entities and PLUnit:

```prolog
% src/tests/domain_test_entity.pl
:- self_entity(test_entities(domain)).

entity(test_entity).

component(test_entity, has(domain(fact)), domain(fact([prop(value)]))).

% src/domain/semantics.plt
:- load_entity(semantic(file('@/src/tests/domain_test_entity.pl'))).

:- begin_tests(domain_semantics).

test(expansion) :-
    user:please_verify(component(test_entity, has(domain(fact)), _)),
    user:please_verify(component(test_entity, domain_property, _)).

test(spell) :-
    user:magic_cast(conjure(domain(action(arg))), Result),
    assertion(Result = ok(_)).

:- end_tests(domain_semantics).
```

**Key principles:**
- All test entities declared in `.pl` files
- Setup/cleanup manipulate filesystem only
- Tests use `user:please_verify` and `user:magic_cast`
- No runtime assertions/retracts

## Documentation

- **README.md** (this file) - User guide and quick reference
- **docs/ecs.md** - Entity-Component-System architecture
- **docs/spells.md** - Spell system deep-dive
- **docs/session.md** - Session domain and persistence
- **docs/testing.md** - Testing patterns and practices
- **docs/philosophy.md** - System philosophy and design principles
- **CLAUDE.md** - Development patterns for contributors
- **src/GRIMOIRE.md** - LLM system instructions (MCP/API users)

## Contributing

See `CLAUDE.md` for development patterns and conventions.

Key rules:
- Use `::` operator for verification (never write verification clauses manually)
- Use `register_spell` for spells (never write implementation wrappers manually)
- All test entities in files (no runtime assertions)
- Use `please_verify` for verification, `magic_cast` for spells

## License

AGPL

## Developer Command

For interactive Prolog queries and advanced debugging:

```bash
./grimoire exec
```

Example queries (note: no trailing period in query strings):

```prolog
entity(E)
component(git, ctor, X)
component(Entity, ComponentType, Value)
please_verify(component(my_project, git_repository_root, Root))
magic_cast(perceive(git(status(git_root("/path")))), Result)
```

This opens an interactive Prolog REPL with the full Grimoire knowledge base loaded.
