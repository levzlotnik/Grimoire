# Grimoire

A knowledge-based operating system built on Entity-Component-System (ECS) architecture with a mathematically principled dual kernel: **generative** (expansion) and **discriminative** (verification) flows unified through composable predicates.

## Core Architecture

Grimoire operates on a fundamental **generative-discriminative duality**:

```
ecs_kernel.pl (Generative)  ⊣  ecs_kernel.plt (Discriminative)
     ↓                              ↓
semantics.pl (Expansion)     ⊣  semantics.plt (Verification)
```

### The Dual Pattern

Every domain follows this exact pattern:

- **`semantics.pl`** (Generative): Entity declarations, component expansion rules, spell implementations
- **`semantics.plt`** (Discriminative): Verification predicates via `verify/1`, PLUnit tests

**Example - Git Domain:**

```prolog
% src/git.pl - GENERATIVE FLOW
:- self_entity(git).

% Component expansion: has(git(...)) → rich properties
component(Entity, git_repository_remote_url, RemoteURL) :-
    component(Entity, has(git(repository)), git(repository(Spec))),
    member(remote(_, RemoteURL), Spec).

% src/git.plt - DISCRIMINATIVE FLOW
% Verify composite fact by composing primitive verifications
verify(component(Entity, has(git(repository)), git(repository(Spec)))) :-
    please_verify(component(Entity, git_repository_remote_url, URL)),
    please_verify(component(Entity, git_repository_branch, Branch)),
    % Then check OS reality
    verify_git_repository_exists(URL, Branch).
```

### Critical Distinctions

#### `cast` vs `magic_cast`

- **`cast/2`** - Spell **IMPLEMENTATION** (appears in HEAD of clauses)
  ```prolog
  % ✅ CORRECT - declaring a spell
  cast(conjure(git(commit(Message))), Result) :- ...
  ```

- **`magic_cast/2`** - Spell **INVOCATION** (appears in BODY of clauses)
  ```prolog
  % ✅ CORRECT - invoking a spell
  do_something(Result) :-
      magic_cast(conjure(git(commit("message"))), Result).

  % ❌ WRONG - bypasses hooks and grounding
  do_something(Result) :-
      cast(conjure(git(commit("message"))), Result).
  ```

**Why**: `magic_cast` ensures terms are grounded (no uninstantiated variables) and executes pre/post hooks (logging, monitoring, session integration).

#### `verify` vs `please_verify`

- **`verify/1`** - Verification **IMPLEMENTATION** (appears in HEAD)
  ```prolog
  % ✅ CORRECT - declaring how to verify
  verify(component(Entity, git_repository_root, Root)) :-
      exists_directory(Root) -> true
      ; throw(verification_error(git, missing_repository(Root))).
  ```

- **`please_verify/1`** - Verification **INVOCATION** (appears in BODY)
  ```prolog
  % ✅ CORRECT - invoking verification
  verify(component(E, has(project(app)), project(app(Spec)))) :-
      please_verify(component(E, project_git_repo, GitRepo)),
      please_verify(component(E, project_nix_flake, NixFlake)),
      verify_combination(GitRepo, NixFlake).

  % ❌ WRONG - doesn't ensure grounding
  verify(component(E, has(project(app)), project(app(Spec)))) :-
      component(E, project_git_repo, GitRepo),  % Not grounded!
      component(E, project_nix_flake, NixFlake).
  ```

**Why**: `please_verify` ensures component exists AND is grounded before verification, enables composable verification chains, and executes verification hooks.

### Spell Registration

**Every spell MUST have `register_spell/4` immediately above its `cast/2` implementation:**

```prolog
% Registration with input/output formats and docstring
register_spell(
    conjure(git(commit)),
    input(git(commit(message('Message')))),
    output(either(
        ok(committed(hash('Hash'))),
        error(commit_failed('Reason'))
    )),
    docstring("Create a git commit with the given message")
).

% Implementation immediately follows
cast(conjure(git(commit(Message))), Result) :-
    % Implementation here
    ...
```

**What `register_spell/4` provides:**
- Automatic component registration: `component(conjure, ctor, git(commit))`
- Input/output format specifications for introspection
- Self-documenting spell system via docstrings
- Type safety and validation

## Installation

### Nix Flakes (Recommended)

```bash
# Clone and enter development environment
git clone <repository-url>
cd Grimoire
nix develop

# Or directly from flake
nix develop github:your-org/grimoire
```

### MCP Server Setup

Add to your MCP configuration (`.mcp.json`):

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

### Basic CLI Commands

```bash
# Interactive REPL with system loaded
./grimoire exec

# Run full test suite
./grimoire test

# Run specific tests
./grimoire test git

# Cast spells via CLI
./grimoire conjure "git(commit('fix: update'))"
./grimoire perceive "git(status)"
```

### Environment Variables

- **`GRIMOIRE_ROOT`** - Project directory (auto-detected from script location)
- **`GRIMOIRE_DATA`** - Data storage directory (defaults to `$HOME/.grimoire`)

## Spell System

Grimoire provides two spell categories:

### Conjure (Mutations)

State-changing operations, always invoked via `magic_cast`:

```prolog
% Project creation
magic_cast(conjure(mkproject('/projects', 'myapp', [template(rust)])), Result).

% Git operations
magic_cast(conjure(git(commit("Initial commit"))), Result).
magic_cast(conjure(git(push(origin, main))), Result).

% File operations
magic_cast(conjure(fs(mkfile(path('/tmp/test.txt'), content("Hello")))), Result).
```

### Perceive (Queries)

Read-only operations for querying system state:

```prolog
% Git status
magic_cast(perceive(git(status)), Result).

% List entities
magic_cast(perceive(entities), Result).

% File reading
magic_cast(perceive(fs(read_file(path('file.txt')))), Result).
```

## Core Domains

### Level 2: System Domains

- **`git`** - Version control with structured repository modeling
- **`nix`** - Package management and reproducible builds via flakes
- **`fs`** - Filesystem operations with pattern matching
- **`db`** - SQLite integration with automatic schema discovery
- **`utils`** - Tree building and collection utilities

### Level 3: Coordination Domains

- **`project`** - Project structure composing git+nix+fs patterns
- **`interface`** - Multi-frontend access (CLI/HTTP/MCP)
- **`protocol_clients`** - External service consumption (HTTP/MCP clients)

### Level 5: External Interfaces

- **`golems`** - AI agents with PydanticAI integration

## ECS Patterns

### Entity Declaration

```prolog
:- self_entity(my_domain).  % Auto-creates entity and self component

entity(my_thing).  % Explicit entity declaration
```

### Component Expansion Pattern

```prolog
% User declares high-level fact
component(my_project, has(git(repository)), git(repository([
    remote(origin, 'https://github.com/user/repo'),
    branch(main)
]))).

% Domain expands into queryable properties
component(Entity, git_repository_remote_url, URL) :-
    component(Entity, has(git(repository)), git(repository(Spec))),
    member(remote(_, URL), Spec).
```

### Verification Pattern

```prolog
% Verify composite by composing primitives
verify(component(Entity, has(git(repository)), git(repository(Spec)))) :-
    please_verify(component(Entity, git_repository_remote_url, URL)),
    please_verify(component(Entity, git_repository_branch, Branch)),
    % Check OS reality
    verify_git_accessible(URL, Branch).

% Verify primitive property against OS reality
verify(component(Entity, git_repository_root, Root)) :-
    % Component existence already proven by please_verify
    exists_directory(Root) -> true
    ; throw(verification_error(git, missing_repository(Root))).
```

## Testing Patterns

### File-Based Test Entities

**CRITICAL**: Tests NEVER use `assertz`/`retract` for component facts. All test knowledge lives in `.pl` files:

```prolog
% src/tests/git_test_entity.pl
:- self_entity(test_entities(git)).

entity(test_basic_git).

component(test_basic_git, has(git(repository)), git(repository([
    root('/tmp/test_repo'),
    branch(main)
]))).
```

### Test Pattern

```prolog
% src/git.plt
:- begin_tests(git_semantics).

% Test component verification
test(git_repository_verification, [
    setup(setup_test_repo),
    cleanup(cleanup_test_repo)
]) :-
    % Use user: prefix in tests
    user:please_verify(component(test_basic_git, has(git(repository)), _)),
    user:please_verify(component(test_basic_git, git_repository_root, Root)),
    assertion(Root = '/tmp/test_repo').

% Test spell invocation
test(git_commit_spell) :-
    % Use user:magic_cast in tests
    user:magic_cast(conjure(git(commit("test message"))), Result),
    assertion(Result = ok(_)).

:- end_tests(git_semantics).
```

### Setup/Cleanup Rules

- ✅ Create/delete directories and files
- ✅ Run `process_create` for git init, sqlite3, etc.
- ❌ NEVER `assertz(component(...))`
- ❌ NEVER `retract(entity(...))`
- ❌ NEVER modify ECS state at runtime in tests

## Python Bridge Pattern

For domains integrating with Python (golems, protocol_clients):

**Structure:**
```
domain/
├── semantics.pl        # Pure Prolog (NO py_call)
├── python_bridge.pl    # ALL py_call + decoding
├── client.py          # Python implementation
└── semantics.plt      # Tests
```

**Critical**: Python objects NEVER appear in `semantics.pl`. The `python_bridge.pl` decodes everything to pure Prolog terms.

```prolog
% semantics.pl - PURE PROLOG ONLY
:- use_module('python_bridge.pl', [python_function/2]).

cast(conjure(domain(operation(Args))), Result) :-
    python_function(Args, DecodedResult),  % Already Prolog!
    Result = ok(DecodedResult).

% python_bridge.pl - ALL py_call + DECODING
python_function(Args, DecodedResult) :-
    py_call(module:function(Args), PyResult),
    decode_result(PyResult, DecodedResult).  % Convert to Prolog
```

## System Architecture Benefits

### Mathematical Correctness

The generative-discriminative duality ensures:
- Everything generated must be verifiable
- Everything verified must have been generated
- No inconsistent knowledge can exist

### Composability

`please_verify/1` enables clean composition:
- Domains verify sub-components recursively
- Cross-domain dependencies work naturally
- Verification is complete and composable

### Simplicity

Users only need to:
- Declare facts in `semantics.pl`
- Declare `verify/1` predicates in `semantics.plt` that check OS reality
- System handles expansion and verification automatically

## Documentation

For deeper technical details:
- **`CLAUDE.md`** - Development patterns and conventions
- **`DESIGN.md`** - System architecture deep dive
- **`overhaul-plan.md`** - Complete architectural specification
- **`review_instructions.md`** - Domain review checklist

## Key Principles

1. **No runtime asserts** - All knowledge lives in files
2. **Dual kernel** - Generative and discriminative flows are complementary
3. **Composable verification** - Use `please_verify` to compose checks
4. **Spell registration** - Every `cast` has `register_spell/4` above it
5. **Grounding** - `magic_cast` and `please_verify` ensure grounded terms
6. **File-based tests** - No `assertz`/`retract` in tests, only filesystem setup
7. **Python isolation** - Python objects never leak into `semantics.pl`

## License

[Your License Here]
