# Grimoire System Design

## Executive Summary

Grimoire is a knowledge-based operating system built on Entity-Component-System (ECS) architecture with a mathematically principled **dual kernel**: a generative flow (expansion) and a discriminative flow (verification), unified through composable predicates. The system treats semantic knowledge as first-class entities, stored declaratively in files, with verification against operating system reality.

**Current Status**: v0.1.0 - Production-ready core with all subsystems operational. The system passes comprehensive test suites covering ECS semantics, spell execution, domain integrations, and multi-frontend interfaces.

---

## Part 1: Core Architecture

### The Fundamental Duality

Grimoire operates on a **generative-discriminative duality**, a pattern inspired by machine learning where:

- **Generative Flow** (Covariant): Expands high-level facts into rich component graphs
- **Discriminative Flow** (Contravariant): Verifies components exist and match OS reality

```
ecs_kernel.pl (Generative Kernel)  ⊣  ecs_kernel.plt (Discriminative Kernel)
            ↓                                    ↓
    semantics.pl (Expansion)            ⊣   semantics.plt (Verification)
```

This duality ensures:
- Everything generated must be verifiable
- Everything verified must have been generated
- No inconsistent knowledge can exist in the system

### The ECS Kernel

#### `ecs_kernel.pl` - Generative Kernel

The generative kernel provides the foundational predicates:

```prolog
% Core ECS predicates
:- multifile entity/1.
:- multifile component/3.
:- multifile docstring/2.

% Self-entity pattern for automatic entity detection
:- multifile self_entity/1.

% Component integration for baseline predicates
component(Entity, docstring, Doc) :-
    docstring(Entity, Doc).

component(Entity, defined, true) :-
    entity(Entity).

% Entity registry
component(entity_registry, entity, Entity) :-
    entity(Entity).
```

**Loading Mechanism:**

```prolog
% Single-arity loading API
load_entity(semantic(folder(Path))) :- ...
load_entity(semantic(file(Path))) :- ...

% Self-entity auto-detection
% Detects semantic source from prolog_load_context/2
% Creates component(Entity, self, semantic(file/folder(...))) automatically
```

#### `ecs_kernel.plt` - Discriminative Kernel

The discriminative kernel provides composable verification through `please_verify/1`:

```prolog
% The composable verification primitive with hooks and grounding
please_verify(component(A, B, C)) :-
    component(A, B, C),                    % Component exists (may unify vars)
    ground(component(A, B, C)),            % NOW ground after unification
    verify_pre_hooks(component(A, B, C)),
    verify(component(A, B, C)), !,         % Try domain verification
    verify_post_hooks(component(A, B, C)).

please_verify(component(A, B, C)) :-
    component(A, B, C),                    % Base case - existence is enough
    ground(component(A, B, C)).            % Ground after unification

please_verify(component(A, B, C)) :-
    \+ component(A, B, C),                 % Component doesn't exist
    format(string(E), "Component not found: ~w", [component(A, B, C)]),
    throw(verification_error(missing_component, E)).
```

**Key Properties:**

1. **Free Existence Proofs**: When `verify/1` is called, `component(A, B, C)` is already proven
2. **Grounding Requirement**: All terms must be ground (no uninstantiated variables)
3. **Hook Infrastructure**: Extensible behavior through pre/post hooks
4. **Composability**: `please_verify` calls can be nested and composed

---

## Part 2: Domain Pattern

Every domain in Grimoire follows an identical pattern, creating consistency across the system.

### Domain Generative Flow (`semantics.pl`)

**Structure:**
```prolog
:- self_entity(domain).

% Entity declarations for domain concepts
entity(domain(concept1)).
entity(domain(concept2)).

% Documentation
docstring(domain, "Domain description").

% Component expansion rules: has(domain(...)) → rich components
component(Entity, domain_prop1, Value) :-
    component(Entity, has(domain(fact)), domain(fact(Spec))),
    member(prop1(Value), Spec).

component(Entity, domain_prop2, Value) :-
    component(Entity, has(domain(fact)), domain(fact(Spec))),
    derive_prop2(Spec, Value).
```

**The `has(domain(...))` Pattern:**

Users declare high-level facts using `has(domain(...))`:

```prolog
% User declares this
component(my_project, has(git(repository)), git(repository([
    remote(origin, 'https://github.com/user/repo'),
    branch(main),
    root('/path/to/repo')
]))).
```

The domain **expands** this into queryable properties:

```prolog
% Domain provides these expansions
component(Entity, git_repository_remote_name, origin) :-
    component(Entity, has(git(repository)), git(repository(Spec))),
    member(remote(origin, _), Spec).

component(Entity, git_repository_remote_url, URL) :-
    component(Entity, has(git(repository)), git(repository(Spec))),
    member(remote(_, URL), Spec).

component(Entity, git_repository_branch, Branch) :-
    component(Entity, has(git(repository)), git(repository(Spec))),
    member(branch(Branch), Spec).

component(Entity, git_repository_root, Root) :-
    component(Entity, has(git(repository)), git(repository(Spec))),
    member(root(Root), Spec).
```

### Domain Discriminative Flow (`semantics.plt`)

**Structure:**
```prolog
:- use_module(library(plunit)).

% Load test entities from file-based knowledge
:- load_entity(semantic(file('@/src/tests/domain_test_entities.pl'))).

% High-level fact verification (composite)
verify(component(Entity, has(domain(fact)), domain(fact(Spec)))) :-
    % component(...) already proven to exist via please_verify!
    % Compose primitive verifications
    please_verify(component(Entity, domain_prop1, DP1)),
    please_verify(component(Entity, domain_prop2, DP2)),
    % Then verify the combination against OS reality
    verify_os_reality_combination(DP1, DP2).

% Primitive verifications (check OS reality)
verify(component(Entity, domain_prop1, Value)) :-
    % Already proven: component(Entity, domain_prop1, Value)
    % Just check OS reality
    check_os_reality_prop1(Value) -> true
    ; throw(verification_error(domain, failed_prop1(Value))).

% PLUnit tests
:- begin_tests(domain_semantics).

test(domain_component_verification) :-
    % Use user: prefix in tests
    user:please_verify(component(domain_entity, has(domain(fact)), _)),
    user:please_verify(component(domain_entity, domain_prop1, _)).

test(domain_spell) :-
    user:magic_cast(conjure(domain(operation(args))), Result),
    assertion(Result = ok(_)).

:- end_tests(domain_semantics).
```

**Verification Layers:**

1. **Composite Verification**: Verifies `has(domain(...))` by composing primitive verifications
2. **Primitive Verification**: Each expanded property verified against OS reality
3. **PLUnit Tests**: Comprehensive test coverage using `user:please_verify` and `user:magic_cast`

---

## Part 3: The Spell System

### Spell Categories

Grimoire provides two spell categories inspired by fantasy magic systems:

- **`conjure`** - State-changing operations (mutations)
- **`perceive`** - Read-only operations (queries)

### Spell Declaration and Invocation

#### **`cast/2`** - Spell Implementation (HEAD)

```prolog
% Declaring a spell implementation
cast(conjure(git(commit(Message))), Result) :-
    % Implementation logic
    ...
```

#### **`magic_cast/2`** - Spell Invocation (BODY)

```prolog
% Invoking a spell (correct)
do_task(Result) :-
    magic_cast(conjure(git(commit("message"))), Result).

% Invoking a spell (WRONG - bypasses hooks)
do_task(Result) :-
    cast(conjure(git(commit("message"))), Result).  % ❌ WRONG!
```

**Why `magic_cast`?**

```prolog
magic_cast(Term, Result, CastPreOuts, CastPostOuts) :-
    ground(Term),  % Ensures all variables instantiated
    cast_pre_hooks(Term, CastPreOuts),
    catch(
        cast(Term, Result),
        E,
        Result = error(cast_failed(E))
    ),
    cast_post_hooks(Term, CastPostOuts).
```

**Benefits:**
- **Grounding Check**: Ensures no uninstantiated variables
- **Pre/Post Hooks**: Session logging, monitoring, validation
- **Error Handling**: Standardized error propagation
- **Atomic Operations**: Integration with transaction system

### Spell Registration - `register_spell/4`

**Every spell MUST be registered immediately above its implementation:**

```prolog
% Registration with format specification and docstring
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
    phrase(git_args(commit(Message)), Args),
    magic_cast(conjure(executable_program(git, Args)), Result).
```

**Auto-Generated Components:**

```prolog
% From register_spell/4, system auto-generates:
component(conjure, ctor, git(commit)).
component(git(commit), format_input, git(commit(message('Message')))).
component(git(commit), format_output, either(ok(committed(hash('Hash'))), error(commit_failed('Reason')))).
component(git(commit), docstring, "Create a git commit with the given message").
```

**Benefits:**
- Self-documenting spell system
- Type safety through format specifications
- Introspection capabilities for tooling
- Automatic constructor registration

---

## Part 4: Composable Verification

### The `please_verify/1` Primitive

`please_verify/1` is the cornerstone of Grimoire's verification system:

```prolog
% Three cases for please_verify:

% Case 1: Component exists, is grounded, verify/1 exists and succeeds
please_verify(component(A, B, C)) :-
    component(A, B, C),                    % May unify variables
    ground(component(A, B, C)),            % NOW ground
    verify_pre_hooks(component(A, B, C)),
    verify(component(A, B, C)), !,         % Domain verification
    verify_post_hooks(component(A, B, C)).

% Case 2: Component exists, is grounded, no verify/1 needed
please_verify(component(A, B, C)) :-
    component(A, B, C),
    ground(component(A, B, C)).

% Case 3: Component doesn't exist - error
please_verify(component(A, B, C)) :-
    \+ component(A, B, C),
    throw(verification_error(missing_component, component(A, B, C))).
```

### Verification Composition Pattern

**Example - Git Domain:**

```prolog
% Composite verification using please_verify
verify(component(Entity, has(git(repository)), git(repository(Spec)))) :-
    % Component existence already proven by please_verify
    % Verify all expanded primitive components
    please_verify(component(Entity, git_repository_remote_url, URL)),
    please_verify(component(Entity, git_repository_branch, Branch)),
    please_verify(component(Entity, git_repository_root, Root)),
    % Then check OS reality
    exists_directory(Root),
    git_repository_accessible(URL).

% Primitive verification against OS reality
verify(component(Entity, git_repository_root, Root)) :-
    % Component existence proven - just check OS
    exists_directory(Root) -> true
    ; throw(verification_error(git, missing_repository(Root))).

verify(component(Entity, git_repository_remote_url, URL)) :-
    % Validate URL format against OS reality
    atom_string(URL, URLStr),
    valid_git_url(URLStr) -> true
    ; throw(verification_error(git, invalid_url(URL))).
```

**Pattern:**
1. Composite `verify/1` uses `please_verify` to check all sub-components
2. After all sub-components verified, check their combination
3. Each primitive component has its own `verify/1` checking OS reality

### Cross-Domain Verification

Domains can compose verifications from other domains:

```prolog
% Project domain composes git + nix verifications
verify(component(Entity, has(project(app)), project(app(Spec)))) :-
    please_verify(component(Entity, has(git(repository)), _)),
    please_verify(component(Entity, has(nix(flake)), _)),
    please_verify(component(Entity, project_source_dir, SourceDir)),
    % Verify combination
    exists_directory(SourceDir),
    directory_has_source_files(SourceDir).
```

---

## Part 5: Testing Architecture

### File-Based Test Entities

**CRITICAL PRINCIPLE: NO `assertz`/`retract` in tests.**

All test knowledge lives in declarative `.pl` files:

```prolog
% src/tests/git_test_entity.pl
:- self_entity(test_entities(git)).

entity(test_basic_git).
entity(test_advanced_git).

% Static test components - declared as facts
component(test_basic_git, has(git(repository)), git(repository([
    root('/tmp/test_repo'),
    branch(main),
    remote(origin, 'https://test.example.com/repo.git')
]))).

component(test_basic_git, test_path, '/tmp/test_repo').

% Advanced test entity
component(test_advanced_git, has(git(repository)), git(repository([
    root('/tmp/advanced_repo'),
    branch(development),
    clean(false)
]))).

docstring(test_entities(git), "Test entities for git domain").
```

### Test Implementation Pattern

```prolog
% src/git.plt
:- use_module(library(plunit)).
:- load_entity(semantic(file('@/src/tests/git_test_entity.pl'))).

:- begin_tests(git_semantics).

% Test component expansion and verification
test(git_repository_expansion, [
    setup(setup_test_repo),
    cleanup(cleanup_test_repo)
]) :-
    % Entities already loaded from file - just verify
    user:please_verify(component(test_basic_git, has(git(repository)), _)),
    user:please_verify(component(test_basic_git, git_repository_root, Root)),
    assertion(Root = '/tmp/test_repo'),
    user:please_verify(component(test_basic_git, git_repository_branch, Branch)),
    assertion(Branch = main).

% Test spell invocation
test(git_commit_spell, [
    setup(setup_test_repo),
    cleanup(cleanup_test_repo)
]) :-
    user:magic_cast(conjure(git(commit("test commit"))), Result),
    assertion(Result = ok(_)).

% Test error conditions
test(git_invalid_repository) :-
    catch(
        user:please_verify(component(nonexistent, git_repository_root, _)),
        verification_error(missing_component, _),
        true
    ).

:- end_tests(git_semantics).

% Setup creates ONLY filesystem resources matching entity declarations
setup_test_repo :-
    TestPath = '/tmp/test_repo',
    (exists_directory(TestPath) ->
        delete_directory_and_contents(TestPath)
    ; true),
    make_directory(TestPath),
    process_create(path(git), ['init'], [cwd(TestPath)]),
    process_create(path(git), ['config', 'user.name', 'Test'], [cwd(TestPath)]),
    process_create(path(git), ['config', 'user.email', 'test@example.com'], [cwd(TestPath)]).

% Cleanup removes ONLY filesystem resources
cleanup_test_repo :-
    TestPath = '/tmp/test_repo',
    (exists_directory(TestPath) ->
        delete_directory_and_contents(TestPath)
    ; true).
```

### Setup/Cleanup Rules

**What Goes in Setup/Cleanup:**
- ✅ Creating/deleting directories
- ✅ Creating/deleting files
- ✅ Running `process_create(path(git), ['init'], ...)`
- ✅ Running `process_create(path(sqlite3), ...)`
- ❌ NEVER `assertz(component(...))`
- ❌ NEVER `retract(entity(...))`
- ❌ NEVER `assertz(entity(...))`

**Rationale:**
- Test entities declared in files are order-independent
- No state pollution between tests
- Declarative test data is visible and debuggable
- Matches production pattern (all knowledge in files)

---

## Part 6: Hook System

### Hook Infrastructure

Grimoire provides extensible hook points for cross-cutting concerns:

```prolog
:- dynamic([
    cast_pre_hook/3,
    cast_post_hook/3,
    verify_pre_hook/2,
    verify_post_hook/2
], [
    discontiguous(true),
    multifile(true)
]).

% Hook collection
cast_pre_hooks(Term, Outs) :-
    findall(Out, (is_hook(Hook), cast_pre_hook(Term, Hook, Out)), Outs).

cast_post_hooks(Term, Outs) :-
    findall(Out, (is_hook(Hook), cast_post_hook(Term, Hook, Out)), Outs).

verify_pre_hooks(Term) :-
    findall(_, (is_hook(Hook), verify_pre_hook(Term, Hook)), _).

verify_post_hooks(Term) :-
    findall(_, (is_hook(Hook), verify_post_hook(Term, Hook)), _).
```

### Hook Applications

**Session Logging:**
```prolog
cast_pre_hook(Term, session_logger, logged(SessionId, Term)) :-
    get_current_session(SessionId),
    log_spell_to_session(SessionId, Term).
```

**Performance Monitoring:**
```prolog
cast_pre_hook(Term, timer, start_time(T)) :-
    get_time(T).

cast_post_hook(Term, timer, duration(D)) :-
    get_time(T2),
    cast_pre_hook(Term, timer, start_time(T1)),
    D is T2 - T1.
```

**Validation:**
```prolog
cast_pre_hook(conjure(git(commit(Msg))), validator, validated) :-
    validate_commit_message(Msg).
```

**Permission Checks:**
```prolog
verify_pre_hook(component(Entity, _, _), permission_checker) :-
    has_permission_to_verify(Entity).
```

---

## Part 7: Python Bridge Pattern

### Architecture

For domains integrating with Python (golems, protocol_clients):

```
domain/
├── semantics.pl        # Pure Prolog (NO py_call)
├── python_bridge.pl    # ALL py_call + decoding
├── client.py          # Python implementation
└── semantics.plt      # Tests
```

### Separation of Concerns

**`semantics.pl` - PURE PROLOG:**
```prolog
:- use_module('python_bridge.pl', [
    mcp_call_tool/4,
    mcp_list_tools/2
]).

cast(conjure(protocol_client(mcp(call(Server, Tool, Args)))), Result) :-
    mcp_call_tool(Server, Tool, Args, DecodedResult),  % Already Prolog!
    Result = ok(DecodedResult).
```

**`python_bridge.pl` - ALL py_call + DECODING:**
```prolog
mcp_call_tool(Server, Tool, Args, DecodedResult) :-
    py_call(mcp_client:call_tool(Server, Tool, Args), PyResult),
    decode_mcp_result(PyResult, DecodedResult).  % Decode to Prolog terms

decode_mcp_result(PyResult, mcp_result(Content, IsError)) :-
    py_call(getattr(PyResult, 'content'), Content),
    py_call(getattr(PyResult, 'isError'), IsError).
```

**`client.py` - Python Implementation:**
```python
from fastmcp import FastMCPClient

class MCPClientRegistry:
    def __init__(self):
        self.clients = {}

    def call_tool(self, server, tool, args):
        client = self.clients[server]
        return client.call(tool, args)
```

**Key Principle:** Python objects NEVER leak into `semantics.pl` - all decoding happens in `python_bridge.pl`.

---

## Part 8: Domain Levels

Grimoire organizes domains into dependency levels:

### Level 0: Foundation
- **`ecs_kernel`** - Core ECS predicates and verification primitive
- **`run_tests`** - Test infrastructure and PLUnit integration

### Level 1: Orchestration
- **`grimoire`** - Core system orchestration, spell system, domain loading

### Level 2: System Domains
- **`git`** - Version control operations and repository modeling
- **`nix`** - Package management and reproducible builds
- **`fs`** - Filesystem operations and pattern matching
- **`db`** - SQLite integration with schema discovery
- **`utils`** - Utility predicates (tree building, collections)

### Level 3: Coordination & Composition
- **`interface`** - Multi-frontend access (CLI/HTTP/MCP)
- **`protocol_clients`** - External service consumption (HTTP/MCP clients)
- **`project`** - Project structure composing git+nix+fs

### Level 5: External Interfaces
- **`golems`** - AI agents with PydanticAI integration

---

## Part 9: Implementation Details

### Directory Structure

```
Grimoire/
├── src/
│   ├── ecs_kernel.pl           # Generative kernel
│   ├── ecs_kernel.plt          # Discriminative kernel
│   ├── grimoire.pl             # Core orchestration
│   ├── grimoire.plt            # Core tests
│   ├── git.pl                  # Git domain
│   ├── git.plt                 # Git tests
│   ├── fs.pl                   # Filesystem domain
│   ├── fs.plt                  # Filesystem tests
│   ├── db/
│   │   ├── semantics.pl        # Database domain
│   │   ├── semantics.plt       # Database tests
│   │   └── sqlite3.pl          # SQLite interface
│   ├── nix/
│   │   ├── semantics.pl        # Nix domain
│   │   └── semantics.plt       # Nix tests
│   ├── project/
│   │   ├── semantics.pl        # Project domain
│   │   └── semantics.plt       # Project tests
│   ├── interface/
│   │   ├── semantics.pl        # Interface domain
│   │   ├── semantics.plt       # Interface tests
│   │   └── api/
│   │       ├── semantics.pl    # API implementation
│   │       └── mcp_server.py   # MCP server
│   ├── protocol_clients/
│   │   ├── semantics.pl        # Protocol clients coordination
│   │   ├── semantics.plt       # Tests
│   │   ├── http/
│   │   │   ├── semantics.pl    # HTTP client (pure Prolog)
│   │   │   ├── python_bridge.pl # py_call + decoding
│   │   │   └── http_client.py   # httpx implementation
│   │   └── mcp/
│   │       ├── semantics.pl    # MCP client (pure Prolog)
│   │       ├── python_bridge.pl # py_call + FastMCP decoding
│   │       └── mcp_client.py    # FastMCP implementation
│   ├── golems/
│   │   ├── semantics.pl        # Golems domain (pure Prolog)
│   │   ├── semantics.plt       # Golems tests
│   │   └── python_bridge.pl    # py_call + PydanticAI decoding
│   ├── utils.pl                # Utility predicates
│   ├── utils.plt               # Utility tests
│   └── tests/
│       ├── git_test_entity.pl  # Git test entities
│       ├── fs_test_entity.pl   # Filesystem test entities
│       └── ...
├── README.md                   # User documentation
├── CLAUDE.md                   # Development guide
├── DESIGN.md                   # This document
├── overhaul-plan.md            # Complete specification
├── review_instructions.md      # Review checklist
└── flake.nix                   # Nix package specification
```

### Environment Variables

- **`GRIMOIRE_ROOT`** - Project directory (auto-detected from `./grimoire` script location)
- **`GRIMOIRE_DATA`** - Data storage directory (defaults to `$HOME/.grimoire`)

Path resolution:
```prolog
grimoire_resolve_path('@/src/git.pl', '/home/user/Projects/Grimoire/src/git.pl').
```

---

## Part 10: Benefits of This Design

### Mathematical Correctness

The generative-discriminative duality ensures:
- **Consistency**: Everything generated must be verifiable
- **Completeness**: Everything verified must have been generated
- **No Contradictions**: Inconsistent knowledge cannot exist

### Composability

`please_verify/1` enables clean composition:
- Domains verify sub-components recursively
- Cross-domain dependencies work naturally
- Verification chains are composable and complete

### Simplicity for Users

Users only need to:
1. Declare high-level facts using `has(domain(...))` in `semantics.pl`
2. Domain automatically expands into queryable properties
3. Tests verify properties using `user:please_verify`
4. System handles verification automatically

### Flexibility

The design allows:
- Unification patterns in verification
- Optional verification (base case in `please_verify`)
- Cross-domain composition
- Extensible hooks for custom behavior

### Testability

File-based test entities ensure:
- Order-independent tests (no state pollution)
- Declarative test data (visible in source)
- Matches production patterns (knowledge in files)
- Reproducible test runs

### Python Integration

Python bridge pattern provides:
- Type safety (Python objects never leak to Prolog)
- Clear separation of concerns
- Easy debugging (Python and Prolog layers separate)
- Flexible integration with external libraries

---

## Part 11: Future Directions

### Planned Enhancements

1. **Incremental Verification** - Cache verification results for performance
2. **Distributed Knowledge** - Git-like distribution of semantic knowledge
3. **Type Inference** - Automatic derivation of component types
4. **Visual Tools** - Graph visualization of component relationships
5. **Template Expansion** - More sophisticated project templates

### Research Directions

1. **Category Theory** - Formalize generative-discriminative as adjoint functors
2. **Modal Logic** - Express verification modalities formally
3. **Dependent Types** - Type-level verification of components
4. **Linear Logic** - Resource management through linear types

---

## Conclusion

Grimoire demonstrates that knowledge-based operating systems can be both theoretically principled and practically useful. The dual kernel architecture provides mathematical correctness while remaining accessible to users. The ECS pattern enables flexible modeling, the spell system provides safety, and the verification system ensures correctness.

The result is a system where semantic knowledge, operating system reality, and formal verification are unified in a coherent, composable architecture.
