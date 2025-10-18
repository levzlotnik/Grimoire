# Grimoire System Design

## Executive Summary

Grimoire is a knowledge-based operating system built on a **declarative DSL with term expansion**. You write high-level schemas using operators, and the system automatically generates all verification and expansion code. The core principle: **you never write implementation code manually - the system generates it from your declarations**.

**Current Status**: v0.1.0 - Production-ready core with declarative DSL, term expansion, and comprehensive domain integrations.

---

## Core Principle: Declarative DSL

### The Most Important Rule

**YOU NEVER WRITE `verify/1` MANUALLY. EVER.**

Instead, you use the `::` operator to declare verification constraints:

```prolog
% ✅ CORRECT - Declarative DSL with :: operator
component(_E, git_repository_root, Root)
    :: atom(Root),
       exists_directory(Root).

% ❌ FORBIDDEN - Never write verify/1 manually
verify(component(_E, git_repository_root, Root)) :-
    atom(Root),
    exists_directory(Root).
```

The `::` operator is processed by **term expansion** at compile time, automatically generating the `verify/1` clause.

### Term Expansion Architecture

Grimoire's power comes from automatic code generation through term expansion:

```prolog
% You write this declarative schema
component(_E, db_sqlite_file, File)
    :: (atom(File) ; string(File)),
       atom_string(File, FileStr),
       exists_file(FileStr).

% Term expansion automatically generates this at compile time
verify(component(_E, db_sqlite_file, File)) :-
    (atom(File) ; string(File)),
    atom_string(File, FileStr),
    exists_file(FileStr).
```

**Benefits:**
- No boilerplate code
- Type-safe by construction
- Self-documenting schemas
- Impossible to forget verification logic

---

## Part 1: The Two Operators

Grimoire provides two fundamental operators for declarative specification:

### `::` - Verification Constraints

The `::` operator declares "this component must satisfy these constraints":

```prolog
% Simple type constraint
component(_E, project_name, Name)
    :: atom(Name).

% Complex OS reality check
component(_E, git_repository_remote_url, URL)
    :: atom(URL),
       atom_string(URL, URLStr),
       valid_git_url(URLStr).

% Conjunction of multiple constraints
component(_E, db_sqlite_database, DB)
    :: atom(DB),
       exists_file(DB),
       sqlite_database_valid(DB).
```

**Generated Code:**

Each `::` declaration automatically generates a `verify/1` clause with the exact same body:

```prolog
verify(component(_E, project_name, Name)) :-
    atom(Name).

verify(component(_E, git_repository_remote_url, URL)) :-
    atom(URL),
    atom_string(URL, URLStr),
    valid_git_url(URLStr).
```

### `==>` - Component Expansion

The `==>` operator declares "this high-level fact expands to these queryable properties":

```prolog
% High-level git repository declaration expands to multiple properties
component(Entity, has(git(repository)), git(repository(Spec)))
    ==> [
        component(Entity, git_repository_root, Root) :-
            member(root(Root), Spec),

        component(Entity, git_repository_branch, Branch) :-
            member(branch(Branch), Spec),

        component(Entity, git_repository_remote_url, URL) :-
            member(remote(_, URL), Spec)
    ].
```

**Generated Code:**

The `==>` operator generates multiple component expansion rules:

```prolog
component(Entity, git_repository_root, Root) :-
    component(Entity, has(git(repository)), git(repository(Spec))),
    member(root(Root), Spec).

component(Entity, git_repository_branch, Branch) :-
    component(Entity, has(git(repository)), git(repository(Spec))),
    member(branch(Branch), Spec).

component(Entity, git_repository_remote_url, URL) :-
    component(Entity, has(git(repository)), git(repository(Spec))),
    member(remote(_, URL), Spec).
```

---

## Part 2: The Dual Kernel

Grimoire operates on a **generative-discriminative duality**:

```
ecs_kernel.pl (Generative)  ⊣  ecs_kernel.plt (Discriminative)
     ↓                              ↓
semantics.pl (Expansion)     ⊣  semantics.plt (Verification)
```

### Generative Flow (Left Side)

The generative flow **expands** high-level facts into rich component graphs:

```prolog
% User declares high-level fact
component(my_project, has(git(repository)), git(repository([
    root('/home/user/my_project'),
    branch(main),
    remote(origin, 'https://github.com/user/my_project')
]))).

% System auto-expands to queryable properties (via ==> operator)
% - component(my_project, git_repository_root, '/home/user/my_project')
% - component(my_project, git_repository_branch, main)
% - component(my_project, git_repository_remote_url, 'https://github.com/user/my_project')
```

### Discriminative Flow (Right Side)

The discriminative flow **verifies** components against OS reality:

```prolog
% User declares verification constraints (via :: operator)
component(_E, git_repository_root, Root)
    :: atom(Root),
       exists_directory(Root).

component(_E, git_repository_remote_url, URL)
    :: atom(URL),
       valid_git_url(URL).

% System auto-generates verify/1 clauses at compile time
% verify(component(_E, git_repository_root, Root)) :- ...
% verify(component(_E, git_repository_remote_url, URL)) :- ...
```

### Composable Verification

The discriminative flow uses `please_verify/1` for composable verification:

```prolog
% Composite verification uses please_verify to check all sub-components
component(Entity, has(git(repository)), git(repository(Spec)))
    :: please_verify(component(Entity, git_repository_root, _)),
       please_verify(component(Entity, git_repository_branch, _)),
       please_verify(component(Entity, git_repository_remote_url, _)).
```

**What `please_verify/1` does:**
1. Fetches the component (unifying variables)
2. Ensures it's grounded (no uninstantiated variables)
3. Calls `verify/1` if it exists (otherwise succeeds)
4. Throws if component doesn't exist or verification fails

**Critical: Verify Failure vs Exception Semantics**

Understanding the distinction between failure and exception in `verify/1` is essential:

- **`verify/1` FAILS** = "No verification rule exists for this component" → **Component is valid by default**
- **`verify/1` THROWS** = "Verification rule exists but component is invalid" → **Component is invalid**

How the `::` operator implements this:

```prolog
% You write:
component(_E, db_sqlite_file, File)
    :: exists_file(File).

% Term expansion generates:
verify(component(_E, db_sqlite_file, File)) :-
    (exists_file(File) -> true
    ; throw(error(verification_failed(component(_E, db_sqlite_file, File)), ...))).
```

Why `please_verify/1` uses `ignore/1`:

```prolog
please_verify(component(A, B, C)) :-
    % ... grounding checks ...
    ignore(verify(component(A, B, C))).  % Ignore FAILURE, propagate EXCEPTION
```

- If `verify` **fails** (no rule exists): `ignore` succeeds → component is valid by default
- If `verify` **throws** (rule exists, check failed): exception propagates → component is invalid

This enables verification **composition** - complex facts verified by composing primitive verifications.

---

## Part 3: Domain Pattern

Every domain follows the same declarative pattern:

### Structure

```
domain/
├── semantics.pl        # Generative: expansion with ==> operator
├── semantics.plt       # Discriminative: verification with :: operator, tests
└── python_bridge.pl    # (optional) Python integration
```

### Generative Flow: `semantics.pl`

```prolog
:- self_entity(git).

% Self-reference for domain metadata
component(git, self, semantic(folder('@/src/git'))).

% Entities for domain concepts
entity(git(repository)).
entity(git(commit)).
entity(git(branch)).

% Documentation
docstring(git, "Git repository management and version control operations").

% Component expansion using ==> operator
component(Entity, has(git(repository)), git(repository(Spec)))
    ==> [
        component(Entity, git_repository_root, Root) :-
            member(root(Root), Spec),

        component(Entity, git_repository_branch, Branch) :-
            member(branch(Branch), Spec),

        component(Entity, git_repository_remote_url, URL) :-
            member(remote(_, URL), Spec),

        component(Entity, git_repository_remote_name, Name) :-
            member(remote(Name, _), Spec)
    ].

% Spell implementations (discussed later)
% cast_impl/2 is auto-generated from spell schemas
```

### Discriminative Flow: `semantics.plt`

```prolog
:- use_module(library(plunit)).

% Load test entities from file
:- load_entity(semantic(file('@/src/tests/git_test_entity.pl'))).

% Verification constraints using :: operator
component(_E, git_repository_root, Root)
    :: atom(Root),
       exists_directory(Root).

component(_E, git_repository_remote_url, URL)
    :: atom(URL),
       atom_string(URL, URLStr),
       valid_git_url(URLStr).

component(_E, git_repository_branch, Branch)
    :: atom(Branch).

% Composite verification using please_verify
component(Entity, has(git(repository)), git(repository(_Spec)))
    :: please_verify(component(Entity, git_repository_root, _)),
       please_verify(component(Entity, git_repository_remote_url, _)),
       please_verify(component(Entity, git_repository_branch, _)).

% PLUnit tests
:- begin_tests(git_semantics).

test(git_repository_expansion, [
    setup(setup_test_repo),
    cleanup(cleanup_test_repo)
]) :-
    user:please_verify(component(test_basic_git, has(git(repository)), _)),
    user:please_verify(component(test_basic_git, git_repository_root, Root)),
    assertion(Root = '/tmp/test_repo').

test(git_commit_spell) :-
    user:magic_cast(conjure(git(commit("test"))), Result),
    assertion(Result = ok(_)).

:- end_tests(git_semantics).

% Setup/cleanup create ONLY filesystem resources
setup_test_repo :-
    make_directory('/tmp/test_repo'),
    process_create(path(git), ['init'], [cwd('/tmp/test_repo')]).

cleanup_test_repo :-
    delete_directory_and_contents('/tmp/test_repo').
```

**Key Points:**
- All verification uses `::` operator (auto-generates `verify/1`)
- Tests use `user:please_verify` and `user:magic_cast`
- Setup/cleanup manipulate ONLY filesystem (never `assertz`/`retract`)
- Test entities declared in separate `.pl` files

---

## Part 4: The Spell System

Grimoire provides **spells** - typed, composable operations with automatic implementation generation.

### Spell Categories

- **`conjure`** - State-changing operations (git commit, database insert, file write)
- **`perceive`** - Read-only operations (git status, database query, file read)

### Spell Declaration with Automatic Implementation

**YOU NEVER WRITE `cast/2` MANUALLY. EVER.**

Instead, you declare spell schemas and let term expansion generate `cast_impl/2`:

```prolog
% ✅ CORRECT - Declarative spell schema
spell(
    conjure(git(commit)),
    input(git(commit(message('Message')))),
    output(either(
        ok(committed(hash('Hash'))),
        error(commit_failed('Reason'))
    )),
    "Create a git commit with the given message"
) :: (
    % Implementation body goes here
    string(Message),
    phrase(git_args(commit(Message)), Args),
    magic_cast(conjure(executable_program(git, Args)), Result)
).

% ❌ FORBIDDEN - Never write cast_impl/2 manually
cast_impl(conjure(git(commit(Message))), Result) :-
    string(Message),
    phrase(git_args(commit(Message)), Args),
    magic_cast(conjure(executable_program(git, Args)), Result).
```

**What Term Expansion Generates:**

```prolog
% Auto-generates metadata components
component(conjure, ctor, git(commit)).
component(conjure(git(commit)), format_input, input(git(commit(message(_))))).
component(conjure(git(commit)), format_output, output(either(ok(committed(hash(_))), error(commit_failed(_))))).
component(conjure(git(commit)), docstring, "Create a git commit with the given message").

% Auto-generates implementation
cast_impl(conjure(git(commit(Message))), Result) :-
    string(Message),
    phrase(git_args(commit(Message)), Args),
    magic_cast(conjure(executable_program(git, Args)), Result).
```

### Spell Invocation vs Implementation

**`cast_impl/2`** - Spell implementation (auto-generated, appears in HEAD)
**`magic_cast/2`** - Spell invocation (user calls, appears in BODY)

```prolog
% ✅ CORRECT - Invoking a spell
do_task(Result) :-
    magic_cast(conjure(git(commit("message"))), Result).

% ❌ WRONG - Bypasses hooks and grounding
do_task(Result) :-
    cast_impl(conjure(git(commit("message"))), Result).
```

**Why `magic_cast/2`?**
- Ensures terms are grounded (no uninstantiated variables)
- Executes pre/post hooks (session logging, monitoring)
- Validates spell exists and is uniquely registered
- Handles errors consistently

---

## Part 5: Testing Architecture

### File-Based Test Entities

**CRITICAL: Tests NEVER use `assertz`/`retract` for component/entity facts.**

All test knowledge lives in declarative `.pl` files under `src/tests/`:

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

component(test_advanced_git, has(git(repository)), git(repository([
    root('/tmp/advanced_repo'),
    branch(development)
]))).

docstring(test_entities(git), "Test entities for git domain").
```

### Test Implementation

```prolog
% src/git.plt
:- load_entity(semantic(file('@/src/tests/git_test_entity.pl'))).

:- begin_tests(git_semantics).

test(git_component_expansion, [
    setup(setup_test_repo),
    cleanup(cleanup_test_repo)
]) :-
    % Test entities already loaded from file
    user:please_verify(component(test_basic_git, has(git(repository)), _)),
    user:please_verify(component(test_basic_git, git_repository_root, Root)),
    assertion(Root = '/tmp/test_repo'),
    user:please_verify(component(test_basic_git, git_repository_branch, Branch)),
    assertion(Branch = main).

test(git_spell_invocation) :-
    user:magic_cast(conjure(git(commit("test"))), Result),
    assertion(Result = ok(_)).

:- end_tests(git_semantics).

% Setup/cleanup manipulate ONLY filesystem
setup_test_repo :-
    make_directory('/tmp/test_repo'),
    process_create(path(git), ['init'], [cwd('/tmp/test_repo')]).

cleanup_test_repo :-
    delete_directory_and_contents('/tmp/test_repo').
```

**Setup/Cleanup Rules:**
- ✅ Create/delete directories and files
- ✅ Run `process_create(path(git), ...)`
- ✅ Run `process_create(path(sqlite3), ...)`
- ❌ NEVER `assertz(component(...))`
- ❌ NEVER `retract(entity(...))`
- ❌ NEVER modify ECS state at runtime

**Rationale:**
- Test entities in files are order-independent
- No state pollution between tests
- Declarative test data is visible and debuggable
- Matches production pattern (all knowledge in files)

---

## Part 6: Python Bridge Pattern

For domains integrating Python (golems, protocol_clients):

### Structure

```
domain/
├── semantics.pl        # Pure Prolog (NO py_call)
├── python_bridge.pl    # ALL py_call + decoding to Prolog
├── client.py          # Python implementation
└── semantics.plt      # Tests
```

### Separation of Concerns

**`semantics.pl` - PURE PROLOG ONLY:**

```prolog
:- use_module('python_bridge.pl', [
    golem_execute_task/2,
    golem_list_capabilities/1
]).

% Spell implementation using bridge (no py_call here!)
spell(
    conjure(golem(execute)),
    input(golem(execute(task('Task')))),
    output(ok(result('Result'))),
    "Execute a task using a golem agent"
) :: (
    golem_execute_task(Task, DecodedResult),  % Already Prolog terms!
    Result = ok(DecodedResult)
).
```

**`python_bridge.pl` - ALL py_call + DECODING:**

```prolog
:- use_foreign_library(foreign(janus)).

golem_execute_task(Task, DecodedResult) :-
    py_call(golem_module:execute_task(Task), PyResult),
    decode_golem_result(PyResult, DecodedResult).  % Convert to Prolog

decode_golem_result(PyObj, golem_result(Status, Data)) :-
    py_call(getattr(PyObj, 'status'), Status),
    py_call(getattr(PyObj, 'data'), Data).
```

**`client.py` - Python Implementation:**

```python
from pydantic_ai import Agent

class GolemExecutor:
    def __init__(self):
        self.agent = Agent(model='openai:gpt-4')

    def execute_task(self, task):
        result = self.agent.run_sync(task)
        return GolemResult(status='success', data=result.data)
```

**Key Principle:** Python objects NEVER leak into `semantics.pl` - all decoding happens in `python_bridge.pl`.

---

## Part 7: ECS Architecture

### Entity-Component-System Pattern

```prolog
% Entity - A thing that exists
entity(my_project).
entity(git(repository)).

% Component - A property of an entity
component(my_project, git_repository_root, '/home/user/my_project').
component(my_project, project_name, 'MyProject').

% System - Operations on entities (implemented via spells)
magic_cast(conjure(git(commit("Initial commit"))), Result).
```

### Component Types

- **`ctor`** - Constructors for sum types (spell signatures)
- **`has(domain(...))`** - High-level domain facts that expand
- **`domain_property`** - Expanded properties from `has(domain(...))`
- **`docstring`** - Documentation strings
- **`self`** - Self-referential metadata (file/folder location)
- **`format_input`** - Spell input format specification
- **`format_output`** - Spell output format specification

### Self-Entity Pattern

```prolog
% Domains declare themselves
:- self_entity(git).

% System auto-generates
component(git, self, semantic(folder('@/src/git'))).
entity(git).
```

### Multifile Declarations

```prolog
% Core ECS predicates (never use user: prefix)
:- multifile entity/1.
:- multifile component/3.
:- multifile docstring/2.
:- multifile verify/1.  % Auto-generated by :: operator
```

---

## Part 8: Domain Levels

Grimoire organizes domains into dependency levels:

### Level 0: Foundation
- **`ecs_kernel`** - Core ECS with operators (`::` and `==>`) and `please_verify/1`
- **`run_tests`** - Test infrastructure and PLUnit integration

### Level 1: Orchestration
- **`grimoire`** - Core orchestration, spell system (`magic_cast/2`), domain loading

### Level 2: System Domains
- **`git`** - Version control operations and repository modeling
- **`nix`** - Package management and reproducible builds
- **`fs`** - Filesystem operations with atomic guarantees
- **`db`** - SQLite integration with schema validation
- **`utils`** - Utility predicates (validation, tree building, collections)

### Level 3: Coordination
- **`interface`** - Multi-frontend access (CLI/HTTP/MCP)
- **`protocol_clients`** - External service clients (HTTP/MCP)
- **`project`** - Project structure composing git+nix+fs

### Level 5: External Interfaces
- **`golems`** - AI agents with PydanticAI integration

---

## Part 9: Term Expansion Internals

### How `::` Operator Works

```prolog
% User writes this
component(_E, db_sqlite_file, File)
    :: (atom(File) ; string(File)),
       exists_file(File).

% term_expansion/2 hook in ecs_kernel.pl processes it
term_expansion(
    (component(A, B, C) :: Body),
    verify(component(A, B, C)) :- Body
).

% Result: verify/1 clause auto-generated at compile time
verify(component(_E, db_sqlite_file, File)) :-
    (atom(File) ; string(File)),
    exists_file(File).
```

### How `==>` Operator Works

```prolog
% User writes this
component(Entity, has(git(repository)), git(repository(Spec)))
    ==> [
        component(Entity, git_repository_root, Root) :-
            member(root(Root), Spec),

        component(Entity, git_repository_branch, Branch) :-
            member(branch(Branch), Spec)
    ].

% term_expansion/2 hook processes it
term_expansion(
    (component(A, B, C) ==> Expansions),
    [component(A, B, C), ExpandedRules]
) :-
    % Keep the base component declaration
    % Generate expansion rules with component(A, B, C) guard
    maplist(add_guard(component(A, B, C)), Expansions, ExpandedRules).

% Result: Multiple component/3 clauses generated
component(Entity, has(git(repository)), git(repository(Spec))).

component(Entity, git_repository_root, Root) :-
    component(Entity, has(git(repository)), git(repository(Spec))),
    member(root(Root), Spec).

component(Entity, git_repository_branch, Branch) :-
    component(Entity, has(git(repository)), git(repository(Spec))),
    member(branch(Branch), Spec).
```

### How Spell Schema Works

```prolog
% User writes this
spell(
    conjure(git(commit)),
    input(git(commit(message('Message')))),
    output(ok(committed(hash('Hash')))),
    "Create a git commit"
) :: (
    string(Message),
    % ... implementation ...
).

% term_expansion/2 in grimoire.pl processes it
term_expansion(
    spell(SpellSig, Input, Output, Doc) :: Body,
    [Metadata, Implementation]
) :-
    SpellSig =.. [SpellType, Constructor],
    % Generate metadata
    Metadata = [
        component(SpellType, ctor, Constructor),
        component(SpellSig, format_input, Input),
        component(SpellSig, format_output, Output),
        component(SpellSig, docstring, Doc)
    ],
    % Generate implementation
    Implementation = (cast_impl(SpellTerm, Result) :- Body).

% Result: Metadata + cast_impl/2 auto-generated
component(conjure, ctor, git(commit)).
component(conjure(git(commit)), format_input, input(git(commit(message(_))))).
component(conjure(git(commit)), format_output, output(ok(committed(hash(_))))).
component(conjure(git(commit)), docstring, "Create a git commit").

cast_impl(conjure(git(commit(Message))), Result) :-
    string(Message),
    % ... implementation ...
```

**Key Insight:** Term expansion happens at **compile time**, so all code generation is complete before runtime. This ensures:
- Zero runtime overhead
- Type checking at compile time
- Full introspection capabilities
- Impossible to bypass generated code

---

## Part 10: Hook System

Grimoire provides extensible hook points for cross-cutting concerns:

### Hook Infrastructure

```prolog
:- dynamic([
    cast_pre_hook/3,
    cast_post_hook/3,
    verify_pre_hook/2,
    verify_post_hook/2
]).

% Hooks called by magic_cast and please_verify
cast_pre_hooks(Term, Outs) :-
    findall(Out, (is_hook(Hook), cast_pre_hook(Term, Hook, Out)), Outs).

verify_pre_hooks(Term) :-
    findall(_, (is_hook(Hook), verify_pre_hook(Term, Hook)), _).
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

---

## Part 11: Benefits of This Design

### Declarative Simplicity

Users never write implementation code:
- **Verification**: Use `::` operator, `verify/1` auto-generated
- **Expansion**: Use `==>` operator, component rules auto-generated
- **Spells**: Use `spell(...) ::` schema, `cast_impl/2` auto-generated

### Type Safety

Term expansion ensures:
- Every spell has registered metadata
- Every component has optional verification
- No unregistered spells can be called
- All spells validated for uniqueness

### Composability

`please_verify/1` enables natural composition:
- Primitive verifications compose into complex verifications
- Cross-domain dependencies work seamlessly
- Verification chains are declarative and complete

### Testability

File-based entities ensure:
- Order-independent tests (no state pollution)
- Declarative test data (visible in source)
- Reproducible test runs
- Matches production patterns

### Mathematical Correctness

The generative-discriminative duality ensures:
- **Consistency**: Everything expanded can be verified
- **Completeness**: Everything verified has been expanded
- **Soundness**: Invalid states cannot be represented

---

## Part 12: Implementation Checklist

When adding a new domain:

### 1. Create Directory Structure

```bash
mkdir -p src/domain
touch src/domain/semantics.pl
touch src/domain/semantics.plt
mkdir -p src/tests
touch src/tests/domain_test_entity.pl
```

### 2. Write Generative Flow (`semantics.pl`)

```prolog
:- self_entity(domain).

% Entities
entity(domain(concept)).

% Documentation
docstring(domain, "Domain description").

% Component expansion using ==> operator
component(Entity, has(domain(fact)), domain(fact(Spec)))
    ==> [
        component(Entity, domain_property1, Value) :-
            member(prop1(Value), Spec),

        component(Entity, domain_property2, Value) :-
            derive_property2(Spec, Value)
    ].

% Spell schemas using spell(...) :: syntax
spell(
    conjure(domain(operation)),
    input(domain(operation(arg('Arg')))),
    output(ok(result('Result'))),
    "Perform domain operation"
) :: (
    % Implementation
    validate_arg(Arg),
    perform_operation(Arg, Result)
).
```

### 3. Write Discriminative Flow (`semantics.plt`)

```prolog
:- use_module(library(plunit)).
:- load_entity(semantic(file('@/src/tests/domain_test_entity.pl'))).

% Verification constraints using :: operator
component(_E, domain_property1, Value)
    :: atom(Value).

component(_E, domain_property2, Value)
    :: integer(Value),
       Value > 0.

% Composite verification using please_verify
component(Entity, has(domain(fact)), domain(fact(_Spec)))
    :: please_verify(component(Entity, domain_property1, _)),
       please_verify(component(Entity, domain_property2, _)).

% Tests
:- begin_tests(domain_semantics).

test(domain_expansion) :-
    user:please_verify(component(test_entity, has(domain(fact)), _)),
    user:please_verify(component(test_entity, domain_property1, _)).

test(domain_spell) :-
    user:magic_cast(conjure(domain(operation(arg))), Result),
    assertion(Result = ok(_)).

:- end_tests(domain_semantics).

% Setup/cleanup
setup_test :-
    % Create filesystem resources only
    make_directory('/tmp/test_domain').

cleanup_test :-
    delete_directory_and_contents('/tmp/test_domain').
```

### 4. Create Test Entities

```prolog
% src/tests/domain_test_entity.pl
:- self_entity(test_entities(domain)).

entity(test_entity).

component(test_entity, has(domain(fact)), domain(fact([
    prop1(value1),
    prop2(42)
]))).

docstring(test_entities(domain), "Test entities for domain").
```

### 5. Run Tests

```bash
./grimoire test domain
```

---

## Conclusion

Grimoire demonstrates that a knowledge-based operating system can be both theoretically principled and practically simple. The declarative DSL with term expansion eliminates boilerplate, the dual kernel ensures correctness, and the composable verification enables flexible modeling.

**The result:** A system where you declare what you want, and the system automatically generates all implementation code, verifies it against reality, and provides type-safe operations.

**Remember the golden rule:** You never write `verify/1` or `cast_impl/2` manually - you use the `::` and `==>` operators, and let term expansion do the work.
