## Grimoire Development Guide for AI Assistants

This document contains critical patterns and conventions for working on the Grimoire codebase. **Read this carefully before making any changes.**

## Architecture: The Dual Kernel

Grimoire operates on a **generative-discriminative duality**:

```
ecs_kernel.pl (Generative)  ⊣  ecs_kernel.plt (Discriminative)
     ↓                              ↓
semantics.pl (Expansion)     ⊣  semantics.plt (Verification)
```

### Every Domain Follows This Pattern:

- **`semantics.pl`** - Generative flow: entity declarations, component expansion, spell implementations
- **`semantics.plt`** - Discriminative flow: `verify/1` predicates, PLUnit tests

## Critical Distinctions

### 1. `cast` vs `magic_cast` - THE MOST COMMON ERROR

**`cast/2`** = Spell **DECLARATION** (appears in HEAD)
```prolog
% ✅ CORRECT - declaring a spell implementation
cast(conjure(git(commit(Message))), Result) :- ...
```

**`magic_cast/2`** = Spell **INVOCATION** (appears in BODY)
```prolog
% ✅ CORRECT - invoking a spell
do_task(Result) :-
    magic_cast(conjure(git(commit("message"))), Result).

% ❌ WRONG - bypasses hooks and grounding
do_task(Result) :-
    cast(conjure(git(commit("message"))), Result).
```

**Why This Matters:**
- `magic_cast` ensures terms are grounded (no uninstantiated variables)
- `magic_cast` executes pre/post hooks (session logging, monitoring)
- Using `cast` in bodies bypasses critical infrastructure

### 2. `verify` vs `please_verify` - SAME PRINCIPLE

**`verify/1`** = Verification **DECLARATION** (appears in HEAD)
```prolog
% ✅ CORRECT - declaring verification logic
verify(component(Entity, git_repository_root, Root)) :-
    exists_directory(Root) -> true
    ; throw(verification_error(git, missing_repository(Root))).
```

**`please_verify/1`** = Verification **INVOCATION** (appears in BODY)
```prolog
% ✅ CORRECT - invoking composable verification
verify(component(E, has(project(app)), project(app(Spec)))) :-
    please_verify(component(E, project_git_repo, GitRepo)),
    please_verify(component(E, project_nix_flake, NixFlake)),
    verify_combination(GitRepo, NixFlake).

% ❌ WRONG - doesn't ensure grounding or composability
verify(component(E, has(project(app)), project(app(Spec)))) :-
    component(E, project_git_repo, GitRepo),  % Not grounded!
    component(E, project_nix_flake, NixFlake).
```

**Why This Matters:**
- `please_verify` ensures component exists AND is grounded
- `please_verify` enables composable verification chains
- Direct `component/3` calls don't guarantee grounding

### 3. NEVER Use Bare Predicates in Bodies

```prolog
% ❌ FORBIDDEN in predicate bodies:
perceive(something(X)).        % Use magic_cast(perceive(...))
cast(conjure(...)).           % Use magic_cast(conjure(...))
component(E, prop, V).        % Use please_verify(component(...))
verify(component(...)).       % Use please_verify(component(...))
```

**Exception:** Only spell/verification IMPLEMENTATIONS use these in HEAD position.

## Component Expansion Pattern

### Generative Flow (semantics.pl)

```prolog
% User declares high-level fact with has(domain(...))
component(my_project, has(git(repository)), git(repository([
    remote(origin, 'https://github.com/user/repo'),
    branch(main)
]))).

% Domain expands into queryable properties
component(Entity, git_repository_remote_url, URL) :-
    component(Entity, has(git(repository)), git(repository(Spec))),
    member(remote(_, URL), Spec).

component(Entity, git_repository_branch, Branch) :-
    component(Entity, has(git(repository)), git(repository(Spec))),
    member(branch(Branch), Spec).
```

### Discriminative Flow (semantics.plt)

```prolog
% Verify composite by composing primitive verifications
verify(component(Entity, has(git(repository)), git(repository(Spec)))) :-
    % Step 1: Verify all expanded components exist and are grounded
    please_verify(component(Entity, git_repository_remote_url, URL)),
    please_verify(component(Entity, git_repository_branch, Branch)),
    % Step 2: Check OS reality
    verify_git_repository_accessible(URL, Branch).

% Verify each primitive component against OS reality
verify(component(Entity, git_repository_remote_url, URL)) :-
    % Component existence already proven by please_verify
    atom_string(URL, URLStr),
    % Check OS reality
    (valid_url(URLStr) -> true
    ; throw(verification_error(git, invalid_remote_url(URL)))).
```

**Pattern Summary:**
1. Generative: `has(domain(...))` expands to multiple `domain_property` components
2. Discriminative: `verify(has(domain(...)))` uses `please_verify` on each property, then checks OS reality

## Spell Registration - MANDATORY

**Every `cast/2` implementation MUST have `register_spell/4` immediately above it:**

```prolog
% ALWAYS place register_spell RIGHT ABOVE cast
register_spell(
    conjure(git(commit)),
    input(git(commit(message('Message')))),
    output(either(
        ok(committed(hash('Hash'))),
        error(commit_failed('Reason'))
    )),
    docstring("Create a git commit with the given message")
).

cast(conjure(git(commit(Message))), Result) :-
    % Implementation
    ...
```

**What register_spell/4 Provides:**
- Auto-generates: `component(conjure, ctor, git(commit))`
- Auto-generates: `component(git(commit), format_input, ...)`
- Auto-generates: `component(git(commit), format_output, ...)`
- Auto-generates: `component(git(commit), docstring, "...")`

## Testing Patterns

### File-Based Test Entities - NO Runtime assertz/retract

**CRITICAL: Tests NEVER use `assertz`/`retract` for component/entity facts.**

All test knowledge lives in `.pl` files under `src/tests/`:

```prolog
% src/tests/git_test_entity.pl
:- self_entity(test_entities(git)).

entity(test_basic_git).

component(test_basic_git, has(git(repository)), git(repository([
    root('/tmp/test_repo'),
    branch(main)
]))).

component(test_basic_git, test_path, '/tmp/test_repo').
```

### Test Implementation Pattern

```prolog
% src/git.plt
:- load_entity(semantic(file('@/src/tests/git_test_entity.pl'))).

:- begin_tests(git_semantics).

% Test component verification
test(git_repository_verification, [
    setup(setup_test_repo),
    cleanup(cleanup_test_repo)
]) :-
    % Use user: prefix in tests
    user:please_verify(component(test_basic_git, has(git(repository)), _)),
    user:please_verify(component(test_basic_git, git_repository_remote_url, _)).

% Test spell invocation
test(git_commit_spell) :-
    % Use user:magic_cast in tests
    user:magic_cast(conjure(git(commit("test"))), Result),
    assertion(Result = ok(_)).

:- end_tests(git_semantics).

% Setup creates ONLY filesystem resources
setup_test_repo :-
    TestPath = '/tmp/test_repo',
    make_directory(TestPath),
    process_create(path(git), ['init'], [cwd(TestPath)]).

% Cleanup removes ONLY filesystem resources
cleanup_test_repo :-
    delete_directory_and_contents('/tmp/test_repo').
```

### Setup/Cleanup Rules

- ✅ Create/delete directories and files
- ✅ Run `process_create(path(git), ['init'], ...)`
- ✅ Run `process_create(path(sqlite3), ...)`
- ❌ NEVER `assertz(component(...))`
- ❌ NEVER `retract(entity(...))`
- ❌ NEVER modify ECS state at runtime

### Tests Use `user:` Prefix

**Inside `begin_tests(...) ... end_tests(...)` blocks, ALWAYS use:**
- `user:please_verify(...)` instead of `please_verify(...)`
- `user:magic_cast(...)` instead of `magic_cast(...)`

**Outside test blocks, never use `user:` prefix.**

## Python Bridge Pattern

For domains integrating Python (golems, protocol_clients):

### Structure
```
domain/
├── semantics.pl        # Pure Prolog (NO py_call)
├── python_bridge.pl    # ALL py_call + decoding to Prolog
├── client.py          # Python implementation
└── semantics.plt      # Tests
```

### Critical Rule: Python Isolation

**`semantics.pl` must be PURE PROLOG - NO `py_call`:**

```prolog
% semantics.pl - PURE PROLOG ONLY
:- use_module('python_bridge.pl', [golem_function/2]).

cast(conjure(golem(task(Args))), Result) :-
    golem_function(Args, DecodedResult),  % Already Prolog terms!
    Result = ok(DecodedResult).
```

**`python_bridge.pl` contains ALL `py_call` + decoding:**

```prolog
% python_bridge.pl - ALL py_call + DECODING
golem_function(Args, DecodedResult) :-
    py_call(golem_module:function(Args), PyResult),
    decode_golem_result(PyResult, DecodedResult).  % Convert to Prolog

decode_golem_result(PyObj, PrologTerm) :-
    py_call(getattr(PyObj, 'field'), Field),
    PrologTerm = result(field(Field)).
```

**Why:** Python objects NEVER leak into `semantics.pl`.

## ECS Patterns

### Entity Declaration

```prolog
:- self_entity(domain_name).  % Declares entity and self component

entity(explicit_entity).      % Explicit entity declaration
```

### Component Types

- `ctor` - Constructors for sum types
- `has(domain(...))` - High-level domain facts that expand
- `domain_property` - Expanded properties from `has(domain(...))`
- `docstring` - Documentation strings
- `self` - Self-referential metadata

### Multifile Declarations

```prolog
% Never use user: prefix for these
:- multifile entity/1.
:- multifile component/3.
:- multifile docstring/2.
:- multifile verify/1.
:- multifile register_spell/4.
```

## Git Practices

- **NEVER** use `git add .` or `git add -A`
- **ALWAYS** use `git add -u` or add files individually
- Use `git mv` instead of `mv` for tracked files

## Testing Workflow

```bash
# Run all tests
./grimoire test

# Run specific domain tests
./grimoire test git

# Run specific test name
./grimoire test 'git_repository_verification'

# Use exec for interactive testing
./grimoire exec
```

## Environment Variables

- `GRIMOIRE_ROOT` - Project directory (where `./grimoire` is)
- `GRIMOIRE_DATA` - Data directory (defaults to `$HOME/.grimoire`)

## Common Anti-Patterns to Avoid

### ❌ Anti-Pattern 1: Using cast in bodies
```prolog
% WRONG
my_function(Result) :-
    cast(conjure(something(X)), R1).  % Should be magic_cast!
```

### ❌ Anti-Pattern 2: Using bare component in bodies
```prolog
% WRONG
verify(component(E, has(complex), complex(X))) :-
    component(E, prop1, P1),  % Should be please_verify!
    component(E, prop2, P2).
```

### ❌ Anti-Pattern 3: Missing register_spell
```prolog
% WRONG - no register_spell above cast
cast(conjure(new_spell(X)), Result) :- ...
```

### ❌ Anti-Pattern 4: Runtime assertions in tests
```prolog
% WRONG
setup_test :-
    assertz(entity(test_entity)).  % Use file-based entities!
```

### ❌ Anti-Pattern 5: Python objects in semantics.pl
```prolog
% WRONG - semantics.pl should never see Python objects
cast(conjure(thing(X)), Result) :-
    py_call(module:func(X), PyObj),  % Should be in python_bridge.pl!
    Result = ok(PyObj).
```

## Key Principles Summary

1. **`cast` = declare, `magic_cast` = invoke**
2. **`verify` = declare, `please_verify` = invoke**
3. **Every `cast` needs `register_spell/4` immediately above**
4. **No `assertz`/`retract` in tests - file-based entities only**
5. **Tests use `user:please_verify` and `user:magic_cast`**
6. **Python objects never leak into `semantics.pl`**
7. **Generative flow expands `has(domain(...))` into properties**
8. **Discriminative flow verifies with `please_verify` then checks OS reality**
9. **All knowledge lives in files, not runtime assertions**

## File Structure Convention

```
domain/
├── semantics.pl        # Generative: expansion rules, spells
├── semantics.plt       # Discriminative: verify/1, tests
└── python_bridge.pl    # (if needed) Python integration
```

## When Adding New Features

1. Add component expansion in `semantics.pl`
2. Add `verify/1` in `semantics.plt` using `please_verify` composition
3. Add `register_spell/4` above any new `cast/2` implementations
4. Create file-based test entities in `src/tests/`
5. Write PLUnit tests using `user:please_verify` and `user:magic_cast`

## Documentation References

- **`README.md`** - User-facing documentation
- **`DESIGN.md`** - Architectural deep dive
- **`overhaul-plan.md`** - Complete system specification
- **`review_instructions.md`** - Domain review checklist
