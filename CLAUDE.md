# INSTRUCTION COMPLIANCE PROTOCOL

**CRITICAL: When the user gives you an explicit instruction or correction:**

1. **STOP** whatever tangent you're on
2. **ACKNOWLEDGE** the instruction explicitly ("You want me to X")
3. **ASK** if anything is unclear - do NOT guess or substitute your own interpretation
4. **EXECUTE** exactly what was instructed - nothing more, nothing less
5. **VERIFY** the result addresses what they asked for

**If you think the instruction is wrong or suboptimal:**
- State clearly: "I understand you want X. However, I see Y. Should we address Y first or proceed with X?"
- Wait for answer
- Do NOT proceed with your alternative without explicit approval

**If you made a mistake and are being corrected:**
- Acknowledge: "You're right, I was wrong about X. I'm sorry."
- Implement the correction immediately
- Do NOT defend your mistake or deflect to other issues

**RED FLAGS that you're failing:**
- User repeats the same instruction
- User escalates tone or uses profanity
- User asks "are you listening?"
- You're investigating something other than what was asked

**When these occur: STOP, re-read their last 3 messages, and address ONLY what they explicitly asked for.**

## Grimoire Development Guide for AI Assistants

This document contains critical patterns and conventions for working on the Grimoire codebase. **Read this carefully before making any changes.**

## Architecture: Declarative DSL with Term Expansion

Grimoire uses **term expansion** to automatically generate code from declarative specifications. You write high-level DSL using operators, the system generates the implementation.

### The Dual Kernel

```
ecs_kernel.pl (Generative)  ⊣  ecs_kernel.plt (Discriminative)
     ↓                              ↓
semantics.pl (Expansion)     ⊣  semantics.plt (Verification)
```

**Every domain follows this pattern:**
- **`semantics.pl`** - DSL schemas using operators (`==>` and `::`), spell registrations, component expansion
- **`semantics.plt`** - PLUnit tests (NO manual `verify/1` clauses)

## Critical: NEVER Write verify/1 Manually

**The `::` operator auto-generates `verify/1` clauses. You NEVER write them by hand.**

```prolog
% ✅ CORRECT - Use :: operator, verify/1 auto-generated
component(_E, db_sqlite_file, File)
    :: (atom(File) ; string(File)),
       atom_string(File, FileStr),
       exists_file(FileStr).

% ❌ FORBIDDEN - Never write verify/1 manually
verify(component(_E, db_sqlite_file, File)) :-
    (atom(File) ; string(File)),
    atom_string(File, FileStr),
    exists_file(FileStr).
```

## Declarative Component DSL

### Operators

- `==>` (1150, xfx) - "expands to" - Generative AND discriminative (component expansion + verification)
- `::` (1160, xfx) - "such that" - Discriminative ONLY (verification constraints)

### The Three Fundamental Patterns

Understanding these three patterns is CRITICAL:

**Manual `component/3` Rules** - External Activation (OS Reality → KB)
```prolog
% Use when component values come from EXTERNAL sources (OS, databases, git status)
% NOT from Grimoire's KB - from external state that changes independently
component(E, git_repository_current_branch, Branch) :-
    component(E, git_repository_root, Root),
    get_git_current_branch(Root, Branch).  % Query external system
```
**When to use:** Component depends on external factors not tracked in Grimoire's KB.

**`::` Operator** - Verification ONLY (KB → OS Reality Check)
```prolog
% ONLY generates verify/1 clause, NO component expansion
% Used to verify KB values against OS reality at test time
component(_E, git_repository_root, Root)
    :: atom(Root),
       exists_directory(Root).
```
**When to use:** Verify a component (already in KB) matches OS reality. Does NOT create expansion rules.

**`==>` Operator** - KB Implications (KB → KB, with verification)
```prolog
% Generates BOTH component expansion rules AND verify clause
% Used for deriving KB components from other KB components
component(E, has(git(repository)), git(repository(Spec)))
    ==> component(E, git_repository_root, Root) :- member(root(Root), Spec),
        component(E, git_repository_branch, Branch) :- member(branch(Branch), Spec).
```
**When to use:** One KB component logically implies other KB components. Auto-generates expansion rules + verification.

### Detailed Pattern Examples

**Component Expansion with Verification (using `==>` operator)**

```prolog
component(Entity, has(db(sqlite)), db(sqlite(database_id(DbId), file(File), schema(Schema))))
    ==> component(Entity, db_sqlite_id, DbId),
        component(Entity, db_sqlite_file, File),
        component(Entity, db_sqlite_schema, Schema)
    ::  format('Database components valid~n').
```

**What this generates:**
1. Three component expansion rules (generative)
2. One `verify/1` clause that calls `please_verify` on each expansion (discriminative)

### Critical: Verify Failure vs Exception Semantics

**Understanding the distinction between failure and exception in `verify/1` is ESSENTIAL:**

- **`verify/1` FAILS** = "No verification rule exists for this component" → **Component is valid by default**
- **`verify/1` THROWS** = "Verification rule exists but component is invalid" → **Component is invalid**

**How this works:**

```prolog
% The :: operator AUTOMATICALLY wraps your body with error handling
component(_E, db_sqlite_file, File)
    :: exists_file(File).

% Term expansion generates this verify/1 clause:
verify(component(_E, db_sqlite_file, File)) :-
    (exists_file(File) -> true  % Success
    ; throw(error(verification_failed(component(_E, db_sqlite_file, File)), ...))).  % Failure → Exception
```

**Why `please_verify/1` uses `ignore/1`:**

```prolog
please_verify(component(A, B, C)) :-
    % ... grounding checks ...
    ignore(verify(component(A, B, C))).  % Ignore FAILURE, propagate EXCEPTION
```

- If `verify` **fails** (no rule exists): `ignore` succeeds → component is valid
- If `verify` **throws** (rule exists, check failed): exception propagates → component is invalid

**This means:**
- Components without verification rules are valid by default
- Only components with `::` verification rules can be invalid
- Verification failures are explicit exceptions, not silent failures

### Leaf Verification Only

```prolog
component(_E, db_sqlite_file, File)
    :: exists_file(File),
       access_file(File, read).
```

**What this generates:**
1. One `verify/1` clause with the constraints

### Expansion Without Verification

```prolog
component(Entity, git_repository_remote_url, URL)
    ==> component(Entity, has(git(repository)), git(repository(Spec))),
        member(remote(_, URL), Spec).
```

**What this generates:**
1. Component expansion rule only
2. Verify clause that does `please_verify` on dependencies (no additional constraints)

## Critical Distinctions

### 1. `cast_impl` vs `magic_cast`

**`cast_impl/2`** = Spell **IMPLEMENTATION** (appears in HEAD, auto-generated by `register_spell`)
```prolog
% ✅ This is auto-generated - you never write it
% register_spell(..., implementation(conjure(git(commit(Message))), Result, (...)))
% generates:
% cast_impl(conjure(git(commit(Message))), Result) :- ...
```

**`magic_cast/2`** = Spell **INVOCATION** (appears in BODY)
```prolog
% ✅ CORRECT - invoking a spell
do_task(Result) :-
    magic_cast(conjure(git(commit("message"))), Result).

% ❌ WRONG - bypasses hooks, grounding, and guards
do_task(Result) :-
    cast_impl(conjure(git(commit("message"))), Result).
```

**Why**: `magic_cast` ensures grounding, executes hooks, and enforces uniqueness.

### 2. `verify` vs `please_verify`

**`verify/1`** = AUTO-GENERATED by `::` operator (NEVER written manually)

**`please_verify/1`** = Verification **INVOCATION** (appears in BODY)
```prolog
% ✅ CORRECT - invoking verification in spell implementation
cast_impl(conjure(project(create(Name))), Result) :-
    please_verify(component(Entity, project_git_repo, GitRepo)),
    please_verify(component(Entity, project_nix_flake, NixFlake)),
    verify_combination(GitRepo, NixFlake),
    Result = ok(created(Name)).

% ❌ WRONG - doesn't ensure grounding
cast_impl(conjure(project(create(Name))), Result) :-
    component(Entity, project_git_repo, GitRepo),  % Not grounded!
    component(Entity, project_nix_flake, NixFlake).
```

**Why**: `please_verify` ensures component exists AND is grounded before verification.

### 3. NEVER Use Bare Predicates in Bodies

```prolog
% ❌ FORBIDDEN in predicate bodies:
cast_impl(...)          % Use magic_cast
component(E, prop, V)   % Use please_verify(component(...))
verify(component(...))  % Use please_verify(component(...))
```

## Spell Registration with register_spell/6

**Every spell uses `register_spell/6` with inline implementation:**

```prolog
register_spell(
    conjure(git(commit)),
    input(git(commit(message('Message')))),
    output(either(
        ok(committed(hash('Hash'))),
        error(commit_failed('Reason'))
    )),
    "Create a git commit with the given message",
    [],
    implementation(conjure(git(commit(Message))), Result, (
        % Implementation here - fully grounded Message available
        atom_string(Message, MessageStr),
        process_create(path(git), ['commit', '-m', MessageStr], []),
        Result = ok(committed(hash("abc123")))
    ))
).
```

**What register_spell/6 generates:**
- `component(conjure, ctor, git(commit))` - Constructor registration
- `component(conjure(git(commit)), docstring, "...")` - Documentation
- `component(conjure(git(commit)), format_input, ...)` - Input format
- `component(conjure(git(commit)), format_output, ...)` - Output format
- `component(conjure(git(commit)), spell_options, [])` - Options
- `cast_impl(conjure(git(commit(Message))), Result) :- (...)` - Implementation

**First argument MUST be constructor with NO variables:**
```prolog
% ✅ CORRECT
register_spell(conjure(git(commit)), ...)

% ❌ WRONG - has variable
register_spell(conjure(git(commit(Message))), ...)
```

### Spell Options

Spells can specify options in the 5th argument:

```prolog
register_spell(
    conjure(db(create)),
    input(db(create(file('File'), schema('Schema')))),
    output(ok(db(sqlite(file('File'))))),
    "Create a SQLite database",
    [session_persistent(has(db(sqlite)))],  % ← Spell options
    implementation(...)
).
```

**Available options:**
- `session_persistent(ComponentProperty)` - Auto-persist successful results to active session
  - On successful `ok(ResultData)`, creates: `component(session(SessionId), ComponentProperty, ResultData)`
  - Appends to session's `semantics.pl` file
  - Session hooks automatically handle this (see `src/session.pl`)

## Spell Invocation Patterns

### From Prolog (Internal)

```prolog
% Direct invocation
magic_cast(conjure(git(commit("message"))), Result).

% FUTURE: do-notation for monadic chaining (NOT YET IMPLEMENTED)
% See poc/do_notation_final.pl for planned syntax
do((
    V1 <- conjure(backend(run)),
    V2 <- conjure(frontend(run)),
    Result = ok(V1, V2)
)).
```

### From Python/MCP (External)

**Current pattern** - Spell signature with explicit variable bindings:

```python
# Perceive query with variables
grimoire.perceive("git(status(Root))", {"Root": "/path/to/repo"})

# Conjure spell with variables
grimoire.conjure("git(commit(Message))", {"Message": "Initial commit"})

# No variables needed
grimoire.perceive("interface(entities)", {})
```

**Why this pattern:**
1. **Self-documenting** - Signature shows what variables are required
2. **Type-checkable** - Can validate args match pattern structure
3. **Discoverable** - Docstring shows example with variable names
4. **Composable** - Works naturally with nested terms

**Template Filling:**
The Python bridge fills variables in the spell signature using provided args dict:
- `"git(status(Root))"` + `{"Root": "/tmp"}` → `git(status("/tmp"))`
- Missing required variables throw `error(missing_template_arg(Var), _)`

## Session Domain Patterns

The session domain provides persistent state across Grimoire invocations:

### Session Structure

```
$GRIMOIRE_DATA/sessions/{session_id}/
├── semantics.pl          # Session knowledge (components)
└── activity_log.jsonl    # Spell cast history
```

### Session Components

```prolog
% Session entity self-reference
component(session(SessionId), self, semantic(folder(SessionFolder)))
    ==> component(session(SessionId), session_directory, SessionFolder),
        component(session(SessionId), session_semantics_path, SemanticsPath),
        component(session(SessionId), session_activity_log_path, LogPath).
```

### Session Hooks

**Automatic spell logging:**
- Every spell cast is logged to `activity_log.jsonl` via `cast_post_hook`
- Includes timestamp, spell term, and result

**Automatic persistence:**
- Spells with `session_persistent` option auto-save results to session
- Example: `db(create)` saves database info for later recovery

### Session Workflow

```prolog
% Create session
magic_cast(conjure(session(create(id('my-project')))), Result).

% Switch to session (loads its semantics.pl)
magic_cast(conjure(session(switch(id('my-project')))), Result).

% Session-persistent spells auto-save state
magic_cast(conjure(db(create(file('/tmp/db.sqlite'), schema([])))), Result).
% → Automatically creates: component(session('my-project'), has(db(sqlite)), ...)

% Export session for backup
magic_cast(conjure(session(export(id('my-project'), path('/tmp/backup.tar.gz')))), Result).

% Import session to restore
magic_cast(conjure(session(import(path('/tmp/backup.tar.gz')))), Result).
```

## Testing Patterns

### File-Based Test Entities

**Tests NEVER use `assertz`/`retract` for component/entity facts.**

All test knowledge lives in `.pl` files:

```prolog
% src/tests/git_test_entity.pl
:- self_entity(test_entities(git)).

entity(test_basic_git).

component(test_basic_git, has(git(repository)), git(repository([
    root('/tmp/test_repo'),
    branch(main)
]))).
```

### Test Implementation

```prolog
% src/git.plt
:- load_entity(semantic(file('@/src/tests/git_test_entity.pl'))).

:- begin_tests(git_semantics).

test(git_repository_verification, [
    setup(setup_test_repo),
    cleanup(cleanup_test_repo)
]) :-
    % Use user: prefix in tests
    user:please_verify(component(test_basic_git, has(git(repository)), _)),
    user:please_verify(component(test_basic_git, git_repository_root, _)).

test(git_commit_spell) :-
    user:magic_cast(conjure(git(commit("test"))), Result),
    assertion(Result = ok(_)).

:- end_tests(git_semantics).

% Setup creates ONLY filesystem resources
setup_test_repo :-
    TestPath = '/tmp/test_repo',
    make_directory(TestPath),
    process_create(path(git), ['init'], [cwd(TestPath)]).

cleanup_test_repo :-
    delete_directory_and_contents('/tmp/test_repo').
```

**Setup/Cleanup Rules:**
- ✅ Create/delete directories and files
- ✅ Run `process_create(path(git), ...)`
- ✅ Create session entities for session tests
- ❌ NEVER `assertz(component(...))`
- ❌ NEVER `retract(entity(...))`

**Tests use `user:` prefix:**
- Inside `begin_tests(...) ... end_tests(...)` blocks: `user:please_verify(...)`, `user:magic_cast(...)`
- Outside test blocks: no `user:` prefix

### Testing Sessions

```prolog
% Setup can create test sessions
setup_test_session :-
    TestSessionId = test_session_xyz,
    magic_cast(conjure(session(create(id(TestSessionId)))), _).

cleanup_test_session :-
    TestSessionId = test_session_xyz,
    magic_cast(conjure(session(delete(id(TestSessionId)))), _).

test(session_persistence, [
    setup(setup_test_session),
    cleanup(cleanup_test_session)
]) :-
    % Test that session-persistent spells work
    user:magic_cast(conjure(session(switch(id(test_session_xyz)))), _),
    user:magic_cast(conjure(db(create(file('/tmp/test.db'), schema([])))), Result),
    Result = ok(_),
    % Verify component was persisted
    user:please_verify(component(session(test_session_xyz), has(db(sqlite)), _)).
```

## Critical Implementation Patterns

### Uniqueness Enforcement

Use `findall` to ensure exactly one match and throw informative errors:
```prolog
% ✅ CORRECT - enforces exactly one, throws on 0 or 2+
findall(X, condition(X), Matches),
(   Matches = [] -> throw(error(not_found, ...))
;   Matches = [Unique] -> use(Unique)
;   throw(error(ambiguous(Matches), ...))
).

% ❌ WRONG - leaves choicepoints
condition(X), !, use(X).
```

### Debugging Choicepoints

When PLUnit warns about choicepoints:
```bash
# Count solutions
findall(true, my_predicate(...), Solutions), length(Solutions, N)

# Check for multiple implementations or matches
# Fix with findall pattern above or cut AFTER validation
```

### Error Handling in Spells

```prolog
% ✅ CORRECT - Explicit error returns
implementation(conjure(git(commit(Message))), Result, (
    (exists_directory('.git')
    -> (atom_string(Message, MessageStr),
        process_create(path(git), ['commit', '-m', MessageStr], []),
        Result = ok(committed))
    ;  Result = error(not_a_git_repository))
)).

% ❌ WRONG - Unhandled exceptions
implementation(conjure(git(commit(Message))), Result, (
    atom_string(Message, MessageStr),
    process_create(path(git), ['commit', '-m', MessageStr], []),  % Throws on error!
    Result = ok(committed)
)).
```

## Python Bridge Pattern

For domains integrating Python (golems, protocol_clients):

```
domain/
├── semantics.pl        # Pure Prolog (NO py_call)
├── python_bridge.pl    # ALL py_call + decoding to Prolog
├── client.py          # Python implementation
└── semantics.plt      # Tests
```

**`semantics.pl` must be PURE PROLOG - NO `py_call`:**

```prolog
% semantics.pl - PURE PROLOG ONLY
:- use_module('python_bridge.pl', [golem_function/2]).

register_spell(
    conjure(golem(task)),
    input(golem(task(args('Args')))),
    output(ok(result('Result'))),
    "Execute golem task",
    [],
    implementation(conjure(golem(task(Args))), Result, (
        golem_function(Args, DecodedResult),  % Already Prolog terms!
        Result = ok(DecodedResult)
    ))
).
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

## Git Practices

- **NEVER** use `git add .` or `git add -A`
- **ALWAYS** use `git add -u` or add files individually
- Use `git mv` instead of `mv` for tracked files
- Check `git status` before committing
- Write meaningful commit messages

## Testing Workflow

```bash
# For users: Use grimoire test command
./grimoire test              # Run all tests
./grimoire test git          # Run specific domain
./grimoire test 'test_name'  # Run specific test

# Use exec for interactive testing
./grimoire exec
```

## Common Anti-Patterns

### ❌ Anti-Pattern 1: Writing verify/1 manually
```prolog
% WRONG - verify/1 is auto-generated by :: operator
verify(component(E, git_repository_root, Root)) :- ...
```

### ❌ Anti-Pattern 2: Using cast_impl in bodies
```prolog
% WRONG
my_function(Result) :-
    cast_impl(conjure(something(X)), R1).  % Use magic_cast!
```

### ❌ Anti-Pattern 3: Using bare component in bodies
```prolog
% WRONG
some_predicate :-
    component(E, prop, V).  % Use please_verify(component(...))!
```

### ❌ Anti-Pattern 4: Missing register_spell
```prolog
% WRONG - all spells need register_spell
cast_impl(conjure(new_spell(X)), Result) :- ...
```

### ❌ Anti-Pattern 5: Runtime assertions in tests
```prolog
% WRONG
setup_test :-
    assertz(entity(test_entity)).  % Use file-based entities!
```

### ❌ Anti-Pattern 6: Python objects in semantics.pl
```prolog
% WRONG - semantics.pl should never see Python objects
cast_impl(conjure(thing(X)), Result) :-
    py_call(module:func(X), PyObj),  % Should be in python_bridge.pl!
    Result = ok(PyObj).
```

### ❌ Anti-Pattern 7: Unclear spell invocation
```python
# WRONG - unclear what variables are needed
perceive("git(status)", {})  # Error: missing Root

# CORRECT - self-documenting
perceive("git(status(Root))", {"Root": "/path/to/repo"})
```

### ❌ Anti-Pattern 8: Bypassing session hooks
```prolog
% WRONG - modifying session state directly
append_to_file(SessionSemanticsPath, 'component(...).\n').

% CORRECT - let session_persistent option handle it
register_spell(
    conjure(my(spell)),
    ...,
    [session_persistent(my_component_type)],  % Auto-saves on success
    implementation(...)
).
```

## Key Principles

1. **NEVER write `verify/1` manually** - Use `::` operator
2. **`cast_impl` = auto-generated, `magic_cast` = invoke**
3. **`verify` = auto-generated, `please_verify` = invoke**
4. **Every spell uses `register_spell/6` with inline implementation**
5. **No `assertz`/`retract` in tests - file-based entities only**
6. **Tests use `user:please_verify` and `user:magic_cast`**
7. **Python objects never leak into `semantics.pl`**
8. **Generative (`==>`) expands to multiple components**
9. **Discriminative (`::`) auto-generates verify clauses**
10. **All knowledge lives in files, not runtime assertions**
11. **Spell invocations show variables explicitly** - `"spell(Var1, Var2)"` + args dict
12. **Sessions provide persistence** - Use `session_persistent` option for stateful spells
13. **Hooks handle cross-cutting concerns** - Logging, persistence, etc.

## File Structure

```
domain/
├── semantics.pl        # DSL schemas (==> and ::), spell registrations
├── semantics.plt       # PLUnit tests (NO verify/1 clauses)
└── python_bridge.pl    # (if needed) Python integration
```

## When Adding New Features

1. Add DSL schema in `semantics.pl` using `==>` and `::` operators
2. Add `register_spell/6` for any new spells (implementation inline)
3. Consider if spell needs `session_persistent` option
4. Create file-based test entities in `src/tests/`
5. Write PLUnit tests using `user:please_verify` and `user:magic_cast`
6. **NEVER write `verify/1` clauses manually**
7. Update MCP/CLI interfaces if adding user-facing features

## Future Features

### Do-Notation (Planned)

See `poc/do_notation_final.pl` for planned monadic spell composition syntax:

```prolog
% FUTURE: Explicit monadic chaining (NOT YET IMPLEMENTED)
my_complex_spell(Result) :-
    do((
        V1 <- conjure(backend(run)),
        V2 <- conjure(frontend(run)),
        some_property(V1, Prop),
        V3 <- conjure(process(Prop)),
        Result = ok(V3)
    )).
```

**Status:** Proof-of-concept complete, not yet integrated into main codebase.

**Benefits:**
- Automatic error short-circuiting
- Clean sequential spell composition
- Makes spell dependencies explicit

## Documentation References

- **`README.md`** - User-facing documentation
- **`DESIGN.md`** - Architectural deep dive
- **`src/GRIMOIRE.md`** - LLM system instructions for using Grimoire
- **`overhaul-2/`** - Domain migration plans
- **`plans/`** - Feature plans and design documents
- **`poc/`** - Proof-of-concept implementations

## Common Patterns Summary

### Component Patterns
```prolog
% External activation (OS → KB)
component(E, git_branch, Branch) :- ...

% KB verification (KB → OS check)
component(_, git_root, Root) :: exists_directory(Root).

% KB expansion (KB → KB)
component(E, has(X), Data) ==> component(E, x_field, Field) :- ...
```

### Spell Patterns
```prolog
% Basic spell
register_spell(
    conjure(domain(action)),
    input(...),
    output(...),
    "Docstring",
    [],  % No options
    implementation(conjure(domain(action(Args))), Result, (...))
).

% Session-persistent spell
register_spell(
    conjure(domain(action)),
    input(...),
    output(...),
    "Docstring",
    [session_persistent(component_type)],  % Auto-save results
    implementation(...)
).
```

### Test Patterns
```prolog
% File-based test entity
:- load_entity(semantic(file('@/src/tests/domain_test.pl'))).

test(my_test, [setup(setup_fn), cleanup(cleanup_fn)]) :-
    user:magic_cast(conjure(domain(action(...))), Result),
    assertion(Result = ok(_)).
```

### Invocation Patterns
```python
# From Python/MCP
grimoire.conjure("domain(action(Var1, Var2))", {"Var1": val1, "Var2": val2})
grimoire.perceive("domain(query(Var))", {"Var": value})
```

```prolog
% From Prolog
magic_cast(conjure(domain(action(val1, val2))), Result).
magic_cast(perceive(domain(query(value))), Result).
```

## Debugging Tips

1. **Use `core_dump/1`** to inspect full system state
2. **Check spell docstrings** with `docstring(SpellCtor)`
3. **Verify components** with `please_verify(component(...))`
4. **Trace spell execution** in session activity logs
5. **Test incrementally** - write tests as you develop
6. **Use `user:` prefix** in PLUnit test blocks
7. **Check for choicepoints** - PLUnit will warn you

## Getting Help

When stuck:
1. Check this document first
2. Look at existing domain implementations for patterns
3. Read test files to see usage examples
4. Examine `src/ecs_kernel.pl` for low-level details
5. Check `plans/` directory for feature documentation
