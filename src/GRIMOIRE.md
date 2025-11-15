# Grimoire System Instructions for LLMs

## What is Grimoire?

Grimoire is a knowledge-based operating system where **all state is declarative knowledge**. The knowledge base is written in Prolog and exposed via MCP tools.

## Internal Knowledge Structure

Understanding how knowledge is represented helps you query it effectively.

### Entities and Components

The knowledge base uses three fundamental predicates:

```prolog
% Declare an entity exists
entity(git).

% Associate typed properties with entities
component(git, subsystem, version_control).
component(git, ctor, commit).
component(git, ctor, clone).

% Document entities
docstring(git, "Version control and knowledge evolution tracking").
```

### Component Expansion with `==>`

High-level component declarations automatically expand into queryable properties:

```prolog
% From src/git.pl - Repository component expands to queryable properties
component(E, has(git(repository)), git(repository(Spec)))
    ==> (component(E, git_repository_remote_name, Name) :-
            member(remote(Name, _), Spec)),
        (component(E, git_repository_remote_url, URL) :-
            member(remote(_, URL), Spec)),
        (component(E, git_repository_branch, Branch) :-
            member(branch(Branch), Spec)).
```

This means when you query `component(Entity, git_repository_branch, Branch)`, the system automatically extracts it from the high-level `has(git(repository))` declaration.

### Component Verification with `::`

Components can be verified against OS reality:

```prolog
% From src/git.pl - Verify root is a real git repository
component(_, git_repository_root, Root)
    :: exists_directory(Root),
       atomic_list_concat([Root, '/.git'], GitDir),
       exists_directory(GitDir).
```

The `::` operator declares verification constraints that are checked when components are validated.

### DSL Schema Registration

Domains can register declarative schemas:

```prolog
% From src/fs.pl - Filesystem structure schema
register_dsl_schema(
    fs,
    has(fs(structure)),
    signature(fs(structure(items(Items:list(fs_entry))))),
    "Declarative filesystem structure specification",
    (
        component(Entity, has(fs(structure)), fs(structure(Items)))
            ==> (component(Entity, fs_structure_file, file_spec(Path, Opts)) :-
                    extract_file_specs(Items, FileSpecs),
                    member(file_spec(Path, Opts), FileSpecs)),
                (component(Entity, fs_structure_folder, folder_spec(Path)) :-
                    extract_folder_specs(Items, FolderSpecs),
                    member(folder_spec(Path), FolderSpecs)),
                (component(Entity, fs_structure_glob, glob_spec(Pattern)) :-
                    extract_glob_specs(Items, GlobSpecs),
                    member(glob_spec(Pattern), GlobSpecs))
    )
).
```

### Spell Registration

Operations are registered as spells with type signatures:

```prolog
% From src/git.pl - Commit spell
register_spell(
    conjure(git(commit)),
    input(git(commit(git_root('Root'), message('Message')))),
    output(either(
        ok(committed(hash('Hash'))),
        error(git_error('Reason'))
    )),
    "Create a git commit with the given message",
    [],
    implementation(conjure(git(commit(git_root(Root), message(Message)))), Result, (
        % Implementation here - Root and Message are grounded
        atom_string(Message, MessageStr),
        phrase(git_args(commit(Message)), Args),
        magic_cast(conjure(executable_program(program(git), args(Args))), ExecResult),
        (ExecResult = ok(_) ->
            Result = ok(committed(hash("generated")))
        ; ExecResult = error(E) ->
            Result = error(git_error(E))
        )
    ))
).
```

## Using Grimoire via MCP

You interact through MCP tools that expose the knowledge base and spell system.

### Discovering Spells

```python
# List all registered spells
list_spells()
# Returns: conjure(git(commit)), perceive(git(status)), ...

# Get spell metadata (shows input format with template variables)
sauce_me("git(commit)")
# Returns: {
#   "input": "git(commit(git_root('Root'), message('Message')))",
#   "output": "either(ok(committed(hash('Hash'))), error(git_error('Reason')))",
#   "docstring": "Create a git commit with the given message"
# }
```

### Invoking Spells

Spells use template variable filling:

```python
# Conjure (state-changing)
conjure("git(commit(git_root(Root), message(Message)))", {
    "Root": "/path/to/repo",
    "Message": "Fix bug"
})
# Returns: ok(committed(hash("abc123")))

# Perceive (read-only)
perceive("git(status(git_root(Root)))", {"Root": "/path/to/repo"})
# Returns: ok(status_info(branch(main), working_status(clean), files([])))
```

Template variables (capitalized in signature) are filled with values from the args dict.

### Querying Components

```python
# List all entities
entities()
# Returns: [git, nix, db, session, project, ...]

# Get component types for an entity
component_types("git")
# Returns: [ctor, subsystem, subcommand, ...]

# Get all components of a specific type
components("git", "ctor")
# Returns: [commit, clone, add, push, pull, ...]

# Get documentation
docstring("git")
# Returns: "Version control and knowledge evolution tracking"
```

### Sessions: Persistent State Across Compactions

Sessions track spell execution history and persist state:

```python
# Create session
conjure("session(create(name(Name)))", {"Name": "my_project"})

# Switch to session (loads its knowledge)
conjure("session(switch(name(Name)))", {"Name": "my_project"})

# After compaction: recover context
perceive("session(history(session_name(Name)))", {"Name": "my_project"})
# Returns: [{
#   "timestamp": 1234567890,
#   "spell": "conjure(git(commit(...)))",
#   "result": "ok(committed(...))"
# }, ...]
```

Use session history after compaction to understand:
- What operations were performed
- What state was created/modified
- What the user was working on

## Common Operations

### Git Operations

```python
# Clone repository
conjure("git(clone(url(Url), path(Path)))", {
    "Url": "https://github.com/user/repo",
    "Path": "/tmp/repo"
})

# Stage files
conjure("git(add(git_root(Root), paths(Paths)))", {
    "Root": "/path/to/repo",
    "Paths": ["file1.txt", "file2.txt"]
})

# Commit (note: git_root wrapper required for repo operations)
conjure("git(commit(git_root(Root), message(Message)))", {
    "Root": "/path/to/repo",
    "Message": "Update files"
})

# Check status
perceive("git(status(git_root(Root)))", {"Root": "/path/to/repo"})
```

### Filesystem Operations

```python
# Read file
perceive("fs(read_file(path(Path), start(Start), end(End)))", {
    "Path": "/tmp/config.txt",
    "Start": 1,
    "End": 10
})

# Edit file
conjure("fs(edit_file(file(Path), edits(Edits)))", {
    "Path": "/tmp/file.txt",
    "Edits": [{"insert": {"line": 1, "content": "New line"}}]
})

# Create directory
conjure("fs(mkdir(path(Path), options(Options)))", {
    "Path": "/tmp/newdir",
    "Options": []
})
```

### Database Operations

```python
# Create database
conjure("db(create(file(DbPath), schema(SchemaSpec)))", {
    "DbPath": "/tmp/mydb.db",
    "SchemaSpec": {"file": "schema.sql"}
})

# Query database
perceive("db(query(database(DbPath), sql(SQL)))", {
    "DbPath": "/tmp/mydb.db",
    "SQL": "SELECT * FROM users WHERE age > 18"
})
```

## Key Principles

1. **Knowledge is declarative** - Entities and components are declared in Prolog files
2. **Components expand automatically** - `==>` operator creates queryable properties
3. **Components are verifiable** - `::` operator checks against OS reality
4. **Spells are type-safe** - All operations have registered input/output formats
5. **Template variables show requirements** - Capitalized names indicate what args to provide
6. **Results are structured** - Always `ok(...)` for success or `error(...)` for failure
7. **Sessions persist state** - Use session history to recover context after compaction

## Error Handling

All spells return structured results:

```python
# Success
ok(result(...))

# Failure
error(reason(...))
```

Always check the result structure and handle errors appropriately.

## Further Documentation

- `README.md` - Building and using Grimoire
- `docs/ecs.md` - Entity-Component-System deep dive
- `docs/spells.md` - Spell system internals
- `docs/session.md` - Session domain and persistence
- `CLAUDE.md` - Development patterns for contributing
