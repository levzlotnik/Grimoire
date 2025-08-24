## Core Philosophy
- Knowledge-based OS built on Entity-Component-System (ECS) architecture
- Semantic knowledge lives in Prolog files (`semantics.pl`)
- Knowledge is local to where it belongs (git-like distributed patterns)
- No runtime asserts – all knowledge lives in files
- Each domain uses its natural tools while integrating through common patterns

## ECS Architecture Patterns
- `entity/1` to declare things that exist
- `component/3` to express relationships between things
- Extensible sum types via `component(Type, ctor, ...)`
- Relation meta-patterns via `component(Type, relation_pattern, Pattern)`
- Don't use `user:` namespacing for `entity/1`, `component/3`, `docstring/2`

## File Structure Conventions
- `semantics.pl/semantics.plt` pattern for logic/tests
- Self-entity declarations: `:- self_entity(EntityName).`
- Single-arity loading API: `load_entity(semantic(folder/file(...)))`
- Explicit entity declarations, no magic binding

## Testing & CLI Workflows
- Testing: `./grimoire test` (full suite) or `./grimoire test <test name>` (specific)
- Use `./grimoire exec` instead of `swipl` - loads system components
- Spell system: `./grimoire cast "conjure(...)"` for mutations, `./grimoire perceive "..."` for queries

## Environment Variables
- `GRIMOIRE_ROOT`: Directory from which `./grimoire` script is executed (project directory)
- `GRIMOIRE_DATA`: Data storage directory for sessions, databases, etc. (defaults to `$HOME/.grimoire`)

## Git Practices
- Never use `git add .` or `git add -A` - use `git add -u` or add files individually
- Session-based workflows with SQLite command logging
- File-based workspaces under `${GRIMOIRE_DATA}/sessions/`

## Nix Command Patterns
- Building: `nix(build(Installable))` or fallback `nix(build("."))`
- Running: `nix(run(Installable))` or `nix(run(Target, Args))`
- Testing: `nix(check("."))` or discovered test targets
- Templates use `nix run .#<command>` pattern for apps
- Canonical operations ensure reproducibility across environments

## Domain Integration
- Mount semantics via `mount_semantic(file/folder(...))`
- Passive loading: `passive_load(Entity, semantic(Source))`
- Core domains auto-load: git, nix, fs, project, session, db

- Don't forget to add newlines to the end of new files
- NEVER EVER DISABLE TESTS WITHOUT MY PERMISSION
- If you're `mv`ing a file in tree, use `git mv` instead of just `mv`