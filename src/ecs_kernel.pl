:- use_module(library(process)).
:- use_module(library(yall)).
:- use_module(library(apply)).
:- use_module(library(strings)).
:- use_module(library(filesex)).

% Core ECS predicates - allow extension across files
:- dynamic([
    entity/1,
    component/3,
    docstring/2,
    load_entity/2          % Allow subsystems to extend entity loading
], [
    discontiguous(true),
    multifile(true)
]).

% === GRIMOIRE_ROOT PATH RESOLUTION ===
% Provides @/ prefix convention for GRIMOIRE_ROOT-relative paths

% Get Grimoire root directory from environment
grimoire_root(Root) :-
    getenv('GRIMOIRE_ROOT', Root).

% Resolve paths with @/ prefix for GRIMOIRE_ROOT-relative paths
% and ./ prefix for current file directory-relative paths
grimoire_resolve_path(Path, Resolved) :-
    (   atom_concat('@/', RelPath, Path) ->
        % @/ prefix means relative to GRIMOIRE_ROOT
        grimoire_root(Root),
        atomic_list_concat([Root, '/', RelPath], Resolved)
    ;   atom_concat('./', RelPath, Path) ->
        % ./ prefix means relative to current file's directory
        grimoire_calling_file_directory(CallingDir),
        atomic_list_concat([CallingDir, '/', RelPath], Resolved)
    ;   % No special prefix - use as-is
        Resolved = Path
    ).

% Get the directory of the file that's currently being loaded/executed
grimoire_calling_file_directory(Dir) :-
    % Get the current directory context from Prolog loading
    (   prolog_load_context(source, File)
    ->  file_directory_name(File, Dir)
    ;   prolog_load_context(directory, Dir)
    ->  true
    ;   % Final fallback to current working directory
        working_directory(Dir, Dir)
    ).

% Load files with @/ support
grimoire_ensure_loaded(File) :-
    grimoire_resolve_path(File, Resolved),
    ensure_loaded(Resolved).

% Passive loading system - entities exist but need explicit loading
passive_load(Entity, semantic(Source)) :-
    assertz(entity(Entity)),
    assertz(component(Entity, to_be_loaded, semantic(Source))),
    % Add default docstring if none exists yet
    (\+ docstring(Entity, _) ->
        format(string(DefaultDoc), "To get a full view of the entity please call load_entity(~w, semantic(~w))", [Entity, Source]),
        assertz(docstring(Entity, DefaultDoc))
    ;
        true
    ).

% Core ECS patterns
entity(component).
component(component, relation_pattern, ctor).
entity(ctor).
docstring(ctor,
    {|string(_)||
    A constructors set component relation pattern.
    Essentially, this is used to represent variants/sum types.

    Example:
        component(command, ctor, shell).
        component(command, ctor, mkdir).
        % Now - `command(shell(...))` and `command(mkdir(...))`
        % are two different variants/constructors.
    |}
).

% Fact schema infrastructure
component(component, relation_pattern, fact_schema).
entity(fact_schema).
docstring(fact_schema,
    {|string(_)||
    A fact schema defines domain-specific declarative patterns that users
    can use in their semantics.pl files. These patterns expand into rich
    ECS structures through domain-provided rules.

    Format: component(Entity, has(Domain(Type)), DomainData)

    The domain provides:
    - ECS expansion rules in semantics.pl (generative/covariant)
    - Verification predicates in semantics.plt (discriminative/contravariant)

    Example:
        % User writes in their semantics.pl:
        component(my_app, has(nix(flake)), nix(flake(ref("./")))).

        % Domain's semantics.pl expands to:
        component(my_app, nix_flake_ref, "./").
        component(my_app, nix_package, "hello").  % auto-discovered

        % Domain's semantics.plt verifies:
        verify(component(my_app, has(nix(flake)), nix(flake(ref("./"))))).
    |}
).


component(component, relation_pattern, option).
entity(option).
docstring(option,
    {|string(_)||
    An options-set component relation pattern.
    Would be used in a similar fashion to keyword-like, optional arguments
    in an options list.
    Format: option(Unique)
        Unique := unique | group

    Note:
        For required arguments, it's preferred that
        term structure regular arguments are used instead of
        option lists.

    Example:
        % `-p, --parents` option for `mkdir`
        component(mkdir, option(unique), parents).
        % `-I` option for `gcc`
        component(gcc, option(not_unique), include_directory)
    |}
).

% Base docstrings for ECS
docstring(entity,
    {|string(_)||
    Declares something as an entity within the system.
    Format: entity(Thing).
    Examples:
      entity(folder("/home/user/docs"))
      entity(file("document.txt"))
    |}
).

docstring(component,
    {|string(_)||
    Defines a hierarchical relationship between entities, where one entity is a
    component of another. ComponentName must be an atom for efficient querying.
    Format: component(Entity, ComponentName, Value)
    |}
).

% Constructor docstring helpers
make_ctors_docstring(Entity, Docstring) :-
    entity(Entity),
    findall(
        CtorDoc,
        (
            component(Entity, ctor, Ctor),
            get_ctor_docstring(Entity, Ctor, CtorDoc)
        ),
        CtorsDocs
    ),
    atomic_list_concat(CtorsDocs, '\n\n', DocsUnindent),
    indent_lines('  ',DocsUnindent, Docstring).

get_ctor_docstring(Entity, Ctor, Doc) :-
    format(string(Atom), "~w(~w)", [Entity, Ctor]),
    term_to_atom(Term, Atom),
    docstring(Term, CtorDoc),
    format(string(Doc), "~w: ~w", [Atom, CtorDoc]).

% === COMPONENT INTEGRATION ===
% Integrate baseline predicates into queryable component system

% docstring/2 becomes queryable as component
component(Entity, docstring, Doc) :-
    docstring(Entity, Doc).

% entity/1 becomes queryable as component
component(Entity, defined, true) :-
    entity(Entity).

% Semantic mounting system
:- dynamic mounted_semantic/2.  % mounted_semantic(Path, Module)

% Enhanced semantic mounting predicates
mount_semantic_file(Path) :-
    % Resolve @/ paths first
    grimoire_resolve_path(Path, ResolvedPath),
    % Get absolute path and check file exists
    absolute_file_name(ResolvedPath, AbsPath),
    (exists_file(AbsPath) ->
        % Only mount if not already mounted
        (\+ mounted_semantic(AbsPath, _) ->
            % Load the semantic file
            catch(
                ensure_loaded(AbsPath),
                Error,
                (print_message(error, Error), fail)
            ),
            % Record the mounting
            assertz(mounted_semantic(AbsPath, AbsPath))
        ;
            true  % Already mounted, silently succeed
        )
    ;
        throw(error(existence_error(source_sink, Path), _))
    ).

mount_semantic_dir(Path) :-
    % Resolve @/ paths first
    grimoire_resolve_path(Path, ResolvedPath),
    absolute_file_name(ResolvedPath, AbsPath),
    atomic_list_concat([AbsPath, '/semantics.pl'], SemanticFile),
    (exists_file(SemanticFile) ->
        mount_semantic_file(SemanticFile)
    ;
        throw(error(no_semantics_file(AbsPath), _))
    ).

docstring(mount_semantic,
    {|string(_)||
    Mounts a semantics.pl file for querying.
    Format: mount_semantic(Path).
    - Loads the module at Path
    - Creates a unique module name based on path
    - Tracks the mounting in mounted_semantic/2
    |}
).

mount_semantic(Source) :-
    (Source = file(Path) ->
        mount_semantic_file(Path)
    ; Source = folder(Path) ->
        mount_semantic_dir(Path)
    ;
        throw(error(invalid_source(Source), _))
    ).

docstring(unmount_semantic,
    {|string(_)||
    Unmounts a previously mounted semantics.pl file.
    Format: unmount_semantic(Path).
    - Unloads the module at Path
    - Removes mounting from mounted_semantic/2
    |}
).

unmount_semantic(Path) :-
    absolute_file_name(Path, AbsPath),
    mounted_semantic(AbsPath, _),
    % Unload the file properly
    unload_file(AbsPath),
    % Remove from mounting registry
    retractall(mounted_semantic(AbsPath, _)).

list_mounted_semantics(Paths) :-
    findall(Path, mounted_semantic(Path, _), Paths).

% Base entity loading cases for semantic files/folders

% Helper for project semantics files - prevents duplicate loading
ensure_load_entity(Entity, Source) :-
    % Check if already loaded (has source component)
    (component(Entity, source, Source) ->
        true  % Already loaded
    ;
        load_entity(Entity, Source)  % Load it
    ).

% Absolute entities that declare themselves directly
load_entity(semantic(Source)) :-
    mount_semantic(Source).

docstring(load_entity,
    {|string(_)||
    Loads semantic entities with single-arity API for absolute entities.

    Format: load_entity(semantic(Source))
    - For absolute entities like git that declare themselves directly
    - Simply mounts the semantic source without transformation

    Format: passive_load(Entity, semantic(Source))
    - Declares entity with to_be_loaded component
    - Entity exists immediately but shows only to_be_loaded info
    - User must explicitly call load_entity to get full functionality

    Examples:
        % Load semantic files directly
        ?- load_entity(semantic(file("src/git.pl"))).
        ?- entity(git).  % Now available

        % Load semantic folders
        ?- load_entity(semantic(folder("src/nix"))).
        ?- entity(nix).  % Now available
    |}
).

% === SELF-ENTITY INTROSPECTION ===

% Allow semantic files to declare themselves and find their own path
self_entity(Entity) :-
    % Get the current file and directory being loaded
    prolog_load_context(source, FilePath),
    prolog_load_context(directory, Dir),

    % Assert the entity
    assertz(entity(Entity)),

    % Create self component based on file type
    file_base_name(FilePath, FileName),
    (FileName = 'semantics.pl' ->
        % It's a semantics.pl file, use the directory
        assertz(component(Entity, self, semantic(folder(Dir)))),
        % Check for README.md and use as docstring if exists
        directory_file_path(Dir, 'README.md', ReadmePath),
        (exists_file(ReadmePath) ->
            read_file_to_string(ReadmePath, ReadmeContent, []),
            assertz(docstring(Entity, ReadmeContent))
        ; true)
    ;
        % Regular semantic file
        assertz(component(Entity, self, semantic(file(FilePath))))
    ).

% Variant with explicit docstring
self_entity(Entity, Docstring) :-
    % Get the current file and directory being loaded
    prolog_load_context(source, FilePath),
    prolog_load_context(directory, Dir),

    % Assert the entity
    assertz(entity(Entity)),

    % Create self component based on file type
    file_base_name(FilePath, FileName),
    (FileName = 'semantics.pl' ->
        % It's a semantics.pl file, use the directory
        assertz(component(Entity, self, semantic(folder(Dir))))
    ;
        % Regular semantic file
        assertz(component(Entity, self, semantic(file(FilePath))))
    ),

    % Assert the provided docstring directly (no retract needed)
    assertz(docstring(Entity, Docstring)).

docstring(self_entity,
    {|string(_)||
    Declares an entity and automatically assigns its semantic source as a self component.
    This allows semantic files to be self-describing about their location.

    Format: self_entity(Entity)
    - Asserts entity(Entity)
    - Uses prolog_load_context/2 to find the current file path and directory
    - If file is 'semantics.pl': adds component(Entity, self, semantic(folder(Directory)))
    - If file is other: adds component(Entity, self, semantic(file(FilePath)))

    Usage in semantic files:
        % In src/git.pl
        :- self_entity(git).
        % Result: component(git, self, semantic(file("src/git.pl")))

        % In templates/rust/semantics.pl
        :- self_entity(rust_template).
        % Result: component(rust_template, self, semantic(folder("templates/rust")))
    |}
).

% === SEMANTIC ENTITY LOOKUP ===

% Find entity ID associated with a semantic source
% This is the ONLY place that should query component/3 with unbound first argument
semantic_entity_id(semantic(Source), EntityId) :-
    component(EntityId, self, semantic(Source)).

docstring(semantic_entity_id,
    {|string(_)||
    Finds the entity ID associated with a semantic source.
    This is the only predicate that should query component/3 with unbound first argument.

    Format: semantic_entity_id(semantic(Source), EntityId)
    - Source: file(Path) or folder(Path)
    - EntityId: The entity that declared itself with this semantic source

    Examples:
        ?- semantic_entity_id(semantic(file("/path/to/git.pl")), Entity).
        Entity = git.

        ?- semantic_entity_id(semantic(folder("/path/to/templates/rust")), Entity).
        Entity = rust_template.
    |}
).

