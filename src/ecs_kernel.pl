:- use_module(library(process)).
:- use_module(library(yall)).
:- use_module(library(apply)).
:- use_module(library(strings)).
:- use_module(library(filesex)).
:- use_module(library(option)).

%% ============================================================================
%% PHASE 3: OPERATOR DEFINITIONS
%% ============================================================================

% Component DSL operators for generative and discriminative flows
:- op(1150, xfx, ==>).  % "implies/expands to" - generative flow (component expansion)
:- op(1160, xfx, '::').  % "such that" - discriminative flow (verification constraints)

% Reading: component(...) ==> expansions :: constraints
% Means: "component implies these expansions, such that these constraints hold"

%% ============================================================================
%% CORE ECS PREDICATES - DYNAMIC DECLARATIONS
%% ============================================================================

% Core ECS predicates - allow extension across files
:- dynamic([
    entity/1,
    component/3,
    docstring/2,
    load_entity/2,          % Allow subsystems to extend entity loading
    verify/1,               % Phase 3: Discriminative verification predicates
    in_please_verify/0      % Phase 3: Guard flag for verify/1
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
        please_verify(component(my_app, has(nix(flake)), nix(flake(ref("./"))))).
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

%% ============================================================================
%% PHASE 3: TERM EXPANSION - COMPONENT DSL
%% ============================================================================

% Case 1: Pattern ==> Expansions :: Verification
term_expansion(
    '::'('==>'(Pattern, ExpansionsComma), VerificationComma),
    Generated
) :-
    Pattern = component(_Entity, _ComponentType, _DomainSpec),
    !,
    comma_to_list(ExpansionsComma, Expansions),
    comma_to_list(VerificationComma, Verifications),

    % GENERATIVE: Generate component rules
    maplist(generate_component_rule(Pattern), Expansions, ComponentRules),

    % DISCRIMINATIVE: Generate verify clause
    generate_verify_clause(Pattern, Expansions, Verifications, VerifyClause),

    flatten(ComponentRules, FlatRules),
    Generated = [FlatRules, VerifyClause].

% Case 2: Pattern ==> Expansions (no explicit verification)
term_expansion(
    '==>'(Pattern, ExpansionsComma),
    Generated
) :-
    Pattern = component(_Entity, _ComponentType, _DomainSpec),
    !,
    comma_to_list(ExpansionsComma, Expansions),
    maplist(generate_component_rule(Pattern), Expansions, ComponentRules),
    generate_verify_clause(Pattern, Expansions, [], VerifyClause),

    flatten(ComponentRules, FlatRules),
    Generated = [FlatRules, VerifyClause].

% Case 3: Pattern :: Verification (leaf verification only)
term_expansion(
    '::'(Pattern, VerificationComma),
    [VerifyClause]
) :-
    Pattern = component(_Entity, _ComponentType, _Value),
    !,
    comma_to_list(VerificationComma, Verifications),
    list_to_conjunction(Verifications, VerifyBody),
    wrap_verify_body_with_error_handling(Pattern, VerifyBody, WrappedBody),
    VerifyClause = (verify(Pattern) :- WrappedBody).

%% ============================================================================
%% PHASE 3: COMPONENT RULE GENERATION
%% ============================================================================

% Case 1: Conditional expansion (component(...) :- Conditions)
generate_component_rule(Pattern, (Head :- Body), Rule) :-
    Head = component(_, _, _),
    !,
    % Generate: Head :- Pattern, Body
    Rule = (Head :- Pattern, Body).

% Case 2: Simple expansion component(...)
generate_component_rule(Pattern, Expansion, Rule) :-
    Expansion = component(_, _, _),
    !,
    % Generate: Expansion :- Pattern
    Rule = (Expansion :- Pattern).

% Case 3: Not a component (skip)
generate_component_rule(_Pattern, _Other, []).

%% ============================================================================
%% PHASE 3: VERIFY CLAUSE GENERATION
%% ============================================================================

generate_verify_clause(Pattern, Expansions, Verifications, (verify(Pattern) :- WrappedBody)) :-
    maplist(wrap_with_verify, Expansions, VerifyCalls),
    append(VerifyCalls, Verifications, AllConditions),
    list_to_conjunction(AllConditions, VerifyBody),
    wrap_verify_body_with_error_handling(Pattern, VerifyBody, WrappedBody).

% Wrap conditional expansion: (component(...) :- Body) becomes (Body -> please_verify(component(...)) ; true)
wrap_with_verify((Head :- Body), (Body -> please_verify(Head) ; true)) :-
    Head = component(_, _, _),
    !.

% Wrap simple expansion: component(...) becomes please_verify(component(...))
wrap_with_verify(Expansion, please_verify(Expansion)) :-
    Expansion = component(_, _, _),
    !.

% Skip non-components
wrap_with_verify(_Other, true).

%% ============================================================================
%% PHASE 3: HELPER PREDICATES
%% ============================================================================

comma_to_list((A, B), [A|Rest]) :- !, comma_to_list(B, Rest).
comma_to_list(A, [A]).

list_to_conjunction([], true).
list_to_conjunction([X], X).
list_to_conjunction([X|Rest], (X, RestConj)) :-
    Rest \= [],
    list_to_conjunction(Rest, RestConj).

% Wrap verification body with error handling - converts failures to exceptions
wrap_verify_body_with_error_handling(Pattern, Body, WrappedBody) :-
    WrappedBody = (
        Body -> true
        ; throw(error(
            verification_failed(Pattern),
            context(verify/1, 'Verification constraints failed')))
    ).

%% ============================================================================
%% PHASE 3: TERM EXPANSION - DSL SCHEMA REGISTRATION
%% ============================================================================

term_expansion(
    register_dsl_schema(Domain, Schema, Signature, Doc, ExpansionBody),
    Generated
) :-
    (atom(Doc) ; string(Doc)),  % Doc can be atom or string
    !,
    % ExpansionBody is (Pattern ==> Expansions :: Verifications)
    % Process it using the same machinery as component DSL
    ExpansionBody = '::'('==>'(Pattern, ExpansionsComma), VerificationComma),
    Pattern = component(_Entity, _ComponentType, _DomainSpec),

    comma_to_list(ExpansionsComma, Expansions),
    comma_to_list(VerificationComma, Verifications),

    % Generate component rules using existing helper
    maplist(generate_component_rule(Pattern), Expansions, ComponentRules),

    % Generate verify clause using existing helper
    generate_verify_clause(Pattern, Expansions, Verifications, VerifyClause),

    flatten(ComponentRules, FlatRules),

    % Generate metadata with docstring
    Metadata = [
        component(dsl_schema, ctor, Schema),
        component(Domain, dsl_schema, Schema),
        component(Schema, signature, Signature),
        component(Schema, provided_by, Domain),
        docstring(Schema, Doc)
    ],

    Generated = [[FlatRules, VerifyClause], Metadata].

docstring(register_dsl_schema,
    {|string(_)||
    Register a DSL schema that provides high-level component patterns for domains.

    Format: register_dsl_schema(Domain, Schema, Signature, Docstring, ExpansionBody)

    Parameters:
    - Domain: atom (e.g., git, db, project)
    - Schema: schema identifier (e.g., has(git(repository)))
    - Signature: type signature (e.g., signature(git(repository('Options'))))
    - Docstring: REQUIRED documentation string
    - ExpansionBody: (Pattern ==> Expansions :: Verifications)

    Generates:
    1. Component expansion rules (generative flow)
    2. Verify clause with please_verify composition (discriminative flow)
    3. Metadata: ctor, signature, provided_by, docstring

    Example:
        register_dsl_schema(
            git,
            has(git(repository)),
            signature(git(repository(root('Root')))),
            "Git repository with root directory",
            (
                component(E, has(git(repository)), git(repository(root(Root))))
                    ==> component(E, git_repository_root, Root)
                    ::  atom(Root), exists_directory(Root)
            )
        ).
    |}
).

%% ============================================================================
%% PHASE 3: COMPONENT-FETCHING please_verify/1
%% ============================================================================

please_verify(component(A, B, C)) :-
    % Entity and Type MUST be grounded
    (ground(A), ground(B)
    -> true
    ; throw(error(
        instantiation_error(component(A, B, C)),
        context(please_verify/1, 'Entity and ComponentType must be grounded')))
    ),

    % Find component - THROW if doesn't exist
    (component(A, B, C)
    -> true
    ; throw(error(
        existence_error(component, component(A, B, C)),
        context(please_verify/1, 'Component does not exist')))
    ),

    % NOW everything must be grounded
    (ground(component(A, B, C))
    -> true
    ; throw(error(
        instantiation_error(component(A, B, C)),
        context(please_verify/1, 'Component value must be grounded after component/3 call')))
    ),

    % Verify with hooks and guards - ignore failures, propagate errors
    % Failure = no verify rule (OK), Exception = verify rule exists but failed (NOT OK)
    setup_call_cleanup(
        assertz(in_please_verify),
        ignore(verify(component(A, B, C))),
        retract(in_please_verify)
    ).

docstring(please_verify,
    {|string(_)||
    Component-fetching verification that ensures component exists, is grounded, and satisfies constraints.

    Format: please_verify(component(Entity, ComponentType, Value))

    Behavior:
    1. Checks Entity and ComponentType are grounded (throws if not)
    2. Calls component/3 to fetch Value (may unify if Value was unbound)
    3. Checks full component is now grounded (throws if not)
    4. Calls verify/1 to check constraints

    Use Cases:
    - Fetch and verify component in one call
    - Compose verifications (composite schemas verify their expansions)
    - Check OS reality (verify/1 checks files exist, DB has valid format, etc.)

    Examples:
        % Fetch and verify git repository root
        please_verify(component(my_app, git_repository_root, Root)).
        % Now Root is bound AND verified to exist on disk

        % Verify composite schema (auto-verifies all expanded components)
        please_verify(component(my_app, has(project(app)), _)).
    |}
).

% Guard: verify can only be called from please_verify (MUST be LAST verify clause!)
verify(Pattern) :-
    \+ in_please_verify,
    !,
    throw(error(
        direct_verify_forbidden(Pattern),
        context(verify/1, 'NEVER call verify/1 directly. Use please_verify/1.')
    )).

%% ============================================================================
%% CORE DUMP: COMPLETE SYSTEM STATE INTROSPECTION
%% ============================================================================

core_dump(CoreDump) :-
    % Collect all components with their verification status
    findall(
        component(A, B, C)-R,
        (component(A, B, C),
         catch(
             (please_verify(component(A, B, C)), R = success),
             E,
             R = failure(E)
         )),
        Ontology
    ),
    % Partition verified components
    findall(C, member(C-success, Ontology), VerifiedOntology),
    % Partition broken components with their errors
    findall(C-E, member(C-failure(E), Ontology), BrokenOntology),
    % The core dump IS the ontology, partitioned by verification status
    CoreDump = core_dump(
        verified(VerifiedOntology),
        broken(BrokenOntology)
    ).

docstring(core_dump,
    {|string(_)||
    Captures complete system state as verified and broken ontology.

    Format: core_dump(core_dump(verified(Components), broken(ComponentErrors)))

    Behavior:
    1. Queries all components in the knowledge base
    2. Attempts verification on each component
    3. Partitions into verified (success) and broken (failure with error)
    4. Returns complete system state

    This is the natural "core dump" for knowledge systems - complete introspection
    of the entire ontology with verification status. Unlike traditional core dumps
    that capture raw memory, this captures semantic knowledge with correctness proofs.

    Use Cases:
    - System health diagnostics (what's broken?)
    - SBOM generation (verified bill of materials)
    - Debugging verification failures
    - Temporal analysis (compare dumps over time)
    - Session persistence (dump verified state to database)

    Examples:
        % Get complete system state
        core_dump(Dump).
        Dump = core_dump(
            verified([component(git, self, ...), ...]),
            broken([component(bad_entity, ..., ...)-error(...), ...])
        ).

        % Query only verified components
        core_dump(core_dump(verified(V), broken(_))),
        length(V, VerifiedCount).

        % Find all verification failures
        core_dump(core_dump(verified(_), broken(B))),
        forall(member(C-E, B), format('BROKEN: ~w~n  ERROR: ~w~n', [C, E])).
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

