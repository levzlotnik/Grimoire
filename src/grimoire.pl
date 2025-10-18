%% ============================================================================
%% LIBRARY IMPORTS
%% ============================================================================

:- use_module(library(option)).
:- use_module(library(filesex)).
:- use_module(library(http/json)).

% Load ECS kernel first - bootstrap with direct ensure_loaded
% We need ecs_kernel.pl to get grimoire_ensure_loaded, so we load it directly
% ecs_kernel provides operators (==> and ::) and component DSL term_expansion
:- (getenv('GRIMOIRE_ROOT', Root) ->
    atomic_list_concat([Root, '/src/ecs_kernel.pl'], EcsKernel),
    ensure_loaded(EcsKernel)
   ;
    ensure_loaded("./ecs_kernel.pl")
   ).

% Core ECS predicates - must match ecs_kernel.pl

% Multifile declarations - must come before any clauses
:- multifile docstring/2.
:- multifile entity/1.
:- multifile component/3.

%% ============================================================================
%% DYNAMIC DECLARATIONS
%% ============================================================================

:- dynamic([
    cast_impl/2,          % Spell implementations (guarded)
    in_magic_cast/0       % Guard flag for cast_impl/2
], [
    discontiguous(true),
    multifile(true)
]).

:- dynamic([
    cast_pre_hook/2,      % Hook infrastructure for extensible spell casting behavior
    cast_post_hook/2
], [
    discontiguous(true),
    multifile(true)
]).

%% ============================================================================
%% TERM EXPANSION - SPELL REGISTRATION
%% ============================================================================

term_expansion(
    register_spell(SpellSig, Input, Output, Doc, Options, implementation(SpellPattern, Result, Impl)),
    Generated
) :-
    % Extract spell information from CONSTRUCTOR (no variables!)
    SpellSig =.. [SpellType, SpellGroundTerm],

    % SpellPattern is provided by user with actual variables
    % These are the SAME variables used in Impl body
    CastImpl = (cast_impl(SpellPattern, Result) :- Impl),

    % Generate metadata components using SpellSig (constructor)
    Components = [
        component(SpellType, ctor, SpellGroundTerm),
        component(SpellSig, docstring, Doc),
        component(SpellSig, format_input, Input),
        component(SpellSig, format_output, Output),
        component(SpellSig, spell_options, Options)
    ],

    Generated = [CastImpl | Components].

%% ============================================================================
%% SPELL SIGNATURE MATCHING
%% ============================================================================

% Find the matching spell signature from registered constructors
% Throws if 0 or 2+ matches (must be exactly one)
find_matching_spell_sig(SpellTerm, SpellSig) :-
    SpellTerm =.. [SpellType, SpellGroundTerm],
    findall(
        CandidateSig,
        (component(SpellType, ctor, CandidateSig),
         is_matching_signature(SpellGroundTerm, CandidateSig)),
        Candidates
    ),
    (   Candidates = []
    ->  throw(error(
            existence_error(spell, SpellTerm),
            context(find_matching_spell_sig/2, 'No registered spell matches this signature')))
    ;   Candidates = [UniqueCandidateSig]
    ->  SpellSig =.. [SpellType, UniqueCandidateSig]
    ;   % Multiple matches
        throw(error(
            ambiguous_spell(SpellTerm, Candidates),
            context(find_matching_spell_sig/2, 'Multiple spells match this signature - spell signatures must be unique')))
    ).

% Base case: we reached the end of the nested structure
is_matching_signature(SpellGroundTerm, SpellSig) :-
    atom(SpellSig),
    SpellGroundTerm =.. [SpellSig|_].

% Recursive case: spell signatures are nested using /1 terms
is_matching_signature(SpellGroundTerm, SpellSig) :-
    SpellSig =.. [OuterFunctorSpellSig, InnerValSpellSig],
    SpellGroundTerm =.. [OuterFunctorSpellSig, InnerValTerm|_],
    is_matching_signature(InnerValTerm, InnerValSpellSig).

%% ============================================================================
%% TYPE-CHECKED magic_cast/2
%% ============================================================================

magic_cast(SpellTerm, Result) :-
    % 1. Input must be fully grounded
    (ground(SpellTerm)
    -> true
    ; throw(error(
        instantiation_error(SpellTerm),
        context(magic_cast/2, 'Spell term must be fully grounded')))
    ),

    % 2. Find matching registered spell signature
    (find_matching_spell_sig(SpellTerm, SpellSig)
    -> true
    ; throw(error(
        existence_error(spell, SpellTerm),
        context(magic_cast/2, 'Spell not registered')))
    ),

    % 3. Check spell has metadata (should always be true if step 2 passed)
    (component(SpellSig, docstring, _)
    -> true
    ; throw(error(
        existence_error(spell, SpellSig),
        context(magic_cast/2, 'Spell not registered')))
    ),

    % 4. Execute with hooks and guards
    setup_call_cleanup(
        assertz(in_magic_cast),
        (
            pre_magic_cast_hook(SpellTerm),
            cast_impl(SpellTerm, Result),
            post_magic_cast_hook(SpellTerm, Result)
        ),
        retract(in_magic_cast)
    ),

    % 5. Validate result is grounded
    (ground(Result)
    -> true
    ; throw(error(
        type_error(spell_output, Result, 'must be grounded'),
        context(magic_cast/2, 'Spell returned ungrounded result')))
    ),

    % 6. Commit - no backtracking after successful spell execution
    !.

% Guard: cast_impl can only be called from magic_cast
cast_impl(SpellTerm, _) :-
    \+ in_magic_cast,
    !,
    throw(error(
        direct_cast_forbidden(SpellTerm),
        context(cast_impl/2, 'NEVER call cast_impl/2 directly. Use magic_cast/2.')
    )).

docstring(magic_cast,
    {|string(_)||
    Execute a spell with full type checking, hooks, and guards.

    Format: magic_cast(SpellTerm, Result)

    Guarantees:
    1. SpellTerm must be fully grounded (no unbound variables)
    2. Spell must be registered (has metadata via register_spell/6)
    3. Pre-hook executes before spell
    4. Spell implementation runs (cast_impl/2)
    5. Post-hook executes after spell
    6. Result must be fully grounded

    Guards:
    - cast_impl/2 can ONLY be called from magic_cast/2
    - Attempting to call cast_impl/2 directly throws error

    Use Cases:
    - Invoke domain spells with type safety
    - Compose spells (call magic_cast from within spell implementations)
    - Session hooks automatically log persistent spells

    Examples:
        % Simple spell invocation
        magic_cast(conjure(git(commit("fix bug"))), Result).
        Result = ok(committed(hash("abc123"))).

        % Spell composition (from within another spell)
        cast_impl(conjure(project(create(Name))), Result) :-
            magic_cast(conjure(git(init(Name))), GitResult),
            magic_cast(conjure(nix(flake(new(Name)))), NixResult),
            Result = ok(project_created(Name)).
    |}
).

%% ============================================================================
%% SPELL HOOKS
%% ============================================================================

% Default hooks (can be overridden by domains)
pre_magic_cast_hook(_SpellTerm).

post_magic_cast_hook(SpellTerm, _Result) :-
    % Check if spell has session_persistent flag
    (find_matching_spell_sig(SpellTerm, SpellSig)
    -> (component(SpellSig, spell_options, Options)
       -> (option(session_persistent(true), Options, false)
          -> % TODO: Session domain will implement actual persistence
             true
          ; true)
       ; true)
    ; true),
    !.  % Cut to make deterministic

docstring(pre_magic_cast_hook,
    {|string(_)||
    Hook executed BEFORE spell implementation.

    Default: no-op
    Override: Domains can define custom pre-hooks for logging, validation, etc.

    Example override:
        pre_magic_cast_hook(conjure(db(create(DbId, _, _)))) :-
            format('Creating database: ~w~n', [DbId]).
    |}
).

docstring(post_magic_cast_hook,
    {|string(_)||
    Hook executed AFTER spell implementation.

    Default behavior:
    - Checks if spell has session_persistent(true) flag
    - Session domain implements actual persistence logic

    Example override:
        post_magic_cast_hook(SpellTerm, ok(Result)) :-
            log_successful_cast(SpellTerm, Result).
    |}
).

%% ============================================================================
%% HOOK SYSTEM
%% ============================================================================

% Hook execution helpers
cast_pre_hooks(Term) :-
    forall(cast_pre_hook(Term, Goal), call(Goal)).

cast_post_hooks(Term) :-
    forall(cast_post_hook(Term, Goal), call(Goal)).

% The most fundamental entity
:- self_entity(system).

% GRIMOIRE_DATA path - defaults to $HOME/.grimoire
grimoire_data_path(Path) :-
    (getenv('GRIMOIRE_DATA', Path) ->
        true
    ;
        (getenv('HOME', Home) ->
            atomic_list_concat([Home, '/.grimoire'], Path)
        ;
            Path = '.grimoire'
        )
    ).

% Templates tools path (must be set by nix devShell)
grimoire_templates_tools_path(Path) :-
    getenv('GRIMOIRE_TEMPLATES_TOOLS', Path).

docstring(system, S) :-
    grimoire_resolve_path('@/src/GRIMOIRE.md', GrimoirePath),
    read_file_to_string(GrimoirePath, GrimoireDoc, []),
    findall(Line, (
        component(system, concept, Concept),
        docstring(Concept, ConceptDoc),
        format(atom(Line), '  - ~w: ~w', [Concept, ConceptDoc])
    ), Lines),
    atomic_list_concat(Lines, '\n', ConceptsDocs),
    format(string(S), '~w~n~nCore Concepts:~n~w', [GrimoireDoc, ConceptsDocs]).


component(system, root_dir, folder("/home/levz/Projects/Grimoire")).
% Some properties
component(
    system,
    semantic_root,
    folder(AbsSemRoot)
) :-
    RelSemRoot = "src/prolog",
    component(system, root_dir, folder(SysRoot)),
    directory_file_path(SysRoot, RelSemRoot, AbsSemRoot).

system_semantic_root(SemanticRootDir) :-
    component(system, semantic_root, SemanticRootDir).


% Fundamental concepts
component(system, concept, spell).
component(system, concept, transaction).
component(system, concept, hardware).
component(system, concept, execute).
component(system, concept, source).
component(system, concept, project).
component(system, concept, conjure).
component(system, concept, perceive).
component(system, concept, interface).
component(system, concept, git).
component(system, concept, nix).

component(system, source, source(semantic(file("grimoire.pl")))).

entity(source).
component(source, ctor, semantic).
entity(semantic).
component(semantic, ctor, file).
component(semantic, ctor, folder).

docstring(semantic,
    {|string(_)||
    Semantic knowledge source specification.
    Constructors: file(Path), folder(Path)
    Used to load knowledge from semantics.pl files or folders containing them.
    Example: load_entity(semantic(file("./semantics.pl")))
    Forms the basis of Grimoire's distributed knowledge architecture.
    |}).
docstring(source,S) :-
    make_ctors_docstring(source, SourceTypes),
    S = {|string(SourceTypes)||
    Source of representation for entity.
    Programmers usually refer to it broadly as "source code".
    Typically could be things like source code files, all the way up to
    entire project directories that contain a mix of code, data
    and execution contexts (compilation/runtime flows and scripts).

    Format: source(SourceType)

    Currently defined source types:
    {SourceTypes}
    |}.
docstring(source(semantic(file)), "A source code file for an entity.\nFormat: semantic(file(Path))").
docstring(source(semantic(folder)), "A folder with source code for entity.\nFormat: semantic(folder(Path))").

% System components and their sources - these are just metadata now
% The actual loading is handled by entity/1 rules below
component(system, subsystem, git).
component(system, subsystem, nix).
component(system, subsystem, fs).
component(system, subsystem, project).
component(system, subsystem, golems).

entity(project).
component(project, source, source(semantic(folder("project")))).

docstring(project,
    {|string(_)||
    A project represents the organizational unit of a system,
    bundling together source code, configuration files,
    and other resources necessary for building and running an application or library.
    It structures the work into manageable components and defines the
    ecosystem in which the system operates.
    |}
).

% Core subsystem entities - loaded immediately on boot
% Spell system - fantasy-themed query/mutation separation
% MUST be defined BEFORE loading subsystems that use register_spell/4
entity(spell).
component(spell, ctor, conjure).
component(spell, ctor, perceive).

% Conjure entity for mutable operations
entity(conjure).

% Perceive entity for query operations
entity(perceive).

% Spell system docstrings
docstring(spell,
    {|string(_)||
    The fundamental magic system of Grimoire.
    Spells are divided into two categories:
    - conjure: Mutable operations that change system state
    - perceive: Perception operations that query system state

    Format: spell(conjure(...)) or spell(perceive(...))
    |}).

docstring(conjure,
    {|string(_)||
    Conjuration spells that modify system state.
    Must be cast using cast/2 predicate for safety.

    Format: cast(conjure(operation(...)), Result)
    Examples:
      - cast(conjure(git(commit('message'))), Result)
      - cast(conjure(mkdir('path')), Result)
    |}).

docstring(perceive,
    {|string(_)||
    Perception spells that query system state without modification.
    Called directly, with variables unified to results.

    Format: perceive(query(Var1, Var2, ...))
    Examples:
      - perceive(git(status(Branch, Ahead, Files)))
      - perceive(nix(flake(show(Apps, Packages, DevShells))))
    |}).

% === SPELL REGISTRATION SYSTEM ===
% Declarative interface documentation for spells

% Spell constructors are auto-generated by term_expansion of register_spell/6
% The term_expansion generates: component(SpellType, ctor, SpellGroundTerm) directly

% Make registered spells entities automatically
entity(Spell) :-
    ground(Spell),
    component(spell, ctor, SpellType),
    component(SpellType, ctor, Spell).

entity(perceive(Spell)) :-
    ground(Spell),
    component(perceive, ctor, Spell).

entity(conjure(Spell)) :-
    ground(Spell),
    component(conjure, ctor, Spell).

% Grimoire-level spell registrations

register_spell(
    perceive(search_regex),
    input(search_regex(content('ContentWithLineNumbers'), pattern('Pattern'))),
    output(either(ok(search_results('MatchedLines')), error(grimoire_error('Reason')))),
    "Search content using regular expressions. Searches through content with line numbers to find pattern matches.",
    [],
    implementation(perceive(search_regex(ContentWithLineNumbers, Pattern)), Result, (
        catch(
            (findall(line(Num, Line),
                (member(line(Num, Line), ContentWithLineNumbers),
                 re_match(Pattern, Line)),
                FoundContent),
             Result = ok(search_results(FoundContent))),
            Error,
            Result = error(grimoire_error(Error))
        )
    ))
).

register_spell(
    conjure(executable_program),
    input(executable_program(program('Program'), args('Args'))),
    output(either(
        ok(result(stdout('StdOut'), stderr('StdErr'))),
        error(process_error(program('Program'), exit('ExitCode'), stdout('StdOut'), stderr('StdErr')))
    )),
    "Execute a program with arguments. Returns stdout and stderr on success or error details on failure.",
    [],
    implementation(conjure(executable_program(Program, Args)), RetVal, (
        setup_call_cleanup(
            process_create(
                path(Program),
                Args,
                [stdout(pipe(Out)), stderr(pipe(Err)), process(PID)]
            ),
            (read_string(Out, _, Stdout),
             read_string(Err, _, Stderr),
             process_wait(PID, exit(ExitCode))),
            (close(Out), close(Err))
        ),
        (ExitCode = 0 ->
            RetVal = ok(result(Stdout, Stderr))
        ;
            RetVal = error(process_error(Program, exit(ExitCode), Stdout, Stderr))
        )
    ))
).

register_spell(
    conjure(executable_program_interactive),
    input(executable_program(program('Program'), args('Args'), interactive)),
    output(ok(completion_message('Message'))),
    "Execute a program interactively with stdin/stdout/stderr passed through. Returns completion message.",
    [],
    implementation(conjure(executable_program(Program, Args, interactive)), RetVal, (
        setup_call_cleanup(
            process_create(
                path(Program),
                Args,
                [stdin(std), stdout(std), stderr(std)]
            ),
            true,
            true
        ),
        RetVal = ok("Interactive program completed")
    ))
).

register_spell(
    conjure(shell),
    input(shell(args('Args'))),
    output(either(
        ok(result(stdout('StdOut'), stderr('StdErr'))),
        error(shell_error(args('Args'), exit('ExitCode'), stdout('StdOut'), stderr('StdErr')))
    )),
    "Execute shell command with arguments. Returns stdout and stderr on success or error details on failure.",
    [],
    implementation(conjure(shell(Args)), RetVal, (
        join_args(Args, JoinedArgs),
        magic_cast(
            conjure(executable_program(sh, ["-c", JoinedArgs])),
            RetVal
        )
    ))
).

register_spell(
    conjure(shell_interactive),
    input(shell(args('Args'), interactive)),
    output(ok(completion_message('Message'))),
    "Execute shell command interactively with stdin/stdout/stderr passed through. Returns completion message.",
    [],
    implementation(conjure(shell(Args, interactive)), RetVal, (
        join_args(Args, JoinedArgs),
        magic_cast(
            conjure(executable_program(sh, ["-c", JoinedArgs], interactive)),
            RetVal
        )
    ))
).

% Helper to join and escape args
join_args(Args, Cmd) :-
    maplist(shell_quote, Args, QuotedArgs),
    atomic_list_concat(QuotedArgs, ' ', Cmd).

shell_quote(Arg, Quoted) :-
    format(string(Quoted), "'~w'", [Arg]).


% Spell casting system - replaces run/2 for mutable operations
docstring(cast,
    {|string(_)||
    Safely cast conjuration spells that modify system state.
    Supports both single spells and ritual (transaction) casting.

    Format:
      cast(conjure(operation(...)), Result)    % Single spell
      cast(ritual([op1, op2, ...]), Result)    % Atomic ritual

    Examples:
      - cast(conjure(git(commit('message'))), Result)
      - cast(ritual([mkdir('dir'), mkfile('dir/file')]), Result)
    |}).

register_spell(
    conjure(ritual),
    input(ritual(operations(list('Spells')))),
    output(ok(results(list('Results')))),
    "Cast multiple spells as an atomic ritual (transaction). All spells must succeed or all fail together.",
    [],
    implementation(ritual(Operations), RetVal, (
        maplist(magic_cast, Operations, Results),
        RetVal = ok(Results)
    ))
).

% NOTE: write_file/2, read_file_to_lines/2, and write_lines_to_file/2
% have been migrated to fs.pl (filesystem domain)

% Add list_mounted_semantics predicate
docstring(list_mounted_semantics,
    {|string(_)||
    Lists all currently mounted semantic modules.
    Format: list_mounted_semantics(Paths).
    Returns list of absolute paths to mounted semantic files.
    |}
).

% ========================================================================
% CORE PERCEIVE SPELLS
% ========================================================================

% Perceive all entities in the system
register_spell(
    perceive(entities),
    input(entities),
    output(entity_list('Entities')),
    "List all entities in the system. Returns a list of all registered entities.",
    [],
    implementation(perceive(entities), Result, (
        catch(
            (findall(Entity, entity(Entity), Entities),
             Result = ok(entity_list(Entities))),
            Error,
            Result = error(grimoire_error(Error))
        )
    ))
).


% Load core system components - loaded AFTER spell system is defined
% Temporarily disabled - will re-enable one by one after fixing each domain
% :- load_entity(semantic(file("@/src/git.pl"))).
% :- load_entity(semantic(file("@/src/utils.pl"))).
% :- load_entity(semantic(folder("@/src/nix"))).
% :- load_entity(semantic(file("@/src/fs.pl"))).
% :- load_entity(semantic(folder("@/src/db"))).
% :- load_entity(semantic(folder("@/src/project"))).
% :- load_entity(semantic(file("@/src/session.pl"))).
% :- load_entity(semantic(folder("@/src/interface"))).
% :- load_entity(semantic(folder("@/src/golems"))).
% :- load_entity(semantic(folder("@/src/protocol_clients"))).


