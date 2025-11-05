it_is_what_it_is :- !.

%% ============================================================================
%% LIBRARY IMPORTS
%% ============================================================================

:- use_module(library(option)).
:- use_module(library(filesex)).
:- use_module(library(http/json)).
:- use_module(library(prolog_stack)).

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
    cast_pre_hook/2,      % cast_pre_hook(SpellTermPattern, Goal)
    cast_post_hook/3      % cast_post_hook(SpellTermPattern, ResultPattern, Goal)
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

    % Capture source location where register_spell was called
    prolog_load_context(file, File),
    prolog_load_context(term_position, TermPos),
    (   TermPos = '$stream_position'(_, Line, _, _, _)
    ->  true
    ;   Line = 0
    ),

    % Convert implementation to atom for easier introspection
    term_string(implementation(SpellPattern, Result, Impl), ImplAtom),

    % Generate metadata components using SpellSig (constructor)
    Components = [
        component(SpellType, ctor, SpellGroundTerm),
        component(SpellSig, docstring, Doc),
        component(SpellSig, format_input, Input),
        component(SpellSig, format_output, Output),
        component(SpellSig, spell_options, Options),
        component(SpellSig, source_location, source_location(File, Line)),
        component(SpellSig, implementation, ImplAtom)
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
            cast_pre_hooks(SpellTerm),
            cast_impl(SpellTerm, Result),
            cast_post_hooks(SpellTerm, Result)
        ),
        retract(in_magic_cast)
    ),

    % 5. Validate result is grounded
    (ground(Result)
    -> true
    ; throw(error(
        instantiation_error(Result),
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
%% HOOK SYSTEM
%% ============================================================================

% Domains extend hooks via multifile cast_pre_hook/2 and cast_post_hook/3
% Example:
%   cast_pre_hook(conjure(db(create(_, _, _))), (format('Creating DB~n', []))).
%   cast_post_hook(conjure(_), ok(_), (format('Success~n', []))).

% Hook execution helpers
cast_pre_hooks(Term) :-
    forall(cast_pre_hook(Term, Goal), call(Goal)).

cast_post_hooks(Term, Result) :-
    forall(cast_post_hook(Term, Result, Goal), call(Goal)).

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
    conjure(it_is_what_it_is),
    input(it_is_what_it_is),
    output(ok(it_is_what_it_is)),
    "It is what it is.",
    [],
    implementation(conjure(it_is_what_it_is), Promise, (
        it_is_what_it_is,
        Promise = ok(it_is_what_it_is)
    ))
).

register_spell(
    perceive(search_regex),
    input(search_regex(content('ContentWithLineNumbers'), pattern('Pattern'))),
    output(either(ok(search_results('MatchedLines')), error(grimoire_error('Reason')))),
    "Search content using regular expressions. Searches through content with line numbers to find pattern matches.",
    [],
    implementation(perceive(search_regex(content(ContentWithLineNumbers), pattern(Pattern))), Result, (
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
    implementation(conjure(executable_program(program(Program), args(Args))), RetVal, (
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
            RetVal = ok(result(stdout(Stdout), stderr(Stderr)))
        ;
            RetVal = error(process_error(program(Program), exit(ExitCode), stdout(Stdout), stderr(Stderr)))
        )
    ))
).

register_spell(
    conjure(executable_program_interactive),
    input(executable_program_interactive(program('Program'), args('Args'))),
    output(ok(completion_message('Message'))),
    "Execute a program interactively with stdin/stdout/stderr passed through. Returns completion message.",
    [],
    implementation(conjure(executable_program_interactive(program(Program), args(Args))), RetVal, (
        setup_call_cleanup(
            process_create(
                path(Program),
                Args,
                [stdin(std), stdout(std), stderr(std)]
            ),
            true,
            true
        ),
        RetVal = ok(completion_message("Interactive program completed"))
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
    implementation(conjure(shell(args(Args))), RetVal, (
        join_args(Args, JoinedArgs),
        magic_cast(
            conjure(executable_program(program(sh), args(["-c", JoinedArgs]))),
            RetVal
        )
    ))
).

register_spell(
    conjure(shell_interactive),
    input(shell_interactive(args('Args'))),
    output(ok(completion_message('Message'))),
    "Execute shell command interactively with stdin/stdout/stderr passed through. Returns completion message.",
    [],
    implementation(conjure(shell_interactive(args(Args))), RetVal, (
        join_args(Args, JoinedArgs),
        magic_cast(
            conjure(executable_program_interactive(program(sh), args(["-c", JoinedArgs]))),
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
    implementation(conjure(ritual(operations(list(Operations)))), RetVal, (
        maplist(magic_cast, Operations, Results),
        RetVal = ok(results(list(Results)))
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
% META-INTROSPECTION SPELLS
% ========================================================================

% Prove component provenance - show where it comes from and how it's verified
register_spell(
    perceive(prove_it),
    input(prove_it(component('Entity', 'ComponentType', 'Value'))),
    output(either(
        ok(qed(
            component('Entity', 'ComponentType', 'Value'),
            generated_by('Generation'),
            discriminated_by('Verification')
        )),
        error(sus('Reason'))
    )),
    "Trace component provenance: where it was generated (fact or derivation) and how it's verified. Returns qed(...) on success, sus(...) when something's off.",
    [],
    implementation(perceive(prove_it(component(Entity, ComponentType, Value))), Result, (
        catch(
            (prove_it(component(Entity, ComponentType, Value), Proof),
             Result = ok(Proof)),
            error(sus(Reason), _Context),
            Result = error(sus(Reason))
        )
    ))
).

% Spell sauce - show complete spell metadata including source location
register_spell(
    perceive(sauce_me),
    input(sauce_me(spell('SpellConstructor'))),
    output(either(
        ok(magic_sauce(
            spell('SpellConstructor'),
            registered_at('Location'),
            implementation('ImplText'),
            input_format('Input'),
            output_format('Output'),
            docstring('Doc'),
            options('Options')
        )),
        error(tragic_sauce('Reason'))
    )),
    "Show complete spell metadata: where registered, implementation source, formats, docs, options. Returns the magic_sauce on success, tragic_sauce when not found.",
    [],
    implementation(perceive(sauce_me(spell(SpellCtor))), Result, (
        catch(
            (extract_spell_sauce(SpellCtor, sauce(Loc, Impl, Input, Output, Doc, Opts)),
             Result = ok(magic_sauce(
                spell(SpellCtor),
                Loc,
                Impl,
                Input,
                Output,
                Doc,
                Opts
             ))),
            Error,
            Result = error(tragic_sauce(Error))
        )
    ))
).

% === PROVE_IT IMPLEMENTATION ===

prove_component_provenance(component(E, C, V), Proof) :-
    % Set up trace collection
    setup_call_cleanup(
        start_component_trace(TraceId),
        catch(
            please_verify(component(E, C, V)),
            error(ErrorTerm, _Context),
            (stop_component_trace(TraceId, Events),
             extract_verification_from_trace(component(E, C, V), Events, Verification),
             Proof = error(verification_failed(component(E, C, V), ErrorTerm, Verification)))
        ),
        (var(Proof) -> stop_component_trace(TraceId, Events) ; true)
    ),
    (var(Proof) ->
        analyze_trace(Events, component(E, C, V), Proof)
    ;
        true
    ).

start_component_trace(TraceId) :-
    gensym(trace, TraceId),
    nb_setval(current_trace_id, TraceId),
    assertz((
        user:prolog_trace_interception(Port, Frame, _PC, continue) :-
            nb_getval(current_trace_id, TID),
            catch(
                (
                    prolog_frame_attribute(Frame, goal, Goal),
                    (   (Goal = component(_, _, _) ; Goal = user:component(_, _, _))
                    ->  prolog_frame_attribute(Frame, level, Level),
                        (   catch(prolog_frame_attribute(Frame, clause, Clause), _, fail)
                        ->  ClauseInfo = clause(Clause)
                        ;   ClauseInfo = no_clause
                        ),
                        recordz(TID, trace_event(Port, Level, Goal, ClauseInfo))
                    ;   (Goal = verify(_) ; Goal = user:verify(_))
                    ->  prolog_frame_attribute(Frame, level, Level),
                        (   catch(prolog_frame_attribute(Frame, clause, Clause), _, fail)
                        ->  ClauseInfo = clause(Clause)
                        ;   ClauseInfo = no_clause
                        ),
                        recordz(TID, trace_event(Port, Level, Goal, ClauseInfo))
                    ;   true
                    )
                ),
                _,
                true
            )
    )),
    trace.

stop_component_trace(TraceId, Events) :-
    notrace,
    retractall(user:prolog_trace_interception(_, _, _, _)),
    findall(Event, recorded(TraceId, Event, _Ref), Events),
    forall(recorded(TraceId, _, Ref), erase(Ref)),
    nb_delete(current_trace_id).

analyze_trace(Events, component(E, C, V), Proof) :-
    findall(
        Level-trace_event(exit, Level, Goal, ClauseInfo),
        (member(trace_event(exit, Level, Goal, ClauseInfo), Events),
         unify_goals(Goal, component(E, C, V))),
        ExitEvents
    ),
    (   ExitEvents = []
    ->  Proof = no_proof(component(E, C, V))
    ;   sort(ExitEvents, SortedExits),
        reverse(SortedExits, [TopLevel-trace_event(exit, _, SuccessGoal, ClauseInfo)|_]),
        extract_provenance(SuccessGoal, ClauseInfo, Events, TopLevel, Generation),
        extract_verification_from_trace(component(E, C, V), Events, Verification),
        Proof = proof(
            component(E, C, V),
            generated_by(Generation),
            Verification
        )
    ).

unify_goals(user:component(E1, C1, V1), component(E2, C2, V2)) :- !,
    E1 = E2, C1 = C2, V1 = V2.
unify_goals(component(E1, C1, V1), user:component(E2, C2, V2)) :- !,
    E1 = E2, C1 = C2, V1 = V2.
unify_goals(component(E1, C1, V1), component(E2, C2, V2)) :-
    E1 = E2, C1 = C2, V1 = V2.

extract_provenance(_Goal, no_clause, _Events, _TopLevel, unknown_source) :- !.

extract_provenance(Goal, clause(ClauseRef), Events, TopLevel, Generation) :-
    catch(clause_property(ClauseRef, file(File)), _, File = '<no file>'),
    catch(clause_property(ClauseRef, line_count(Line)), _, Line = 0),
    catch(clause(Goal, Body, ClauseRef), _, Body = unknown),
    (   Body = true
    ->  Generation = fact(source_location(File, Line))
    ;   Body = unknown
    ->  Generation = unknown_body(source_location(File, Line))
    ;   find_dependency(Body, Events, TopLevel, Dependency),
        Generation = derived_from(Dependency, source_location(File, Line))
    ).

find_dependency(_Body, Events, TopLevel, Dependency) :-
    findall(
        Goal,
        (member(trace_event(exit, Level, Goal, _ClauseInfo), Events),
         Level > TopLevel,
         (Goal = component(_, _, _) ; Goal = user:component(_, _, _)),
         strip_user_prefix(Goal, CleanGoal),
         CleanGoal = component(E, _, _),
         E \= component),
        ComponentCalls
    ),
    (   ComponentCalls = []
    ->  Dependency = body_goal(unknown)
    ;   ComponentCalls = [SingleDep|_]
    ->  strip_user_prefix(SingleDep, Dependency)
    ).

strip_user_prefix(user:Goal, Goal) :- !.
strip_user_prefix(Goal, Goal).

extract_verification_from_trace(Component, Events, Verification) :-
    findall(
        verify_event(Level, Goal, ClauseInfo, Port),
        (member(trace_event(Port, Level, Goal, ClauseInfo), Events),
         (Port = exit ; Port = redo(_) ; Port = exception(_)),
         ClauseInfo = clause(_),
         strip_user_prefix(Goal, CleanGoal),
         CleanGoal = verify(VerifyArg),
         VerifyArg = Component),
        VerifyEvents
    ),
    (VerifyEvents = [] ->
        Verification = no_verifier
    ;   sort(VerifyEvents, SortedVerifyEvents),
        reverse(SortedVerifyEvents, [verify_event(_, _, ClauseInfo, _Port)|_]),
        (ClauseInfo = clause(ClauseRef) ->
            catch(clause_property(ClauseRef, file(File)), _, File = '<no file>'),
            catch(clause_property(ClauseRef, line_count(Line)), _, Line = 0),
            catch(clause(verify(Component), Body, ClauseRef), _, Body = unknown),
            (Body = ((\+ in_please_verify), _, _) ->
                Verification = no_verifier
            ;   Verification = discriminated_by(
                    verifier(verify(Component) :- Body, source_location(File, Line))
                )
            )
        ;   Verification = no_verifier
        )
    ).

% === SAUCE IMPLEMENTATION ===

extract_spell_sauce(SpellCtor, Sauce) :-
    % Get all metadata from component facts (generated by register_spell)
    (   component(SpellCtor, docstring, Doc)
    ->  true
    ;   throw(error(spell_not_found(SpellCtor)))
    ),

    component(SpellCtor, format_input, Input),
    component(SpellCtor, format_output, Output),
    component(SpellCtor, spell_options, Options),
    component(SpellCtor, source_location, Location),
    component(SpellCtor, implementation, Implementation),

    Sauce = sauce(
        registered_at(Location),
        implementation(Implementation),
        input_format(Input),
        output_format(Output),
        docstring(Doc),
        options(Options)
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


% ========================================================================
% SKILL SYSTEM
% ========================================================================

% Invoke skill on entity - resolves skill component to spell and casts it
register_spell(
    conjure(invoke_skill),
    input(invoke_skill(entity('Entity'), skill('SkillTerm'))),
    output(either(
        ok(skill_result('Result')),
        error(skill_error('Reason'))
    )),
    "Invoke a skill on an entity. Skills are derived from entity components and represent available operations. The skill term is resolved to a spell term which is then cast.",
    [],
    implementation(conjure(invoke_skill(entity(Entity), skill(SkillTerm))), Result, (
        % Skill component should map to a spell term
        (catch(please_verify(component(Entity, skill(SkillTerm), SpellTerm)), _, fail)
        -> (magic_cast(SpellTerm, SpellResult),
            Result = ok(skill_result(SpellResult)))
        ;  Result = error(skill_error(skill_not_found(Entity, SkillTerm))))
    ))
).

% List all skills available on an entity
register_spell(
    perceive(skills),
    input(skills(entity('Entity'))),
    output(either(
        ok(skills_list('Skills')),
        error(skill_error('Reason'))
    )),
    "List all available skills for an entity. Skills are operations derived from entity structure (e.g., nix packages become build skills).",
    [],
    implementation(perceive(skills(entity(Entity))), Result, (
        catch(
            (findall(
                skill(SkillTerm, SpellTerm),
                component(Entity, skill(SkillTerm), SpellTerm),
                Skills
             ),
             Result = ok(skills_list(Skills))),
            Error,
            Result = error(skill_error(Error))
        )
    ))
).


:- load_entity(semantic(file("@/src/git.pl"))).
:- load_entity(semantic(file("@/src/utils.pl"))).
:- load_entity(semantic(folder("@/src/nix"))).
:- load_entity(semantic(file("@/src/fs.pl"))).
:- load_entity(semantic(folder("@/src/db"))).
:- load_entity(semantic(folder("@/src/project"))).

:- load_entity(semantic(file("@/src/session.pl"))).

% Interface domain - Python-Prolog bridge
:- load_entity(semantic(folder("@/src/interface"))).

% Temporarily disabled - will re-enable after fixing
% :- load_entity(semantic(folder("@/src/golems"))).
% :- load_entity(semantic(folder("@/src/protocol_clients"))).


