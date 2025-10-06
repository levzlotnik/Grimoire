% Load ECS kernel first - bootstrap with direct ensure_loaded
% We need ecs_kernel.pl to get grimoire_ensure_loaded, so we load it directly
:- (getenv('GRIMOIRE_ROOT', Root) ->
    atomic_list_concat([Root, '/src/ecs_kernel.pl'], EcsKernel),
    ensure_loaded(EcsKernel)
   ;
    ensure_loaded("./ecs_kernel.pl")
   ).
:- use_module(library(filesex)).
:- use_module(library(http/json)).

% Core ECS predicates - must match ecs_kernel.pl

% Dynamic declarations for spell system and hooks
:- dynamic([
    cast/2,               % cast(Spell, RetVal)
    magic_cast/2,         % Composable spell casting primitive
    register_spell/4,     % register_spell(SpellCtor, input(Format), output(Format), docstring(Doc))
    cast_pre_hook/2,      % Hook infrastructure for extensible spell casting behavior
    cast_post_hook/2
], [
    discontiguous(true),
    multifile(true)
]).

% === MAGIC_CAST/2: COMPOSABLE SPELL CASTING PRIMITIVE ===
% Like PyTorch: define .forward() but call submodules via __call__()
% Within cast/2, use magic_cast/2 to invoke other spells

% magic_cast ensures grounding and executes hooks
magic_cast(SpellTerm, Result) :-
    ground(SpellTerm),                  % Ensure spell is fully grounded
    cast_pre_hooks(SpellTerm),          % Execute pre-cast hooks
    catch(
        cast(SpellTerm, Result),        % Execute the spell
        Error,
        Result = error(cast_failed(Error))
    ),
    cast_post_hooks(SpellTerm).         % Execute post-cast hooks

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
component(system, concept, session).

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
component(system, subsystem, session).
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
% Load core system components - immediate loading for core functionality
:- load_entity(semantic(file("@/src/git.pl"))).
:- load_entity(semantic(file("@/src/utils.pl"))).
:- load_entity(semantic(folder("@/src/nix"))).
:- load_entity(semantic(file("@/src/fs.pl"))).
:- load_entity(semantic(folder("@/src/db"))).
:- load_entity(semantic(folder("@/src/project"))).
:- load_entity(semantic(file("@/src/session.pl"))).
:- load_entity(semantic(folder("@/src/golems"))).
:- load_entity(semantic(folder("@/src/protocol_clients"))).

% Spell system - fantasy-themed query/mutation separation
entity(spell).
component(spell, ctor, conjure).
component(spell, ctor, perceive).

% Conjure entity for mutable operations
entity(conjure).
% Spell constructors (manual, for spells without register_spell yet)
component(conjure, ctor, session).

% Perceive entity for query operations
entity(perceive).
% Core perceive constructors
% search_regex doesn't have register_spell yet
component(perceive, ctor, search_regex).

% Core perceive command entities (manual, for spells without register_spell yet)
entity(search_regex).

% Core perceive docstrings (manual, for spells without register_spell yet)
docstring(search_regex,
    {|string(_)||
    Search content using regular expressions.
    Searches through content with regex patterns to find matches.
    Format: perceive(search_regex(ContentWithLineNumbers, Pattern, FoundContent)).
        ContentWithLineNumbers - input content with line numbers
        Pattern - regular expression pattern to search for
        FoundContent - unifies with matching content
    |}
).


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

% Derive spell constructors from spell registrations
% This automatically creates component(perceive/conjure, ctor, Spell) from register_spell declarations
component(SpellType, ctor, Spell) :-
    atom(SpellType),  % Guard: only derive for atomic spell types
    SpellTerm =.. [SpellType, Spell],
    register_spell(SpellTerm, _, _, _).

% Make registered spells entities automatically:
entity(Spell) :-
    ground(Spell),
    register_spell(SpellTerm, _, _, _),
    SpellTerm =.. [SpellType, Spell],
    atom(SpellType),  % Guard: ensure SpellType is atomic
    component(spell, ctor, SpellType),
    component(SpellType, ctor, Spell).

entity(perceive(Spell)) :-
    ground(Spell),
    register_spell(perceive(Spell), _, _, _).

entity(conjure(Spell)) :-
    ground(Spell),
    register_spell(conjure(Spell), _, _, _).

component(perceive, ctor, Spell) :-
    ground(Spell),
    register_spell(perceive(Spell), _, _, _).

component(conjure, ctor, Spell) :-
    ground(Spell),
    register_spell(conjure(Spell), _, _, _).

% Derive docstrings from spell registrations
docstring(E, S) :-
    register_spell(E, input(InputFormat), output(OutputFormat), docstring(Explanation)),
    E =.. [SpellCtor, SpellType],
    atom(SpellCtor),  % Guard: ensure SpellCtor is atomic
    component(spell, ctor, SpellCtor),
    component(SpellCtor, ctor, SpellType),
    format(string(S), "~w~n~nInput Format: ~w~nOutput Format: ~w", [Explanation, InputFormat, OutputFormat]).

% Grimoire-level spell registrations
register_spell(
    conjure(shell),
    input(shell(args('Args'))),
    output(either(
        ok(result(stdout('StdOut'), stderr('StdErr'))),
        error(shell_error(args('Args'), exit('ExitCode'), stdout('StdOut'), stderr('StdErr')))
    )),
    docstring("Execute shell command with arguments. Returns stdout and stderr on success or error details on failure.")
).

register_spell(
    conjure(executable_program),
    input(executable_program(program('Program'), args('Args'))),
    output(either(
        ok(result(stdout('StdOut'), stderr('StdErr'))),
        error(process_error(program('Program'), exit('ExitCode'), stdout('StdOut'), stderr('StdErr')))
    )),
    docstring("Execute a program with arguments. Returns stdout and stderr on success or error details on failure.")
).

register_spell(
    perceive(entities),
    input(entities),
    output(entity_list('Entities')),
    docstring("List all entities in the system. Returns a list of all registered entities.")
).


cast(conjure(executable_program(Program, Args)), RetVal) :-
    % Non-interactive mode - capture output and exit code
    setup_call_cleanup(
        process_create(
            path(Program),
            Args,
            [stdout(pipe(Out)), stderr(pipe(Err)), process(PID)]
        ),
        % Read output and wait for process
        (read_string(Out, _, Stdout),
         read_string(Err, _, Stderr),
         process_wait(PID, exit(ExitCode))),
        % Cleanup
        (close(Out), close(Err))
    ),
    % Return structured result based on exit code
    (ExitCode = 0 ->
        RetVal = ok(result(Stdout, Stderr))
    ;
        RetVal = error(process_error(Program, exit(ExitCode), Stdout, Stderr))
    ).

cast(conjure(executable_program(Program, Args, interactive)), RetVal) :-
    % Interactive mode - pass through stdin/stdout
    setup_call_cleanup(
        process_create(
            path(Program),
            Args,
            [stdin(std), stdout(std), stderr(std)]
        ),
        true,  % Process runs interactively
        true   % No cleanup needed
    ),
    RetVal = ok("Interactive program completed").


cast(conjure(shell(Args)), RetVal) :-
    join_args(Args, JoinedArgs),
    cast(
        conjure(executable_program(sh, ["-c", JoinedArgs])),
        RetVal
    ).

cast(conjure(shell(Args, interactive)), RetVal) :-
    join_args(Args, JoinedArgs),
    cast(
        conjure(executable_program(sh, ["-c", JoinedArgs], interactive)),
        RetVal
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

% Remove this dispatcher - let individual files handle cast(conjure(...)) directly

cast(ritual(Operations), RetVal) :-
    % Cast multiple conjuration spells as a ritual (atomic transaction)
    maplist(cast, Operations, Results),
    RetVal = ok(Results).

execute_commands([], []).
execute_commands([Cmd|Rest], [Res|Results]) :-
    format("~w\n", [run(Cmd, Res)]),
    run(Cmd, Res),
    ( Res = error(_) ->
        Results = []
    ;
        execute_commands(Rest, Results)
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
cast(perceive(entities), Result) :-
    catch(
        (findall(Entity, entity(Entity), Entities),
         Result = ok(entity_list(Entities))),
        Error,
        Result = error(grimoire_error(Error))
    ).

% Search for regex pattern in content with line numbers
cast(perceive(search_regex(ContentWithLineNumbers, Pattern)), Result) :-
    catch(
        (findall(line(Num, Line),
            (member(line(Num, Line), ContentWithLineNumbers),
             re_match(Pattern, Line)),
            FoundContent),
         Result = ok(search_results(FoundContent))),
        Error,
        Result = error(grimoire_error(Error))
    ).

