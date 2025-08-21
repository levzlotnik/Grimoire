% Interface layer for ECS exploration and system interaction
% Returns structured data - frontends (CLI/API/MCP) handle formatting

% Interface entity
entity(interface).
component(interface, source, source(semantic(file("src/interface.pl")))).

% Interface subcommands (following git pattern)
component(interface, subcommand, compt).
component(interface, subcommand, comp).
component(interface, subcommand, doc).
component(interface, subcommand, repl).
component(interface, subcommand, status).
component(interface, subcommand, test).
component(interface, subcommand, session).
component(interface, subcommand, cast).
component(interface, subcommand, perceive).
component(interface, subcommand, run).
component(interface, subcommand, load).

% Make interface subcommands available as command constructors (like git does)
component(command, ctor, interface(C)) :- component(interface, subcommand, C).

% Also make them available as interface constructors
component(interface, ctor, C) :- component(interface, subcommand, C).

% Namespaced entities for each interface command
entity(interface(compt)).
entity(interface(comp)).
entity(interface(doc)).
entity(interface(repl)).
entity(interface(status)).
entity(interface(test)).
entity(interface(session)).
entity(interface(cast)).
entity(interface(perceive)).
entity(interface(run)).
entity(interface(load)).

% Docstrings follow namespacing pattern
docstring(interface(compt), "List all component types of current entity").
docstring(interface(comp), "List components of specific type for current entity").
docstring(interface(doc), "Show docstring of current entity").
docstring(interface(repl), "Start interactive REPL with context awareness").
docstring(interface(status), "Show session/transaction status").
docstring(interface(test), "Run the test suite").
docstring(interface(session), "Session management commands (start, close, execute)").
docstring(interface(cast), "Cast conjuration spells (mutable operations)").
docstring(interface(perceive), "Execute perception spells (query operations)").
docstring(interface(run), "Execute arbitrary command term structures (legacy)").
docstring(interface(load), "Load entity into current session for persistent access").

% Main interface docstring
docstring(interface, S) :-
    make_ctors_docstring(interface, CtorsDoc),
    S = {|string(CtorsDoc)||\n    Interface commands for ECS exploration and system interaction.\n    Format: interface(subcommand(...))\n\n    Available subcommands:\n    {CtorsDoc}\n    |}.

% === CONTEXT MANAGEMENT ===

% Auto-detect current working context
current_entity(Entity) :-
    (exists_file('./semantics.pl') ->
        % Get entity name from working directory
        working_directory(Cwd, Cwd),
        file_base_name(Cwd, DirName),
        atom_string(Entity, DirName),
        % Ensure local project is loaded
        ensure_local_project_loaded(Entity)
    ;
        % Default to system entity
        Entity = system
    ).

% Resolve entity paths with shortcuts
resolve_entity_path(PathStr, Entity) :-
    (PathStr = "/" ->
        Entity = system
    ;
        % Try loading as semantic folder (works for both "." and "path/to/something")
        catch(
            (load_entity(semantic(folder(PathStr))),
             file_base_name(PathStr, DirName),
             atom_string(Entity, DirName)),
            _,
            % If loading fails, treat as entity name directly
            atom_string(Entity, PathStr)
        )
    ).

% Load local semantics.pl if not already loaded
ensure_local_project_loaded(ProjectEntity) :-
    (entity(ProjectEntity) ->
        true  % Already loaded
    ;
        load_entity(semantic(file('./semantics.pl')))
    ).

% Load entity into current session state
load_entity_in_session(Entity) :-
    % Get current session ID
    get_current_session_id(SessionId),
    % Create a semantic specification for the entity
    entity_to_semantic_spec(Entity, EntitySpec),
    % Validate that the entity exists before adding to session
    (validate_entity_exists(Entity, EntitySpec) ->
        % Store the load in persistent session file for future operations
        (SessionId \= main ->
            add_entity_load_to_session(SessionId, EntitySpec)
        ;
            % In main session, just succeed (no persistent storage)
            true
        )
    ;
        % Entity doesn't exist - throw error
        format(atom(ErrorMsg), 'Entity ~w not found', [Entity]),
        throw(entity_load_failed(Entity, EntitySpec, ErrorMsg))
    ).

% Validate that an entity exists and can be loaded
validate_entity_exists(Entity, EntitySpec) :-
    % Check if entity already exists in the system
    (entity(Entity) ->
        true
    ;
        % Try to load the semantic specification to see if it's valid
        catch(
            load_entity(EntitySpec),
            _,
            fail
        )
    ).

% Convert entity to semantic specification
entity_to_semantic_spec(Entity, EntitySpec) :-
    % Map entities to proper semantic specifications
    (Entity = '/' ->
        EntitySpec = semantic(system)
    ; Entity = '.' ->
        EntitySpec = semantic(folder(.))
    ; atom_string(Entity, EntityStr), 
      (sub_string(EntityStr, _, _, _, '/') ->
          EntitySpec = semantic(folder(Entity))
      ;
          EntitySpec = semantic(entity(Entity))
      )
    ).

% Get current session ID from Git branch
get_current_session_id(SessionId) :-
    get_current_branch(Branch),
    (atom_concat('session-', SessionId, Branch) ->
        true  % Extract session ID from branch name
    ;
        SessionId = main  % Not in a session branch
    ).

% Ensure session state is loaded before interface operations
ensure_session_state_loaded :-
    get_current_session_id(SessionId),
    (SessionId \= main ->
        % Load session state if in a session
        load_session_state_file(SessionId)
    ;
        % In main session, no persistent state to load
        true
    ).

% === INTERFACE COMMAND IMPLEMENTATIONS ===

% Component types listing
run(command(interface(compt)), RetVal) :-
    ensure_session_state_loaded,
    current_entity(Entity),
    interface_compt(Entity, Types),
    RetVal = ok(component_types(Entity, Types)).

run(command(interface(compt(EntityPath))), RetVal) :-
    ensure_session_state_loaded,
    resolve_entity_path(EntityPath, Entity),
    interface_compt(Entity, Types),
    RetVal = ok(component_types(Entity, Types)).

% Component listing with new argument order: entity first, then type
run(command(interface(comp(EntityPath, Type))), RetVal) :-
    ensure_session_state_loaded,
    resolve_entity_path(EntityPath, Entity),
    interface_comp(Entity, Type, Components),
    RetVal = ok(components(Entity, Type, Components)).

% Documentation retrieval
run(command(interface(doc)), RetVal) :-
    ensure_session_state_loaded,
    current_entity(Entity),
    interface_doc(Entity, Doc),
    RetVal = ok(documentation(Entity, Doc)).

run(command(interface(doc(Entity))), RetVal) :-
    ensure_session_state_loaded,
    interface_doc(Entity, Doc),
    RetVal = ok(documentation(Entity, Doc)).

% REPL command - delegate to existing implementation
run(command(interface(repl)), RetVal) :-
    % Load and call existing REPL functionality
    catch(
        (ensure_loaded('src/repl.pl'), grimoire_repl_command, RetVal = ok(repl_completed))
    ,
        Error,
        RetVal = error(repl_failed(Error))
    ).

% Status command - show session/transaction status
run(command(interface(status)), RetVal) :-
    get_session_status(Status),
    RetVal = ok(session_status(Status)).

% Test command - delegate to existing implementation
run(command(interface(test)), RetVal) :-
    catch(
        (ensure_loaded('src/tests/run_tests.pl'), run_all_tests, RetVal = ok(tests_passed))
    ,
        Error,
        RetVal = error(tests_failed(Error))
    ).

% Test command with specific test arguments
run(command(interface(test(TestArgs))), RetVal) :-
    catch(
        (ensure_loaded('src/tests/run_tests.pl'), 
         (member('--list', TestArgs) ->
             (list_available_tests, 
              RetVal = ok(tests_listed))
         ;
             (run_specific_tests(TestArgs), 
              RetVal = ok(tests_passed))
         ))
    ,
        Error,
        RetVal = error(tests_failed(Error))
    ).

% Session commands - forward to session.pl
run(command(interface(session(Args))), RetVal) :-
    run(command(session(Args)), RetVal).

% Cast command - execute conjuration spells
run(command(interface(cast(SpellTerm))), RetVal) :-
    cast(SpellTerm, RetVal).

% Perceive command - execute perception spells directly
run(command(interface(perceive(QueryTerm))), RetVal) :-
    (call(QueryTerm) ->
        RetVal = ok(query_succeeded)
    ;
        RetVal = error(query_failed)
    ).

% Run command - execute arbitrary command structures (legacy)
run(command(interface(run(CommandTerm))), RetVal) :-
    run(CommandTerm, RetVal).

% Load command - load entity into current session
run(command(interface(load(EntitySpec))), RetVal) :-
    resolve_entity_path(EntitySpec, Entity),
    catch(
        (load_entity_in_session(Entity),
         RetVal = ok(entity_loaded(Entity))),
        entity_load_failed(E, Spec, Msg),
        RetVal = error(entity_load_failed(E, Spec, Msg))
    ).

% === CORE INTERFACE FUNCTIONS ===
% These return structured data, no printing

% List component types for entity
interface_compt(Entity, Types) :-
    findall(Type, component(Entity, Type, _), AllTypes),
    sort(AllTypes, Types).

% List components of specific type with entity flags
interface_comp(Entity, Type, ComponentsWithFlags) :-
    findall(comp_entry(Comp, Flag), (
        component(Entity, Type, Comp),
        (entity(Comp) -> Flag = entity ; Flag = value)
    ), ComponentsWithFlags).

% Get entity documentation
interface_doc(Entity, Doc) :-
    (docstring(Entity, Doc) ->
        true
    ;
        Doc = no_docstring_available
    ).

% Get comprehensive session status
get_session_status(Status) :-
    % Use git directly instead of session.pl predicates to avoid hanging
    run(command(git(branch(['--show-current']))), BranchResult),
    (BranchResult = ok(result(BranchOutput, _)) ->
        string_concat(BranchStr, "\n", BranchOutput),
        atom_string(CurrentBranch, BranchStr)
    ;
        CurrentBranch = main
    ),
    run(command(git(status(['--porcelain']))), StatusResult),
    (StatusResult = ok(result("", _)) ->
        WorkingStatus = clean
    ;
        WorkingStatus = dirty
    ),
    % For now, just show current session to avoid parsing issues
    Sessions = [CurrentBranch],
    Status = status_info(CurrentBranch, WorkingStatus, Sessions).

% Find all session branches
find_all_sessions(Sessions) :-
    run(command(git(branch(['--list']))), Result),
    (Result = ok(result(BranchOutput, _)) ->
        string_lines(BranchOutput, BranchLines),
        include_session_branches(BranchLines, Sessions)
    ;
        Sessions = []
    ).

% Filter branch list to only session branches
include_session_branches([], []).
include_session_branches([Line|Rest], Sessions) :-
    atom_string(LineAtom, Line),
    include_session_branches(Rest, RestSessions),  % Process rest first to avoid issues
    (atom_concat('  session-', SessionId, LineAtom) ->
        Sessions = [SessionId|RestSessions]
    ; atom_concat('* session-', SessionId, LineAtom) ->
        Sessions = [active(SessionId)|RestSessions]
    ; atom_string('  main', Line) ->
        Sessions = [main|RestSessions]
    ; atom_string('* main', Line) ->
        Sessions = [active(main)|RestSessions]
    ;
        Sessions = RestSessions
    ).