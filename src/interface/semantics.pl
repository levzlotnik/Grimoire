% Interface layer for ECS exploration and system interaction
% Returns structured data - frontends (CLI/API/MCP) handle formatting

% Interface entity - now a folder entity
:- self_entity(interface).

% Interface subcommands (following git pattern)
component(interface, subcommand, compt).
component(interface, subcommand, comp).
component(interface, subcommand, doc).
component(interface, subcommand, entities).
component(interface, subcommand, repl).
component(interface, subcommand, status).
component(interface, subcommand, test).
component(interface, subcommand, session).
component(interface, subcommand, conjure).
component(interface, subcommand, perceive).
component(interface, subcommand, load).
component(interface, subcommand, read_file).
component(interface, subcommand, edit_file).

% Removed legacy command constructor - interface functions called directly by CLI

% Also make them available as interface constructors
component(interface, ctor, C) :- component(interface, subcommand, C).

% Namespaced entities for each interface command
entity(interface(compt)).
entity(interface(comp)).
entity(interface(doc)).
entity(interface(entities)).
entity(interface(repl)).
entity(interface(status)).
entity(interface(test)).
entity(interface(session)).
entity(interface(conjure)).
entity(interface(perceive)).
entity(interface(load)).
entity(interface(read_file)).
entity(interface(edit_file)).

% Docstrings follow namespacing pattern with detailed format information
docstring(interface(compt), "List all component types of current entity. Format: interface(compt) or interface(compt(Entity)). Returns component_types(Entity, [Type1, Type2, ...]).").
docstring(interface(comp), "List components of specific type for current entity. Format: interface(comp(Entity, ComponentType)). Returns components(Entity, ComponentType, [Component1, Component2, ...]).").
docstring(interface(doc), "Show docstring of current entity. Format: interface(doc) or interface(doc(Entity)). Returns documentation(Entity, DocString).").
docstring(interface(entities), "List all entities in the system. Format: interface(entities). Returns entities([Entity1, Entity2, ...]).").
docstring(interface(repl), "Start interactive REPL with context awareness. Format: interface(repl). Interactive command that starts a Prolog REPL.").
docstring(interface(status), "Show session/transaction status. Format: interface(status). Returns session_status(status_info(Branch, WorkingStatus, Sessions)).").
docstring(interface(test), "Run the test suite. Format: interface(test) or interface(test([TestName1, TestName2, ...]). Returns test results.").
docstring(interface(session), "File-based session management with SQLite command logging. Format: interface(session([SubCommand, ...])) - use 'grimoire comp session subcommand' to see available subcommands.").
docstring(interface(conjure), "Execute conjuration spells (mutable operations). Format: interface(conjure(SpellTerm)) - use 'grimoire comp conjure ctor' to see available spells.").
docstring(interface(perceive), "Execute perception spells (query operations). Format: interface(perceive(QueryTerm)) - use 'grimoire comp perceive ctor' to see available queries.").
docstring(interface(load), "Load entity into current session for persistent access. Format: interface(load(EntitySpec)). EntitySpec examples: semantic(folder/file), system, etc.").
docstring(interface(read_file), "Read lines from a file using 1-based indexing. Format: interface(read_file(FilePath, Start, End)). Returns file content with line numbers.").
docstring(interface(edit_file), "Edit file with specified operations. Format: interface(edit_file(FilePath, Edits)). Edits is a list of edit operations.").

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

% Note: get_current_session_id/1 is now defined in session.pl

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
cast(conjure(interface(compt)), RetVal) :-
    ensure_session_state_loaded,
    current_entity(Entity),
    interface_compt(Entity, Types),
    RetVal = ok(component_types(Entity, Types)).

cast(conjure(interface(compt(EntityPath))), RetVal) :-
    ensure_session_state_loaded,
    resolve_entity_path(EntityPath, Entity),
    interface_compt(Entity, Types),
    RetVal = ok(component_types(Entity, Types)).

% Component listing with new argument order: entity first, then type
cast(conjure(interface(comp(EntityPath, Type))), RetVal) :-
    ensure_session_state_loaded,
    resolve_entity_path(EntityPath, Entity),
    interface_comp(Entity, Type, Components),
    RetVal = ok(components(Entity, Type, Components)).

% Documentation retrieval
cast(conjure(interface(doc)), RetVal) :-
    ensure_session_state_loaded,
    current_entity(Entity),
    interface_doc(Entity, Doc),
    RetVal = ok(documentation(Entity, Doc)).

cast(conjure(interface(doc(Entity))), RetVal) :-
    ensure_session_state_loaded,
    interface_doc(Entity, Doc),
    RetVal = ok(documentation(Entity, Doc)).

% Entities listing
cast(conjure(interface(entities)), RetVal) :-
    ensure_session_state_loaded,
    interface_entities(Entities),
    RetVal = ok(entities(Entities)).

% REPL command - delegate to existing implementation
cast(conjure(interface(repl)), RetVal) :-
    % Load and call existing REPL functionality
    catch(
        (ensure_loaded('src/repl.pl'), grimoire_repl_command, RetVal = ok(repl_completed))
    ,
        Error,
        RetVal = error(repl_failed(Error))
    ).

% Status command - show session/transaction status
cast(conjure(interface(status)), RetVal) :-
    get_session_status(Status),
    RetVal = ok(session_status(Status)).

% Test command - delegate to existing implementation
cast(conjure(interface(test)), RetVal) :-
    catch(
        (ensure_loaded('src/tests/run_tests.pl'), run_all_tests, RetVal = ok(tests_passed))
    ,
        Error,
        RetVal = error(tests_failed(Error))
    ).

% Test command with specific test arguments
cast(conjure(interface(test(TestArgs))), RetVal) :-
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
cast(conjure(interface(session(Args))), RetVal) :-
    cast(conjure(session(Args)), RetVal).

% Conjure command - execute conjuration spells
cast(conjure(interface(conjure(SpellTerm))), RetVal) :-
    cast(conjure(SpellTerm), RetVal).

% Perceive command - execute perception spells directly
cast(conjure(interface(perceive(QueryTerm))), RetVal) :-
    (perceive(QueryTerm) ->
        RetVal = ok(query_succeeded)
    ;
        RetVal = error(query_failed)
    ).

% Removed legacy run command

% Load command - load entity into current session
cast(conjure(interface(load(EntitySpec))), RetVal) :-
    resolve_entity_path(EntitySpec, Entity),
    catch(
        (load_entity_in_session(Entity),
         RetVal = ok(entity_loaded(Entity))),
        entity_load_failed(E, Spec, Msg),
        RetVal = error(entity_load_failed(E, Spec, Msg))
    ).

% Read file command - execute read_file perceive operation
cast(conjure(interface(read_file(FilePath, Start, End))), RetVal) :-
    ensure_session_state_loaded,
    % Convert start/end to lines list for the perceive operation
    findall(LineNum, between(Start, End, LineNum), Lines),
    (perceive(read_file(FilePath, Lines, ContentWithLineNumbers)) ->
        RetVal = ok(ContentWithLineNumbers)
    ;
        RetVal = error(read_file_failed)
    ).

% Edit file command - forward to edit_file conjure operation  
cast(conjure(interface(edit_file(FilePath, Edits))), RetVal) :-
    ensure_session_state_loaded,
    cast(conjure(edit_file(file(FilePath), Edits)), RetVal).

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
    docstring(Entity, Doc).

% List all entities
interface_entities(Entities) :-
    perceive(entities(Entities)).

% Get comprehensive session status
get_session_status(Status) :-
    % Use git directly instead of session.pl predicates to avoid hanging
    perceive(git(status(BranchResult, _, _))),
    (BranchResult = ok(result(BranchOutput, _)) ->
        string_concat(BranchStr, "\n", BranchOutput),
        atom_string(CurrentBranch, BranchStr)
    ;
        CurrentBranch = main
    ),
    perceive(git(status(_, StatusResult, _))),
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
    perceive(git(branch(Result))),
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

% === PYTHON INTERFACE SUPPORT ===

% Python-specific cast that converts Prolog terms to Python-friendly dictionaries
python_cast(conjure(ConjureStruct), PyResult) :-
    % If it's an atom, try to convert it to a term
    % This will preserve quoted atoms as atoms (e.g., 'interface(doc)' stays as atom)
    % and convert unquoted compound terms (e.g., interface(doc)) to terms
    (   atom(ConjureStruct) ->
        atom_to_term(ConjureStruct, Term, [])
    ;
        Term = ConjureStruct  % Already a term
    ),
    cast(conjure(Term), Result),
    term_struct_to_python_dict(Result, PyResult).

% Convert Prolog term structures to Python dictionaries recursively
term_struct_to_python_dict(Term, Dict) :-
    % Handle primitive types that pass through directly
    (   atomic(Term) ->
        (   atom(Term) ->
            Dict = _{type: "atom", value: Term}
        ;   string(Term) ->
            Dict = _{type: "string", value: Term}
        ;   number(Term) ->
            (   integer(Term) ->
                Dict = _{type: "int", value: Term}
            ;   Dict = _{type: "float", value: Term}
            )
        )
    ;   is_list(Term) ->
        % Convert list elements recursively
        maplist(term_struct_to_python_dict, Term, Elements),
        Dict = _{type: "list", elements: Elements}
    ;   compound(Term) ->
        % Handle compound terms by decomposing with functor and args
        compound_name_arity(Term, Functor, Arity),
        Term =.. [Functor|Args],
        maplist(term_struct_to_python_dict, Args, ConvertedArgs),
        Dict = _{type: "term_struct", functor: Functor, arity: Arity, args: ConvertedArgs}
    ;   % Fallback for any other types
        Dict = _{type: "unknown", value: Term}
    ).
