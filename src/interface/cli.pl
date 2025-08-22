#!/usr/bin/env swipl
% Grimoire - Knowledge-Based Operating System CLI
% Interface wrapper that formats results from interface.pl

:- initialization(main, main).

% Load grimoire system + interface layer
:- ensure_loaded('src/grimoire.pl').
:- ensure_loaded('src/interface/semantics.pl').

% Main entry point - parse command line arguments
main(Args) :-
    (Args = [] ->
        show_usage,
        halt(1)
    ; Args = [Command|RestArgs] ->
        (handle_command(Command, RestArgs) ->
            halt(0)
        ;
            halt(1)
        )
    ; 
        show_usage,
        halt(1)
    ).

% Auto-generated usage from interface components
show_usage :-
    writeln(''),
    writeln('🔮 Grimoire - Knowledge-Based Operating System'),
    writeln(''),
    catch(current_entity(CurrentEntity), _, CurrentEntity = system),
    format('Context: ~w~n~n', [CurrentEntity]),
    writeln('Available commands:'),
    findall(Cmd, component(interface, subcommand, Cmd), Commands),
    forall(member(Cmd, Commands), (
        (docstring(interface(Cmd), Doc) ->
            true
        ;
            Doc = "No documentation available"
        ),
        format('  ~w~t~12| - ~w~n', [Cmd, Doc])
    )),
    writeln(''),
    writeln('Usage patterns:'),
    writeln('  grimoire compt [entity]         # List component types'),
    writeln('  grimoire comp <entity> <type>   # List components of entity'),
    writeln('  grimoire doc [entity]           # Show documentation'),
    writeln(''),
    writeln('Entity paths:'),
    writeln('  /                               # System entity'),
    writeln('  .                               # Current directory entity'),
    writeln('  path/to/semantic/folder         # Custom semantic entity'),
    writeln(''),
    writeln('Spell-based commands:'),
    writeln('  grimoire conjure "operation"    # Cast conjuration spells'),
    writeln('  grimoire perceive "query(...)"  # Execute perception queries'),
    writeln('').

% === COMMAND DISPATCH ===

% Component types command
handle_command(compt, []) :-
    !,
    cast(conjure(interface(compt)), Result),
    format_cli_result(Result).

handle_command(compt, [EntityStr]) :-
    !,
    cast(conjure(interface(compt(EntityStr))), Result),
    format_cli_result(Result).

% Component listing command - requires both entity and component type
handle_command(comp, [EntityStr, TypeStr]) :-
    !,
    cast(conjure(interface(comp(EntityStr, TypeStr))), Result),
    format_cli_result(Result).

% Documentation command
handle_command(doc, []) :-
    !,
    cast(conjure(interface(doc)), Result),
    format_cli_result(Result).

handle_command(doc, [EntityStr]) :-
    !,
    atom_string(EntityAtom, EntityStr),
    cast(conjure(interface(doc(EntityAtom))), Result),
    format_cli_result(Result).

% REPL command
handle_command(repl, _) :-
    !,
    cast(conjure(interface(repl)), Result),
    format_cli_result(Result).

% Status command  
handle_command(status, _) :-
    !,
    cast(conjure(interface(status)), Result),
    format_cli_result(Result).

% Test command
handle_command(test, []) :-
    !,
    cast(conjure(interface(test)), Result),
    format_cli_result(Result).

handle_command(test, TestArgs) :-
    !,
    cast(conjure(interface(test(TestArgs))), Result),
    format_cli_result(Result).

% Session commands
handle_command(session, [SubCmd|Args]) :-
    !,
    % Check for help flags first
    ((SubCmd = '--help' ; SubCmd = '-h') ->
        show_session_help
    ;
        % Parse session subcommand
        atom_string(SubCmdAtom, SubCmd),
        parse_session_args(SubCmdAtom, Args, SessionArgs),
        cast(conjure(interface(session(SessionArgs))), Result),
        format_cli_result(Result)
    ).

% Handle session command with no args (show help)
handle_command(session, []) :-
    !,
    show_session_help.

% Conjure command with --list support
handle_command(conjure, ['--list']) :-
    !,
    findall(Op, component(conjure, ctor, Op), Operations),
    sort(Operations, SortedOps),
    format('Available conjure operations:~n'),
    forall(member(Op, SortedOps), format('  ~w~n', [Op])).

% Conjure command - execute conjuration spells (validation is whether cast succeeds)
handle_command(conjure, [SpellStr]) :-
    !,
    % Parse and try to cast - if it fails, it's an invalid conjure
    catch(
        (read_term_from_atom(SpellStr, SpellTerm, []),
         (cast(conjure(interface(conjure(SpellTerm))), Result) ->
             format_cli_result(Result)
         ;
             format('~nSorry, "~w" is not a proper conjuration spell. 🧙‍♂️~n', [SpellTerm]),
             format('Try checking available conjure operations with: grimoire conjure --list~n'),
             fail
         )),
        Error,
        (format_cli_result(error(invalid_spell_syntax(Error))), fail)
    ).

% Perceive command with --list support
handle_command(perceive, ['--list']) :-
    !,
    findall(Op, component(perceive, ctor, Op), Operations),
    sort(Operations, SortedOps),
    format('Available perceive operations:~n'),
    forall(member(Op, SortedOps), format('  ~w~n', [Op])).

% Perceive command - execute perception spells with auto-variable output
handle_command(perceive, [QueryStr]) :-
    !,
    % Parse the query string as a Prolog term with variable names
    catch(
        (read_term_from_atom(QueryStr, QueryTerm, [variable_names(VarNames)]),
         execute_perceive_query(QueryTerm, VarNames)),
        Error,
        format_cli_result(error(invalid_query_syntax(Error)))
    ).

% Removed legacy run command

% Load command - load entity into current session
handle_command(load, [EntityStr]) :-
    !,
    cast(conjure(interface(load(EntityStr))), Result),
    format_cli_result(Result).

% Exec command - execute arbitrary Prolog queries
handle_command(exec, [QueryStr]) :-
    !,
    % Parse and execute the query directly
    catch(
        (read_term_from_atom(QueryStr, QueryTerm, [variable_names(VarNames)]),
         call(QueryTerm),
         emit_variable_bindings(VarNames)),
        Error,
        format_cli_result(error(query_execution_failed(Error)))
    ).

% Unknown command
handle_command(Command, _) :-
    format('Unknown command: ~w~n~n', [Command]),
    show_usage,
    fail.

% === CLI FORMATTING FUNCTIONS ===

% Format component types result
format_cli_result(ok(component_types(Entity, Types))) :-
    !,
    format('~w component types:~n', [Entity]),
    forall(member(Type, Types), format('  ~w~n', [Type])).

% Format components listing result
format_cli_result(ok(components(Entity, Type, Components))) :-
    !,
    format('~w components of type "~w":~n~n', [Entity, Type]),
    format('Component~t~28| | v/e~n'),
    format('~`-t~32|~n'),
    forall(member(comp_entry(Comp, Flag), Components), (
        (Flag = entity -> DisplayFlag = 'e' ; DisplayFlag = 'v'),
        format('~w~t~28| | ~w~n', [Comp, DisplayFlag])
    )).

% Format documentation result - no docstring available
format_cli_result(ok(documentation(Entity, no_docstring_available))) :-
    !,
    format('No docstring available for ~w~n', [Entity]).

% Format documentation result - has docstring
format_cli_result(ok(documentation(Entity, Doc))) :-
    !,
    format('~w:~n~w~n', [Entity, Doc]).

% Format session status result
format_cli_result(ok(session_status(status_info(CurrentBranch, WorkingStatus, Sessions)))) :-
    !,
    format('🔮 Grimoire Session Status~n~n'),
    format('Current Branch: ~w~n', [CurrentBranch]),
    format('Working Tree: ~w~n~n', [WorkingStatus]),
    format('Available Sessions:~n'),
    format_sessions(Sessions).

% Format success results
format_cli_result(ok(repl_completed)) :-
    !,
    true.  % REPL handles its own output

format_cli_result(ok(tests_passed)) :-
    !,
    writeln('All tests passed ✓').

format_cli_result(ok(tests_listed)) :-
    !,
    true.  % list_available_tests handles its own output

% Format session results
format_cli_result(ok(session_started(SessionId, Type, State, Method))) :-
    !,
    format('Session ~w started (~w session, ~w state, ~w)~n', [SessionId, Type, State, Method]).

format_cli_result(ok(session_closed(SessionId, Strategy))) :-
    !,
    format('Session ~w closed (~w)~n', [SessionId, Strategy]).

format_cli_result(ok(transaction_executed(Results, State))) :-
    !,
    length(Results, NumResults),
    format('Transaction executed (~w) with ~w results~n', [State, NumResults]).

% Format session results
format_cli_result(ok(session_switched(SessionId))) :-
    !,
    format('Switched to session: ~w~n', [SessionId]).

format_cli_result(ok(session_list(Sessions))) :-
    !,
    format('Available Sessions:~n'),
    format_sessions_list(Sessions).

format_cli_result(ok(session_commit_created(Message))) :-
    !,
    format('Session commit created: ~w~n', [Message]).

format_cli_result(ok(session_rollback_completed)) :-
    !,
    writeln('Session rolled back to previous transaction').

% Format entity loaded result
format_cli_result(ok(entity_loaded(Entity))) :-
    !,
    format('Entity "~w" loaded into current session ✓~n', [Entity]).

format_cli_result(ok(comprehensive_session_status(CurrentSession, WorkingStatus, AllSessions, RecentCommits))) :-
    !,
    format('🔮 Comprehensive Session Status~n~n'),
    format('Current Session: ~w~n', [CurrentSession]),
    format('Working Tree: ~w~n~n', [WorkingStatus]),
    format('Available Sessions:~n'),
    format_sessions_list(AllSessions),
    format('~nRecent Commits:~n~w~n', [RecentCommits]).

% Format error results
format_cli_result(error(session_not_found(SessionId))) :-
    !,
    format('Error: Session "~w" not found. Use "grimoire session list" to see available sessions.~n', [SessionId]),
    fail.

format_cli_result(error(not_in_session_branch(Branch))) :-
    !,
    format('Error: Not in a session branch (currently on "~w"). Use "grimoire session start" to create a session.~n', [Branch]),
    fail.

format_cli_result(error(failed_to_switch_to_session(GitResult))) :-
    !,
    format('Error: Failed to switch to session. Git error: ~w~n', [GitResult]),
    fail.

% Format session validation errors
format_cli_result(error(invalid_session_id(SessionId))) :-
    !,
    format('Error: Invalid session ID "~w". Session IDs must be alphanumeric with dashes/underscores, 1-100 characters.~n', [SessionId]),
    fail.

format_cli_result(error(not_in_git_repository)) :-
    !,
    format('Error: Not in a Git repository. Sessions require a Git repository to function.~n', []),
    fail.

% Format entity load errors
format_cli_result(error(entity_load_failed(Entity, EntitySpec, Msg))) :-
    !,
    format('Error: Failed to load entity "~w" (~w): ~w~n', [Entity, EntitySpec, Msg]),
    fail.

format_cli_result(error(Reason)) :-
    !,
    format('Error: ~w~n', [Reason]),
    fail.

% Format command execution results
format_cli_result(ok(result(Stdout, Stderr))) :-
    !,
    (Stdout \= "" -> write(Stdout) ; true),
    (Stderr \= "" -> (write('STDERR: '), write(Stderr)) ; true).

% Fallback for unexpected results
format_cli_result(Result) :-
    format('Unexpected result: ~w~n', [Result]).

% === SESSION FORMATTING HELPERS ===

format_sessions([]) :-
    writeln('  (no sessions found)').
    
format_sessions(Sessions) :-
    Sessions \= [],
    format_all_sessions(Sessions).

format_all_sessions([]).
format_all_sessions([Session|Rest]) :-
    format_single_session(Session),
    format_all_sessions(Rest).

% Format session list (for session list command)
format_sessions_list([]) :-
    writeln('  (no sessions found)').

format_sessions_list(Sessions) :-
    Sessions \= [],
    format_all_sessions_list(Sessions).

format_all_sessions_list([]).
format_all_sessions_list([Session|Rest]) :-
    format_single_session_list(Session),
    format_all_sessions_list(Rest).

format_single_session_list(session(main, active)) :-
    !,
    writeln('  * main (active, main session)').

format_single_session_list(session(main, inactive)) :-
    !,
    writeln('    main (main session)').

format_single_session_list(session(SessionId, active)) :-
    !,
    format('  * session-~w (active)~n', [SessionId]).

format_single_session_list(session(SessionId, inactive)) :-
    !,
    format('    session-~w~n', [SessionId]).

format_single_session(active(main)) :-
    !,
    writeln('  * main (active, main session)').
    
format_single_session(main) :-
    !,
    writeln('    main (main session)').
    
format_single_session(active(SessionId)) :-
    !,
    extract_session_display_name(SessionId, DisplayName),
    format('  * ~w (active)~n', [DisplayName]).
    
format_single_session(SessionId) :-
    extract_session_display_name(SessionId, DisplayName),
    format('    ~w~n', [DisplayName]).

% Extract display name from branch name or session ID
extract_session_display_name(BranchName, DisplayName) :-
    atom_string(BranchName, BranchStr),
    (string_concat('session-', _, BranchStr) ->
        % Full branch name - use as-is
        DisplayName = BranchName
    ;
        % Session ID only - add prefix
        format(atom(DisplayName), 'session-~w', [BranchName])
    ).

% === SESSION ARGUMENT PARSING ===

% Parse session subcommand arguments
parse_session_args(start, [], start) :- !.
parse_session_args(start, [SessionIdStr], start(SessionId)) :-
    !,
    atom_string(SessionId, SessionIdStr).
parse_session_args(close, [SessionIdStr, StrategyStr], close(SessionId, Strategy)) :-
    !,
    atom_string(SessionId, SessionIdStr),
    atom_string(Strategy, StrategyStr).
parse_session_args(switch, [SessionIdStr], switch(SessionId)) :-
    !,
    atom_string(SessionId, SessionIdStr).
parse_session_args(list, [], list) :- !.
parse_session_args(commit, [MessageStr], commit(Message)) :-
    !,
    atom_string(Message, MessageStr).
parse_session_args(rollback, [], rollback) :- !.
parse_session_args(status, [], status) :- !.

% === SESSION HELP SYSTEM ===

% Show session help information
show_session_help :-
    writeln(''),
    writeln('🔮 Grimoire Session Management'),
    writeln(''),
    writeln('File-based session system with SQLite command logging.'),
    writeln('Sessions accumulate commands in workspace directories.'),
    writeln(''),
    writeln('Available commands:'),
    writeln('  start [<id>]              Start new session (auto-generated ID if none provided)'),
    writeln('  history                   Show command history for current session'),
    writeln('  commit_accumulated <msg>  Commit accumulated commands (placeholder)'),
    writeln(''),
    writeln('Examples:'),
    writeln('  grimoire session start                    # Start new session'),
    writeln('  grimoire session start my-session        # Start named session'),
    writeln('  grimoire session history                  # Show session history'),
    writeln('  grimoire session commit_accumulated "msg" # Commit commands'),
    writeln(''),
    writeln('Note: Sessions store commands in SQLite databases within'),
    writeln('      workspace directories under $GRIMOIRE_ROOT/sessions/'),
    writeln('').

% === PERCEIVE QUERY EXECUTION ===

% Execute a perceive query - validation is whether perceive/1 succeeds
execute_perceive_query(QueryTerm, VarNames) :-
    % Simply try to execute the perceive query - if it fails, it's invalid
    catch(
        (findall(VarNames, perceive(QueryTerm), Solutions),
         (Solutions = [] ->
             format('~nSorry, "~w" is not a proper perception spell. 🔮~n', [QueryTerm]),
             format('Try checking available perceive operations with: grimoire comp perceive ctor~n'),
             fail
         ;
             emit_all_solutions(Solutions)
         )),
        Error,
        (format_cli_result(error(invalid_perceive_query(Error))), fail)
    ).

% Emit all solutions separated by semicolons
emit_all_solutions([]).
emit_all_solutions([Solution]) :-
    !,
    emit_variable_bindings(Solution).
emit_all_solutions([Solution|Rest]) :-
    emit_variable_bindings(Solution),
    write(' ;'),
    nl,
    emit_all_solutions(Rest).

% Emit variable bindings in Variable = Value format
emit_variable_bindings([]).
emit_variable_bindings([Name=Value|Rest]) :-
    format('~w = ~w~n', [Name, Value]),
    emit_variable_bindings(Rest).