% Clean Session System - Git-backed state management
% Semantic layer on top of Git operations
:- use_module(library(uuid)).

% Session and transaction entities
entity(session).
entity(transaction).

% Session concepts
component(session, concept, session(state)).
component(session, concept, close_strategy).
component(session, concept, transition_branch).
component(session, concept, working_tree_status).

% Session state entity and constructors
entity(session(state)).
component(session(state), ctor, active).
component(session(state), ctor, closed).

docstring(session(state), 
    "Session state represents the lifecycle stage of a Git-backed session branch."
).

% Close strategy entity and constructors  
entity(close_strategy).
component(close_strategy, ctor, merge_to_main).
component(close_strategy, ctor, abandon).
component(close_strategy, ctor, keep_branch).

docstring(close_strategy,
    "Close strategy determines how a session branch is handled when closing the session."
).

% Transition branch entity
entity(transition_branch).
component(transition_branch, concept, creation).
component(transition_branch, concept, integration). 
component(transition_branch, concept, restoration).

docstring(transition_branch,
    "Transition branch provides a context-aware stash mechanism for moving dirty state to new sessions."
).

% Working tree status entity
entity(working_tree_status).
component(working_tree_status, ctor, clean).
component(working_tree_status, ctor, dirty).

docstring(working_tree_status,
    "Working tree status indicates whether there are uncommitted changes in the repository."
).

% Transaction concepts
component(transaction, concept, transaction(state)).

% Transaction state entity and constructors
entity(transaction(state)).
component(transaction(state), ctor, committed).
component(transaction(state), ctor, failed).

docstring(transaction(state),
    "Transaction state represents the outcome of atomic operations within a session."
).

% Spell constructors for session operations
% Conjure constructors (state-changing operations)
component(conjure, ctor, session(start)).
component(conjure, ctor, session(close)).
component(conjure, ctor, session(execute)).

% Perceive constructors (query operations)
component(perceive, ctor, session(current)).
component(perceive, ctor, session(list)).
component(perceive, ctor, session(status)).

% Legacy session commands for backwards compatibility
component(command, ctor, session).
component(session, subcommand, start).
component(session, subcommand, close).
component(session, subcommand, execute).

docstring(session, 
    "Clean session system where sessions are Git branches and transactions are Git commits.\nThree transition patterns:\n1. Clean state → any session: Direct git checkout\n2. Dirty state → existing session: Git checkout (let git handle conflicts)\n3. Dirty state → NEW session: Via transition branch workflow"
).

% === CORE SESSION TRANSITIONS ===

% Main session start entry point
start_session(SessionId, Result) :-
    check_working_tree_status(Status),
    transition_to_session(SessionId, Status, Result).

% Pattern 1: Clean state → any session (existing or new)
transition_to_session(SessionId, clean, Result) :-
    (session_exists(SessionId) ->
        session_branch_name(SessionId, BranchName),
        run(command(git(checkout([BranchName]))), GitResult),
        (GitResult = ok(_) ->
            Result = ok(session_started(SessionId, existing, clean, direct))
        ;
            Result = error(failed_to_checkout_existing_session(GitResult))
        )
    ;
        session_branch_name(SessionId, BranchName),
        run(command(git(checkout(['-b', BranchName]))), GitResult),
        (GitResult = ok(_) ->
            Result = ok(session_started(SessionId, new, clean, direct))
        ;
            Result = error(failed_to_create_new_session(GitResult))
        )
    ).

% Pattern 2: Dirty state → existing session (let git handle merge/conflicts)
transition_to_session(SessionId, dirty, Result) :-
    session_exists(SessionId), !,
    session_branch_name(SessionId, BranchName),
    run(command(git(checkout([BranchName]))), GitResult),
    (GitResult = ok(_) ->
        Result = ok(session_started(SessionId, existing, dirty, direct_with_merge))
    ;
        Result = error(failed_to_checkout_existing_with_dirty(GitResult))
    ).

% Pattern 3: Dirty state → NEW session (via transition branch)
transition_to_session(SessionId, dirty, Result) :-
    % Only for NEW sessions - create transition workflow
    create_transition_workflow(SessionId, Result).

% === TRANSITION BRANCH WORKFLOW ===
% Only used for dirty state → NEW session

create_transition_workflow(SessionId, Result) :-
    get_current_branch(SourceBranch),
    transition_branch_name(SourceBranch, SessionId, TransitionBranch),
    session_branch_name(SessionId, SessionBranch),
    
    % Step 1: Create transition branch from current branch
    run(command(git(checkout(['-b', TransitionBranch]))), CreateResult),
    (CreateResult = ok(_) ->
        % Step 2: Commit dirty changes to transition branch
        commit_dirty_changes(TransitionBranch, SessionId, CommitResult),
        (CommitResult = ok(_) ->
            % Step 3: Create new session branch from transition branch
            run(command(git(checkout(['-b', SessionBranch]))), FinalResult),
            (FinalResult = ok(_) ->
                Result = ok(session_started(SessionId, new, dirty, via_transition(TransitionBranch)))
            ;
                Result = error(failed_to_create_session_from_transition(FinalResult))
            )
        ;
            Result = error(failed_to_commit_to_transition(CommitResult))
        )
    ;
        Result = error(failed_to_create_transition_branch(CreateResult))
    ).

commit_dirty_changes(_, SessionId, Result) :-
    % Add tracked changes only (not untracked files)
    run(command(git(add(['-u']))), AddResult),
    (AddResult = ok(_) ->
        format(string(CommitMsg), "Transition to session ~w: dirty state preserved", [SessionId]),
        run(command(git(commit(['-m', CommitMsg]))), CommitResult),
        Result = CommitResult
    ;
        Result = error(failed_to_stage_changes(AddResult))
    ).

% === SESSION MANAGEMENT ===

session_exists(SessionId) :-
    session_branch_name(SessionId, BranchName),
    run(command(git(branch(['--list', BranchName]))), Result),
    Result = ok(result(Output, _)),
    Output \= "".

session_branch_name(SessionId, BranchName) :-
    format(atom(BranchName), 'session-~w', [SessionId]).

transition_branch_name(SourceBranch, SessionId, TransitionBranch) :-
    format(atom(TransitionBranch), 'transition_branch/~w--~w', [SourceBranch, SessionId]).

get_current_branch(Branch) :-
    run(command(git(['branch', '--show-current'])), Result),
    Result = ok(result(BranchOutput, _)),
    % Strip newline and convert to atom
    string_concat(BranchStr, "\n", BranchOutput),
    atom_string(Branch, BranchStr).

% === WORKING TREE STATUS ===

check_working_tree_status(Status) :-
    run(command(git(status(['--porcelain']))), Result),
    (Result = ok(result("", _)) ->
        Status = clean
    ;
        Status = dirty
    ).

% === SESSION OPERATIONS ===

run(command(session(start)), RetVal) :-
    uuid(SessionId),
    start_session(SessionId, RetVal).

run(command(session(start(SessionId))), RetVal) :-
    start_session(SessionId, RetVal).

run(command(session(close(SessionId, Strategy))), RetVal) :-
    close_session(SessionId, Strategy, RetVal).

run(command(session(execute(SessionId, Commands))), RetVal) :-
    execute_transaction_in_session(SessionId, Commands, RetVal).

% === SESSION CLOSURE ===

close_session(SessionId, merge_to_main, Result) :-
    session_branch_name(SessionId, SessionBranch),
    % Switch to main and merge session
    run(command(git(checkout(['main']))), CheckoutResult),
    (CheckoutResult = ok(_) ->
        run(command(git(merge(['--no-ff', SessionBranch]))), MergeResult),
        (MergeResult = ok(_) ->
            % Clean up session branch
            run(command(git(branch(['-d', SessionBranch]))), _),
            % Clean up any associated transition branch
            cleanup_session_transition_branch(SessionId),
            Result = ok(session_closed(SessionId, merged_to_main))
        ;
            Result = error(failed_to_merge_session(MergeResult))
        )
    ;
        Result = error(failed_to_checkout_main(CheckoutResult))
    ).

close_session(SessionId, abandon, Result) :-
    session_branch_name(SessionId, SessionBranch),
    % Switch to main and delete session branch
    run(command(git(checkout(['main']))), CheckoutResult),
    (CheckoutResult = ok(_) ->
        run(command(git(branch(['-D', SessionBranch]))), DeleteResult),
        (DeleteResult = ok(_) ->
            % For abandoned sessions, restore dirty state from transition branch if it exists
            restore_from_transition_branch(SessionId),
            Result = ok(session_closed(SessionId, abandoned))
        ;
            Result = error(failed_to_delete_session_branch(DeleteResult))
        )
    ;
        Result = error(failed_to_checkout_main_for_abandon(CheckoutResult))
    ).

close_session(SessionId, keep_branch, Result) :-
    run(command(git(checkout(['main']))), CheckoutResult),
    (CheckoutResult = ok(_) ->
        Result = ok(session_closed(SessionId, branch_kept))
    ;
        Result = error(failed_to_checkout_main_for_keep(CheckoutResult))
    ).

% === TRANSITION BRANCH CLEANUP ===

cleanup_session_transition_branch(SessionId) :-
    get_current_branch(CurrentBranch),
    transition_branch_name(CurrentBranch, SessionId, TransitionBranch),
    % Try to delete transition branch if it exists
    catch(run(command(git(branch(['-D', TransitionBranch]))), _), _, true).

restore_from_transition_branch(SessionId) :-
    get_current_branch(CurrentBranch), 
    transition_branch_name(CurrentBranch, SessionId, TransitionBranch),
    % Check if transition branch exists
    run(command(git(branch(['--list', TransitionBranch]))), Result),
    (Result = ok(result(Output, _)), Output \= "" ->
        % Apply changes from transition branch as unstaged changes
        run(command(git(checkout([TransitionBranch, '--', '.']))), _),
        run(command(git(reset(['HEAD']))), _),  % Unstage the changes
        % Delete transition branch
        run(command(git(branch(['-D', TransitionBranch]))), _)
    ;
        true  % No transition branch to restore from
    ).

% === TRANSACTION EXECUTION ===

execute_transaction_in_session(SessionId, Commands, Result) :-
    session_branch_name(SessionId, SessionBranch),
    % Ensure we're on the session branch
    run(command(git(checkout([SessionBranch]))), CheckoutResult),
    (CheckoutResult = ok(_) ->
        % Execute commands atomically
        execute_commands_atomically(Commands, ExecuteResult),
        (ExecuteResult = ok(Results) ->
            % Commit as transaction
            create_transaction_commit(Commands, CommitResult),
            (CommitResult = ok(_) ->
                Result = ok(transaction_executed(Results, committed))
            ;
                Result = error(transaction_commit_failed(CommitResult))
            )
        ;
            Result = error(transaction_execution_failed(ExecuteResult))
        )
    ;
        Result = error(failed_to_switch_to_session(CheckoutResult))
    ).

execute_commands_atomically(Commands, Result) :-
    % Get commit to rollback to if needed
    run(command(git(['rev-parse', 'HEAD'])), CommitResult),
    (CommitResult = ok(result(PreCommit, _)) ->
        execute_commands(Commands, Results),
        (member(error(_), Results) ->
            % Rollback on failure
            string_concat(PreCommitClean, "\n", PreCommit),
            run(command(git(reset(['--hard', PreCommitClean]))), _),
            Result = error(commands_failed_and_rolled_back)
        ;
            Result = ok(Results)
        )
    ;
        Result = error(failed_to_get_current_commit)
    ).

create_transaction_commit(Commands, Result) :-
    length(Commands, NumCmds),
    format(string(CommitMsg), "Transaction: ~w commands executed", [NumCmds]),
    run(command(git(commit(['-a', '-m', CommitMsg]))), Result).

% === COMMAND EXECUTION ===
% Uses execute_commands/2 from grimoire.pl core system

% === PERCEIVE PREDICATES - Session Queries ===

% Get current session information
perceive(session(current(SessionId, State))) :-
    get_current_branch(Branch),
    (atom_concat('session-', SessionId, Branch) ->
        State = active
    ;
        SessionId = main,
        State = main_session
    ).

% List all available sessions
perceive(session(list(Sessions))) :-
    run(command(git(branch(['--list']))), Result),
    (Result = ok(result(BranchOutput, _)) ->
        string_lines(BranchOutput, BranchLines),
        findall(Session, (
            member(Line, BranchLines),
            parse_session_branch(Line, Session)
        ), Sessions)
    ;
        Sessions = []
    ).

% Parse branch lines to extract session information
parse_session_branch(Line, Session) :-
    atom_string(LineAtom, Line),
    (atom_concat('  session-', SessionId, LineAtom) ->
        Session = session(SessionId, inactive)
    ; atom_concat('* session-', SessionId, LineAtom) ->
        Session = session(SessionId, active)
    ; atom_string('  main', Line) ->
        Session = session(main, inactive)
    ; atom_string('* main', Line) ->
        Session = session(main, active)
    ;
        fail
    ).

% Get comprehensive session status
perceive(session(status(CurrentSession, WorkingStatus, AllSessions))) :-
    perceive(session(current(CurrentSession, _))),
    check_working_tree_status(WorkingStatus),
    perceive(session(list(AllSessions))).