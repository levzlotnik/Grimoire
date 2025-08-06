% Session and Transaction System - Git-backed state management
:- use_module(library(uuid)).
:- use_module(library(strings)).

% Session and transaction entities
entity(session).
entity(transaction).
% Core session components
component(session, state, active).
component(session, state, closed).
component(session, state, suspended).
component(session, state, merged).
component(session, state, abandoned).

% Session branch strategies - every session creates new branch
component(session, branch_strategy, new_branch).

% Session closure strategies
component(session, close_strategy, merge_to_main).
component(session, close_strategy, abandon).
component(session, close_strategy, keep_branch).

% Transaction states
component(transaction, state, planned).
component(transaction, state, executing).
component(transaction, state, committed).
component(transaction, state, failed).
component(transaction, state, rolled_back).

% Dynamic session tracking
:- dynamic([
    active_session/3,     % active_session(SessionId, BranchName, StartCommit)
    session_transaction/4 % session_transaction(SessionId, TransactionId, Commands, State)
]).

docstring(session,
    {|string(_)||
    A session represents a logical work context that creates a Git branch for isolation.
    Each session automatically creates its own branch, enabling concurrent work streams without conflicts.

    Session Lifecycle:
    1. start_session/2 - Create session (always creates new branch)
    2. execute_transaction/3 - Execute atomic operations within session
    3. close_session/3 - Merge, abandon, or keep session branch

    Format: session(SessionId, CloseStrategy)
    - CloseStrategy: merge_to_main | abandon | keep_branch

    Benefits:
    - Session isolation via Git branches (always new branch)
    - Concurrent work streams
    - Atomic transaction rollback
    - Rich Git-native history
    |}
).

docstring(transaction,
    {|string(_)||
    A transaction is an atomic set of operations within a session that results in a Git commit.
    All operations in a transaction succeed together or fail together with Git-based rollback.

    Transaction Lifecycle:
    1. Commands executed atomically
    2. Git reset --hard on failure (rollback)
    3. Git commit on success with descriptive message
    4. Transaction ID tagged for easy reference

    Format: transaction(SessionId, Commands)
    Commands: List of command(Entity(Action)) terms

    Benefits:
    - Atomic operations with rollback
    - Git commit provides transaction boundary
    - Rich commit messages with command summaries
    - Transaction history via Git log
    |}
).

% Session management predicates

% Start a new session with branch creation
start_session(SessionId, Result) :-
    uuid(SessionId),
    get_current_commit(StartCommit),
    create_session_branch(SessionId, BranchName, Result),

    % Track active session
    assertz(active_session(SessionId, BranchName, StartCommit)),
    format("Session ~w started on branch: ~w~n", [SessionId, BranchName]).

% Create a new branch for the session
create_session_branch(SessionId, BranchName, Result) :-
    format(string(BranchName), "session-~w", [SessionId]),
    run(command(git(checkout(['-b', BranchName]))), CheckoutResult),
    (CheckoutResult = ok(_) ->
        Result = ok(branch_created(BranchName))
    ;
        Result = error(failed_to_create_branch(CheckoutResult))
    ).

% Close a session
close_session(SessionId, CloseStrategy, Result) :-
    active_session(SessionId, BranchName, StartCommit),

    (CloseStrategy = merge_to_main ->
        merge_session_to_main(SessionId, BranchName, Result)
    ; CloseStrategy = abandon ->
        abandon_session(SessionId, BranchName, Result)
    ; CloseStrategy = keep_branch ->
        Result = ok(branch_kept(BranchName))
    ),

    % Clean up session tracking
    retract(active_session(SessionId, BranchName, StartCommit)),
    retractall(session_transaction(SessionId, _, _, _)),
    format("Session ~w closed: ~w~n", [SessionId, Result]).

% Merge session branch back to main
merge_session_to_main(_SessionId, BranchName, Result) :-
    run(command(git(checkout(['main']))), _),
    run(command(git(merge(['--no-ff', BranchName]))), MergeResult),
    (MergeResult = ok(_) ->
        run(command(git(branch(['-d', BranchName]))), _),
        Result = ok(merged_and_deleted(BranchName))
    ;
        Result = error(merge_failed(MergeResult))
    ).

% Abandon session (delete branch)
abandon_session(_SessionId, BranchName, Result) :-
    run(command(git(checkout(['main']))), _),
    run(command(git(branch(['-D', BranchName]))), DeleteResult),
    (DeleteResult = ok(_) ->
        Result = ok(abandoned(BranchName))
    ;
        Result = error(delete_failed(DeleteResult))
    ).

% Transaction execution within sessions

% Execute transaction in session context
execute_transaction(SessionId, Commands, Result) :-
    active_session(SessionId, BranchName, _),
    uuid(TransactionId),

    % Switch to session branch
    run(command(git(checkout([BranchName]))), _),

    % Record transaction start
    assertz(session_transaction(SessionId, TransactionId, Commands, executing)),

    % Execute commands
    execute_commands_atomically(Commands, ExecuteResult),

    (ExecuteResult = ok(Results) ->
        % Commit transaction
        commit_transaction(SessionId, TransactionId, Commands, CommitResult),
        (CommitResult = ok(CommitHash) ->
            retract(session_transaction(SessionId, TransactionId, Commands, executing)),
            assertz(session_transaction(SessionId, TransactionId, Commands, committed)),
            Result = ok(transaction_committed(TransactionId, CommitHash, Results))
        ;
            % Commit failed - rollback
            rollback_transaction(SessionId, TransactionId),
            Result = error(commit_failed(CommitResult))
        )
    ;
        % Commands failed - rollback
        rollback_transaction(SessionId, TransactionId),
        Result = error(commands_failed(ExecuteResult))
    ).

% Execute commands with rollback capability
execute_commands_atomically(Commands, Result) :-
    get_current_commit(PreCommit),
    execute_commands(Commands, Results),

    % Check if any command failed
    (member(error(E), Results) ->
        % Rollback to pre-execution state
        run(command(git(reset(['--hard', PreCommit]))), _),
        Result = error(E)
    ;
        Result = ok(Results)
    ).

% Commit transaction with simple message and individual src files
commit_transaction(_SessionId, _TransactionId, _Commands, Result) :-
    % Find and add files in src directory
    find_src_files(SrcFiles),
    add_src_files(SrcFiles, AddResult),

    (AddResult = ok(_) ->
        % Commit with simple message
        run(command(git(commit(['-m', 'saving work']))), CommitResult),
        (CommitResult = ok(_) ->
            get_current_commit(CommitHash),
            Result = ok(CommitHash)
        ;
            Result = error(CommitResult)
        )
    ;
        Result = error(AddResult)
    ).

% Rollback transaction
rollback_transaction(SessionId, TransactionId) :-
    retract(session_transaction(SessionId, TransactionId, Commands, executing)),
    assertz(session_transaction(SessionId, TransactionId, Commands, rolled_back)),
    format("Transaction ~w rolled back~n", [TransactionId]).

% Helper predicates for staging src files
find_src_files(SrcFiles) :-
    expand_file_name('src/**/*.pl', PlFiles),
    expand_file_name('src/**/*.py', PyFiles),
    expand_file_name('src/**/*.md', MdFiles),
    append(PlFiles, PyFiles, TempFiles),
    append(TempFiles, MdFiles, SrcFiles).

add_src_files([], ok(no_files)).
add_src_files(SrcFiles, Result) :-
    SrcFiles \= [],
    run(command(git(add(SrcFiles))), Result).

% Helper predicates

% Get current Git commit hash
get_current_commit(Commit) :-
    run(command(git(rev_parse(['HEAD']))), ok(CommitOutput)),
    string_trim(CommitOutput, Commit).

% Get current Git branch
get_current_branch(Branch) :-
    run(command(git(branch(['--show-current']))), ok(BranchOutput)),
    string_trim(BranchOutput, Branch).

% Create readable command summary
summarize_commands(Commands, Summary) :-
    maplist(command_to_string, Commands, Strings),
    atomic_list_concat(Strings, ', ', Summary).

command_to_string(command(Term), String) :-
    functor(Term, Entity, _),
    Term =.. [Entity|Args],
    (Args = [Action] ->
        format(atom(String), '~w:~w', [Entity, Action])
    ;
        format(atom(String), '~w', [Entity])
    ).

% Session and transaction queries

% List active sessions
list_active_sessions(Sessions) :-
    findall(session(Id, Branch, Start), active_session(Id, Branch, Start), Sessions).

% List transactions for a session
list_session_transactions(SessionId, Transactions) :-
    findall(transaction(TId, Commands, State),
            session_transaction(SessionId, TId, Commands, State),
            Transactions).

% Show session history (Git log for session branch)
show_session_history(SessionId) :-
    active_session(SessionId, BranchName, _),
    run(command(git(log(['--oneline', '--graph', BranchName]))), ok(History)),
    format("Session ~w history:~n~w~n", [SessionId, History]).

% Session workflow example predicates

% Convenience predicate for quick session workflow
with_session(Commands, Result) :-
    start_session(SessionId, _),
    execute_transaction(SessionId, Commands, TransactionResult),
    close_session(SessionId, merge_to_main, CloseResult),
    Result = session_completed(SessionId, TransactionResult, CloseResult).

% Multi-transaction session workflow
session_workflow(SessionId, TransactionsList, Result) :-
    start_session(SessionId, _),
    execute_session_transactions(SessionId, TransactionsList, Results),
    close_session(SessionId, merge_to_main, CloseResult),
    Result = session_workflow_completed(SessionId, Results, CloseResult).

% Execute multiple transactions in sequence
execute_session_transactions(_, [], []).
execute_session_transactions(SessionId, [Commands|Rest], [Result|Results]) :-
    execute_transaction(SessionId, Commands, Result),
    (Result = ok(_) ->
        execute_session_transactions(SessionId, Rest, Results)
    ;
        % Stop on first failure
        Results = [Result]
    ).

% Integration with existing transaction system

% Enhanced execute/2 that works with sessions - backwards compatible
execute(transaction(Commands), Result) :-
    (active_session(SessionId, _, _) ->
        % Execute within active session
        execute_transaction(SessionId, Commands, Result)
    ;
        % Legacy direct execution without session
        execute_commands(Commands, Results),
        (member(error(E), Results) ->
            Result = error(E)
        ;
            Result = ok(Results)
        )
    ).

% Advanced session features

% Switch between sessions
switch_session(TargetSessionId, Result) :-
    % Check if target session exists
    active_session(TargetSessionId, TargetBranch, _),
    !,

    % Switch to target session branch
    run(command(git(checkout([TargetBranch]))), SwitchResult),
    (SwitchResult = ok(_) ->
        Result = ok(switched_to_session(TargetSessionId, TargetBranch))
    ;
        Result = error(failed_to_switch(SwitchResult))
    ).

switch_session(TargetSessionId, Result) :-
    % Session doesn't exist
    Result = error(session_not_found(TargetSessionId)).

% Suspend current session (switch back to main)
suspend_session(SessionId, Result) :-
    active_session(SessionId, BranchName, _StartCommit),
    run(command(git(checkout(['main']))), SwitchResult),
    (SwitchResult = ok(_) ->
        Result = ok(session_suspended(SessionId, BranchName))
    ;
        Result = error(suspend_failed(SwitchResult))
    ).

% Resume session (switch back to session branch)
resume_session(SessionId, Result) :-
    active_session(SessionId, BranchName, _),
    run(command(git(checkout([BranchName]))), SwitchResult),
    (SwitchResult = ok(_) ->
        Result = ok(session_resumed(SessionId, BranchName))
    ;
        Result = error(resume_failed(SwitchResult))
    ).

% Session templates for common workflows
start_feature_session(FeatureName, SessionId, Result) :-
    uuid(SessionId),
    start_session(SessionId, StartResult),
    (StartResult = ok(_) ->
        Result = ok(feature_session_started(SessionId, FeatureName))
    ;
        Result = error(StartResult)
    ).

start_experiment_session(ExperimentName, SessionId, Result) :-
    uuid(SessionId),
    start_session(SessionId, StartResult),
    (StartResult = ok(_) ->
        Result = ok(experiment_session_started(SessionId, ExperimentName))
    ;
        Result = error(StartResult)
    ).

% Create session snapshot for safe experimentation
create_session_snapshot(SessionId, SnapshotName, Result) :-
    active_session(SessionId, SessionBranch, _),
    format(string(SnapshotBranch), "~w-snapshot-~w", [SessionBranch, SnapshotName]),

    % Create snapshot branch from current session
    run(command(git(checkout(['-b', SnapshotBranch]))), SnapshotResult),
    (SnapshotResult = ok(_) ->
        % Switch back to original session branch
        run(command(git(checkout([SessionBranch]))), _),
        Result = ok(snapshot_created(SnapshotBranch))
    ;
        Result = error(snapshot_failed(SnapshotResult))
    ).

% Compare sessions
compare_sessions(SessionId1, SessionId2, Result) :-
    active_session(SessionId1, Branch1, _),
    active_session(SessionId2, Branch2, _),
    run(command(git(diff([Branch1, Branch2]))), DiffResult),
    (DiffResult = ok(Diff) ->
        Result = ok(session_diff(SessionId1, SessionId2, Diff))
    ;
        Result = error(diff_failed(DiffResult))
    ).

% Session context for LLM agents
discover_session_context(Context) :-
    (active_session(SessionId, BranchName, StartCommit) ->
        list_session_transactions(SessionId, Transactions),
        length(Transactions, TransactionCount),
        format(string(Context),
            "Active session: ~w on branch ~w~nStart commit: ~w~nTransactions: ~w",
            [SessionId, BranchName, StartCommit, TransactionCount])
    ;
        Context = "No active session - working on main branch"
    ).

% Source location
component(session, source, file("session.pl")).

% Helper predicates for Git operations
string_trim(String, Trimmed) :-
    split_string(String, '', ' \t\n\r', [Trimmed|_]).
