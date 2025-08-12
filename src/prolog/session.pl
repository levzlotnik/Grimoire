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



docstring(session, S) :-
    findall(SubCmd, component(session, subcommand, SubCmd), SubCommands),
    maplist(format_session_subcommand, SubCommands, SubCmdDocs),
    atomic_list_concat(SubCmdDocs, '\n    - ', SubCmdList),
    S = {|string(SubCmdList)||
    A session represents a logical work context that creates a Git branch for isolation.
    Each session automatically creates its own branch, enabling concurrent work streams without conflicts.

    Session Lifecycle:
    1. session(start) - Create session (always creates new branch)
    2. session(execute(SessionId, Commands)) - Execute atomic operations within session
    3. session(close(SessionId, Strategy)) - Merge, abandon, or keep session branch

    Available Commands:
    - {SubCmdList}
    |}.

docstring(command(session(start)),
    "Start a new session with auto-generated UUID. Creates new Git branch automatically."
).

docstring(command(session(start)),
    "Start a new session with specific ID. Format: session(start(my_session_id))"
).

docstring(command(session(close)),
    "Close a session with specified strategy: merge_to_main, abandon, or keep_branch"
).

docstring(command(session(execute)),
    "Execute a transaction (list of commands) within the specified session"
).

docstring(command(session(kill)),
    "Safely kill an inactive session. Cannot kill the currently active session."
).

docstring(command(session(kill_all_inactive)),
    "Kill all sessions except the currently active one. Safe cleanup operation."
).

docstring(command(session(abandon_current)),
    "Abandon the current session safely. Stashes changes and returns to main branch."
).

docstring(command(session(emergency_cleanup)),
    "⚠️ EMERGENCY ONLY: Destroy all sessions. Attempts to preserve uncommitted changes."
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

docstring(kill_session,
    {|string(_)||
    Safely kills a specific session that is NOT currently active.
    Format: kill_session(SessionId, Result)

    Safety features:
    - Refuses to kill the current session
    - Verifies session exists before killing
    - Cleans up all tracking data
    - Only operates on session branches

    Returns:
    - ok(session_killed(SessionId, BranchName)) on success
    - error(cannot_kill_current_session(SessionId, CurrentBranch)) if trying to kill current session
    - error(session_not_found(SessionId)) if session doesn't exist
    |}
).

docstring(kill_all_inactive_sessions,
    {|string(_)||
    Kills all sessions except the currently active one.
    Format: kill_all_inactive_sessions(Result)

    Safety features:
    - Current session is always preserved
    - Batch operation with individual result tracking
    - Safe to run from any session or main branch

    Use case: Cleanup after testing or development sessions
    |}
).

docstring(emergency_session_cleanup,
    {|string(_)||
    EMERGENCY ONLY: Destroys all sessions and attempts to restore clean state.
    Format: emergency_session_cleanup(Result)

    ⚠️  WARNING: Use only when session system is broken!

    Safety features:
    - Stashes uncommitted changes before cleanup
    - Switches to main branch for safety
    - Attempts to restore original branch (if not session)
    - Attempts to restore stashed changes

    Use case: Recovery from corrupted session state
    |}
).

docstring(abandon_current_session_safely,
    {|string(_)||
    Safely abandons the current session and returns to main.
    Format: abandon_current_session_safely(Result)

    Safety features:
    - Only works if currently in a session
    - Stashes any uncommitted changes
    - Switches to main before deletion
    - Full cleanup of session tracking

    Use case: Quick exit from experimental session
    |}
).

% Helper to format session subcommands for docstring
format_session_subcommand(start, "session(start) - Start new session with auto-generated ID").
format_session_subcommand(close, "session(close(SessionId, Strategy)) - Close session with strategy").
format_session_subcommand(execute, "session(execute(SessionId, Commands)) - Execute transaction in session").
format_session_subcommand(kill, "session(kill(SessionId)) - Safely kill inactive session").
format_session_subcommand(kill_all_inactive, "session(kill_all_inactive) - Kill all sessions except current").
format_session_subcommand(emergency_cleanup, "session(emergency_cleanup) - Emergency cleanup (⚠️ DANGEROUS)").
format_session_subcommand(abandon_current, "session(abandon_current) - Abandon current session safely").
format_session_subcommand(switch, "session(switch(SessionId)) - Switch to different session").
format_session_subcommand(suspend, "session(suspend(SessionId)) - Suspend session (return to main)").
format_session_subcommand(resume, "session(resume(SessionId)) - Resume suspended session").
format_session_subcommand(list, "session(list) - List all active sessions").
format_session_subcommand(stats, "session(stats) - Get session statistics").
format_session_subcommand(history, "session(history(SessionId)) - Show session Git history").
format_session_subcommand(with_commands, "session(with_commands(Commands)) - Quick workflow: start, execute, close").
format_session_subcommand(workflow, "session(workflow(TransactionsList)) - Multi-transaction workflow").
format_session_subcommand(feature, "session(feature(Name)) - Start feature development session").
format_session_subcommand(experiment, "session(experiment(Name)) - Start experimental session").

% Utility predicates

% Generate session branch name from session ID
session_branch_name(SessionId, BranchName) :-
    format(atom(BranchName), "session-~w", [SessionId]).

% Generate transition branch name
transition_branch_name(SourceBranch, SessionId, TransitionBranch) :-
    format(atom(TransitionBranch), "transition_branch/~w--session-~w", [SourceBranch, SessionId]).

% Session management predicates

% Start a new session with transition branch support
start_session(SessionId, Result) :-
    uuid(SessionId),
    start_session_with_transition(SessionId, Result).

% Core session creation with transition branch handling
start_session_with_transition(SessionId, Result) :-
    get_current_branch(SourceBranch),
    check_tracked_changes(StatusResult),

    (StatusResult = clean ->
        % Clean state - direct session creation
        direct_session_creation(SessionId, SourceBranch, Result)
    ;
        % Dirty state - use transition branch
        transition_based_session_creation(SessionId, SourceBranch, Result)
    ).

% Direct session creation for clean state
direct_session_creation(SessionId, SourceBranch, Result) :-
    get_current_commit(StartCommit),
    format(string(SessionBranch), "session-~w", [SessionId]),
    run(command(git(checkout(['-b', SessionBranch]))), CheckoutResult),

    (CheckoutResult = ok(_) ->
        format("Session ~w started on branch: ~w (direct)~n", [SessionId, SessionBranch]),
        Result = ok(session_started(SessionId, SessionBranch, StartCommit, direct))
    ;
        Result = error(failed_to_create_session_branch(CheckoutResult))
    ).

% Transition-based session creation for dirty state
transition_based_session_creation(SessionId, SourceBranch, Result) :-
    create_transition_for_new_session(SourceBranch, SessionId, TransitionResult),

    (TransitionResult = ok(transition_created(TransitionBranch, CommitHash)) ->
        % Create session branch from transition
        format(string(SessionBranch), "session-~w", [SessionId]),
        run(command(git(checkout(['-b', SessionBranch]))), CheckoutResult),

        (CheckoutResult = ok(_) ->
            format("Session ~w started on branch: ~w (via transition ~w)~n",
                   [SessionId, SessionBranch, TransitionBranch]),
            Result = ok(session_started(SessionId, SessionBranch, CommitHash, via_transition(TransitionBranch)))
        ;
            % Session creation failed - transition branch remains as recovery point
            Result = error(failed_to_create_session_from_transition(CheckoutResult, TransitionBranch))
        )
    ;
        % Transition creation failed
        Result = error(transition_creation_failed(TransitionResult))
    ).

% Create transition branch for new session with tracked changes
create_transition_for_new_session(SourceBranch, SessionId, Result) :-
    % Generate transition branch name
    format(string(TransitionBranch), "transition_branch/~w--session-~w", [SourceBranch, SessionId]),

    % Clean up any existing transition branch with same name (defensive)
    catch(run(command(git(branch(['-D', TransitionBranch]))), _), _, true),

    % Create transition branch from source
    run(command(git(checkout(['-b', TransitionBranch]))), CreateResult),

    (CreateResult = ok(_) ->
        % Stage and commit tracked changes only
        commit_tracked_changes_only(TransitionBranch, SessionId, CommitResult),

        (CommitResult = ok(CommitHash) ->
            Result = ok(transition_created(TransitionBranch, CommitHash))
        ;
            % Commit failed - clean up transition branch
            run(command(git(checkout([SourceBranch]))), _),
            run(command(git(branch(['-D', TransitionBranch]))), _),
            Result = error(failed_to_commit_transition(CommitResult))
        )
    ;
        Result = error(failed_to_create_transition_branch(CreateResult))
    ).

% Commit only tracked changes to transition branch
commit_tracked_changes_only(TransitionBranch, SessionId, Result) :-
    % Stage tracked files with changes (not untracked files)
    run(command(git(add(['-u']))), AddResult),

    (AddResult = ok(_) ->
        % Check if there are staged changes
        run(command(git(diff(['--cached', '--quiet']))), DiffResult),

        (DiffResult = error(_) ->
            % There are staged changes - commit them
            format(string(CommitMsg), "Transition: tracked changes for session-~w", [SessionId]),
            run(command(git(commit(['-m', CommitMsg]))), CommitResult),

            (CommitResult = ok(_) ->
                get_current_commit(CommitHash),
                Result = ok(CommitHash)
            ;
                Result = error(commit_failed(CommitResult))
            )
        ;
            % No staged changes - skip transition
            Result = error(no_tracked_changes)
        )
    ;
        Result = error(failed_to_stage_changes(AddResult))
    ).

% Check for tracked unstaged changes
check_tracked_changes(Status) :-
    % Check for tracked files with unstaged changes
    run(command(git(diff(['--quiet']))), DiffResult),

    (DiffResult = error(_) ->
        % There are unstaged changes in tracked files
        Status = dirty
    ;
        % No unstaged changes in tracked files
        Status = clean
    ).

% Close a session with transition cleanup
close_session(SessionId, CloseStrategy, Result) :-
    % Use new session branch naming
    format(string(SessionBranch), "session-~w", [SessionId]),

    (CloseStrategy = merge_to_main ->
        merge_session_to_main_with_transition_cleanup(SessionId, SessionBranch, Result)
    ; CloseStrategy = abandon ->
        abandon_session_with_transition_cleanup(SessionId, SessionBranch, Result)
    ; CloseStrategy = keep_branch ->
        Result = ok(branch_kept(SessionBranch))
    ),

    format("Session ~w closed: ~w~n", [SessionId, Result]).

% Merge session branch to main and cleanup transition
merge_session_to_main_with_transition_cleanup(SessionId, SessionBranch, Result) :-
    run(command(git(checkout(['main']))), _),
    run(command(git(merge(['--no-ff', SessionBranch]))), MergeResult),

    (MergeResult = ok(_) ->
        % Delete session branch
        run(command(git(branch(['-d', SessionBranch]))), _),

        % Delete associated transition branch
        delete_transition_on_merge(SessionId, _),

        Result = ok(merged_and_deleted(SessionBranch))
    ;
        Result = error(merge_failed(MergeResult))
    ).

% Abandon session and cleanup transition
abandon_session_with_transition_cleanup(SessionId, SessionBranch, Result) :-
    run(command(git(checkout(['main']))), _),
    run(command(git(branch(['-D', SessionBranch]))), DeleteResult),

    (DeleteResult = ok(_) ->
        % Delete associated transition branch
        delete_transition_on_merge(SessionId, _),

        Result = ok(abandoned(SessionBranch))
    ;
        Result = error(delete_failed(DeleteResult))
    ).

% Delete transition branch when session is merged or abandoned
delete_transition_on_merge(SessionId, Result) :-
    % Find transition branch for this session
    find_transition_for_session(SessionId, TransitionBranch),

    (TransitionBranch \= none ->
        run(command(git(branch(['-D', TransitionBranch]))), DeleteResult),
        (DeleteResult = ok(_) ->
            Result = ok(transition_deleted(TransitionBranch))
        ;
            Result = error(failed_to_delete_transition(DeleteResult))
        )
    ;
        % No transition branch found (direct session creation)
        Result = ok(no_transition_to_delete)
    ).

% Find transition branch for a session
find_transition_for_session(SessionId, TransitionBranch) :-
    % Look for transition branch matching pattern
    run(command(git(branch(['--format=%(refname:short)']))), BranchResult),

    (BranchResult = ok(result(BranchOutput, _)) ->
        split_string(BranchOutput, '\n', '\n \t', BranchLines),
        format(string(Pattern), "--session-~w", [SessionId]),

        (member(Branch, BranchLines),
         sub_string(Branch, _, _, _, Pattern) ->
            TransitionBranch = Branch
        ;
            TransitionBranch = none
        )
    ;
        TransitionBranch = none
    ).

% List all transition branches
list_transition_branches(TransitionBranches) :-
    run(command(git(branch(['--format=%(refname:short)']))), BranchResult),
    (BranchResult = ok(result(BranchOutput, _)) ->
        split_string(BranchOutput, '\n', '\n \t', BranchLines),
        include(is_transition_branch, BranchLines, TransitionBranches)
    ;
        TransitionBranches = []
    ).

% Helper: check if branch name is a transition branch
is_transition_branch(BranchName) :-
    sub_string(BranchName, 0, _, _, 'transition_branch/').

% Cleanup orphaned transition branches (for sessions that no longer exist)
cleanup_orphaned_transitions(Result) :-
    list_transition_branches(TransitionBranches),
    include(is_orphaned_transition, TransitionBranches, OrphanedBranches),
    maplist(delete_transition_branch, OrphanedBranches, DeleteResults),
    Result = ok(cleaned_up_transitions(OrphanedBranches, DeleteResults)).

% Helper: check if transition branch is orphaned (session doesn't exist)
is_orphaned_transition(TransitionBranch) :-
    % Extract session ID from transition branch name using string operations
    sub_string(TransitionBranch, _, _, 0, Suffix),
    sub_string(Suffix, StartPos, _, 0, SessionPart),
    sub_string(SessionPart, 0, 8, _, 'session-'),
    sub_string(SessionPart, 8, _, 0, SessionId),
    % Check if session branch exists
    \+ session_exists(SessionId).

% Helper: delete a transition branch
delete_transition_branch(TransitionBranch, Result) :-
    run(command(git(branch(['-D', TransitionBranch]))), DeleteResult),
    (DeleteResult = ok(_) ->
        Result = ok(deleted(TransitionBranch))
    ;
        Result = error(failed_to_delete(TransitionBranch, DeleteResult))
    ).

% Transaction execution within sessions

% Execute transaction in session context
execute_transaction(SessionId, Commands, Result) :-
    % Verify session exists by checking for session branch
    format(string(BranchName), "session-~w", [SessionId]),
    run(command(git(rev_parse(['--verify', BranchName]))), VerifyResult),
    (VerifyResult = ok(_) ->
        % Switch to session branch
        run(command(git(checkout([BranchName]))), _),

        % Execute commands with Git-based transaction
        execute_commands_atomically(Commands, ExecuteResult),

        (ExecuteResult = ok(Results) ->
            % Commit transaction - this IS the transaction record
            commit_transaction(SessionId, Commands, CommitResult),
            (CommitResult = ok(CommitHash) ->
                Result = ok(transaction_committed(CommitHash, Results))
            ;
                % Commit failed - rollback already happened in execute_commands_atomically
                Result = error(commit_failed(CommitResult))
            )
        ;
            % Commands failed - rollback already happened in execute_commands_atomically
            Result = error(commands_failed(ExecuteResult))
        )
    ;
        Result = error(session_not_found(SessionId))
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

% Commit transaction with Git commit as transaction record
commit_transaction(SessionId, Commands, Result) :-
    % Find and add files in src directory
    find_src_files(SrcFiles),
    add_src_files(SrcFiles, AddResult),

    (AddResult = ok(_) ->
        % Create structured commit message with command summary
        summarize_commands(Commands, CommandSummary),
        format(string(CommitMsg), "Session ~w: ~w", [SessionId, CommandSummary]),

        % Commit with descriptive message - this creates the transaction record
        run(command(git(commit(['-m', CommitMsg]))), CommitResult),
        (CommitResult = ok(_) ->
            get_current_commit(CommitHash),
            Result = ok(CommitHash)
        ;
            Result = error(CommitResult)
        )
    ;
        Result = error(AddResult)
    ).

% No rollback tracking needed - Git reset --hard is the rollback

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
    run(command(git(rev_parse(['HEAD']))), ok(result(CommitOutput, _))),
    string_trim(CommitOutput, Commit).

% Get current Git branch
get_current_branch(Branch) :-
    run(command(git(branch(['--show-current']))), ok(result(BranchOutput, _))),
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

% List active sessions (inferred from git branches)
list_active_sessions(Sessions) :-
    list_session_branches(SessionBranches),
    maplist(session_branch_to_info, SessionBranches, Sessions).

% Convert session branch info to full session information
session_branch_to_info(session(SessionId, BranchName), session(SessionId, BranchName, unknown_start)) :-
    % We could enhance this to get start commit from git log if needed
    true.

% List transactions for a session (Git-inferred from commits)
list_session_transactions(SessionId, Transactions) :-
    format(string(BranchName), "session-~w", [SessionId]),
    run(command(git(rev_parse(['--verify', BranchName]))), VerifyResult),
    (VerifyResult = ok(_) ->
        % Get commits on session branch that aren't on main
        run(command(git(log(['--format=%H|%s', '--reverse', BranchName, '^main']))), LogResult),
        (LogResult = ok(result(LogOutput, _)) ->
            split_string(LogOutput, '\n', '\n \t', CommitLines),
            findall(transaction(CommitHash, CommitMsg, committed),
                    (member(Line, CommitLines),
                     Line \= '',
                     split_string(Line, '|', '', [CommitHash, CommitMsg])),
                    Transactions)
        ;
            Transactions = []
        )
    ;
        Transactions = []
    ).

% Show session history (Git log for session branch)
show_session_history(SessionId) :-
    format(string(BranchName), "session-~w", [SessionId]),
    run(command(git(rev_parse(['--verify', BranchName]))), VerifyResult),
    (VerifyResult = ok(_) ->
        run(command(git(log(['--oneline', '--graph', BranchName]))), LogResult),
        (LogResult = ok(History) ->
            format("Session ~w history:~n~w~n", [SessionId, History])
        ;
            format("Failed to get history for session ~w~n", [SessionId])
        )
    ;
        format("Session ~w not found~n", [SessionId])
    ).

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
    (current_session(SessionId) ->
        % Execute within active session (inferred from current branch)
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

% Switch between sessions with transition branch for unstaged changes
switch_session(TargetSessionId, Result) :-
    switch_session_with_transition(TargetSessionId, Result).

% Safe session switching with transition branches for dirty state
switch_session_with_transition(TargetSessionId, Result) :-
    % Check if target session exists
    format(string(TargetBranch), "session-~w", [TargetSessionId]),
    run(command(git(rev_parse(['--verify', TargetBranch]))), VerifyResult),
    (VerifyResult = ok(_) ->
        % Get current state
        get_current_branch(CurrentBranch),
        check_git_status(StatusResult),

        (StatusResult = clean ->
            % No changes - direct switch
            run(command(git(checkout([TargetBranch]))), SwitchResult),
            (SwitchResult = ok(_) ->
                Result = ok(switched_to_session(TargetSessionId, TargetBranch, direct))
            ;
                Result = error(failed_to_switch(SwitchResult))
            )
        ;
            % Has changes - use transition branch
            create_transition_branch(CurrentBranch, TargetSessionId, TransitionResult),
            (TransitionResult = ok(transition_created(TransitionBranch)) ->
                % Now switch to target session
                run(command(git(checkout([TargetBranch]))), SwitchResult),
                (SwitchResult = ok(_) ->
                    Result = ok(switched_to_session(TargetSessionId, TargetBranch, via_transition(TransitionBranch)))
                ;
                    Result = error(failed_to_switch_after_transition(SwitchResult, TransitionBranch))
                )
            ;
                Result = error(failed_to_create_transition(TransitionResult))
            )
        )
    ;
        Result = error(session_not_found(TargetSessionId))
    ).

% Create transition branch for current state before switching
create_transition_branch(SourceBranch, TargetSessionId, Result) :-
    % Generate transition branch name: transition_branch/source--target
    format(string(TransitionBranch), "transition_branch/~w--~w", [SourceBranch, TargetSessionId]),

    % Create and switch to transition branch
    run(command(git(checkout(['-b', TransitionBranch]))), CreateResult),
    (CreateResult = ok(_) ->
        % Stage all changes (including untracked files)
        run(command(git(add(['.']))), AddResult),
        (AddResult = ok(_) ->
            % Commit with descriptive message
            format(string(CommitMsg), "Transition: ~w -> session-~w", [SourceBranch, TargetSessionId]),
            run(command(git(commit(['-m', CommitMsg]))), CommitResult),
            (CommitResult = ok(_) ->
                Result = ok(transition_created(TransitionBranch))
            ;
                Result = error(failed_to_commit_transition(CommitResult))
            )
        ;
            Result = error(failed_to_stage_changes(AddResult))
        )
    ;
        Result = error(failed_to_create_transition_branch(CreateResult))
    ).

% Check git status - clean or dirty
check_git_status(Status) :-
    run(command(git(status(['--porcelain']))), StatusResult),
    (StatusResult = ok(result("", _)) ->
        Status = clean
    ;
        Status = dirty
    ).

% Check for tracked unstaged changes specifically
check_tracked_changes(Status) :-
    run(command(git(status(['--porcelain']))), StatusResult),
    (StatusResult = ok(result(Output, _)) ->
        % Parse porcelain output for tracked changes (modifications)
        (split_string(Output, '\n', '\n \t', Lines),
         include(is_tracked_change, Lines, TrackedChanges),
         (TrackedChanges = [] ->
             Status = clean
         ;
             Status = dirty(tracked_changes(TrackedChanges))
         ))
    ;
        Status = error(git_status_failed)
    ).

% Helper: identify tracked change lines (M, A, D, R, C at position 1 or 2)
is_tracked_change(Line) :-
    atom_length(Line, Len),
    Len >= 3,
    sub_atom(Line, 0, 1, _, First),
    sub_atom(Line, 1, 1, _, Second),
    (member(First, [' ', 'M', 'A', 'D', 'R', 'C']) ; member(Second, ['M', 'A', 'D', 'R', 'C'])),
    % Exclude untracked files (starts with ??)
    \+ sub_atom(Line, 0, 2, _, '??').

% Simple predicate to check if we have dirty tracked files
has_dirty_tracked_files :-
    check_tracked_changes(Status),
    Status = dirty(_).

% Decide whether to use transition branch for new session creation
should_use_transition(SessionId) :-
    % Only use transitions for new sessions with dirty state
    \+ session_exists(SessionId),
    has_dirty_tracked_files.

% Check if a session branch exists
session_exists(SessionId) :-
    session_branch_name(SessionId, BranchName),
    run(command(git(branch(['--list', BranchName]))), Result),
    Result = ok(result(Output, _)),
    Output \= "".

% Suspend current session (switch back to main)
suspend_session(SessionId, Result) :-
    % Verify we're in the correct session
    (current_session(CurrentSessionId) ->
        (CurrentSessionId = SessionId ->
            get_current_branch(BranchName),
            run(command(git(checkout(['main']))), SwitchResult),
            (SwitchResult = ok(_) ->
                Result = ok(session_suspended(SessionId, BranchName))
            ;
                Result = error(suspend_failed(SwitchResult))
            )
        ;
            Result = error(not_in_specified_session(SessionId, CurrentSessionId))
        )
    ;
        Result = error(not_in_session)
    ).

% Resume session (switch back to session branch)
resume_session(SessionId, Result) :-
    format(string(BranchName), "session-~w", [SessionId]),
    run(command(git(rev_parse(['--verify', BranchName]))), VerifyResult),
    (VerifyResult = ok(_) ->
        run(command(git(checkout([BranchName]))), SwitchResult),
        (SwitchResult = ok(_) ->
            Result = ok(session_resumed(SessionId, BranchName))
        ;
            Result = error(resume_failed(SwitchResult))
        )
    ;
        Result = error(session_not_found(SessionId))
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

% Session context for LLM agents
discover_session_context(Context) :-
    (current_session(SessionId) ->
        get_current_branch(BranchName),
        list_session_transactions(SessionId, Transactions),
        length(Transactions, TransactionCount),
        format(string(Context),
            "Active session: ~w on branch ~w~nTransactions: ~w",
            [SessionId, BranchName, TransactionCount])
    ;
        get_current_branch(BranchName),
        format(string(Context), "No active session - working on branch ~w", [BranchName])
    ).

% Source location
component(session, source, file("session.pl")).

% Helper predicates for Git operations
string_trim(String, Trimmed) :-
    split_string(String, '', ' \t\n\r', [Trimmed|_]).

% Safe session killing predicates

% Kill a specific session safely (not the current one)
kill_session(SessionId, Result) :-
    % Safety check: Don't kill current session
    get_current_branch(CurrentBranch),
    format(string(SessionBranch), "session-~w", [SessionId]),

    (CurrentBranch = SessionBranch ->
        Result = error(cannot_kill_current_session(SessionId, CurrentBranch))
    ;
        % Check if session branch exists
        run(command(git(rev_parse(['--verify', SessionBranch]))), VerifyResult),
        (VerifyResult = ok(_) ->
            % Session exists and is not current - safe to kill
            kill_session_branch(SessionBranch, KillResult),
            (KillResult = ok(_) ->
                Result = ok(session_killed(SessionId, SessionBranch))
            ;
                Result = error(kill_failed(KillResult))
            )
        ;
            Result = error(session_not_found(SessionId))
        )
    ).

% Kill session branch (low-level operation)
kill_session_branch(BranchName, Result) :-
    % Verify it's a session branch (safety check)
    (is_session_branch(BranchName) ->
        run(command(git(branch(['-D', BranchName]))), DeleteResult),
        (DeleteResult = ok(_) ->
            Result = ok(branch_deleted(BranchName))
        ;
            Result = error(delete_failed(DeleteResult))
        )
    ;
        Result = error(not_a_session_branch(BranchName))
    ).

% Kill all inactive sessions (current session is safe)
kill_all_inactive_sessions(Result) :-
    get_current_branch(CurrentBranch),
    list_session_branches(SessionBranches),
    findall(SessionId, member(session(SessionId, _), SessionBranches), AllSessions),
    kill_inactive_sessions(AllSessions, CurrentBranch, [], Results),
    Result = ok(killed_sessions(Results)).

kill_inactive_sessions([], _, Acc, Acc).
kill_inactive_sessions([SessionId|Rest], CurrentBranch, Acc, Results) :-
    format(string(SessionBranch), "session-~w", [SessionId]),
    (CurrentBranch = SessionBranch ->
        % Skip current session
        kill_inactive_sessions(Rest, CurrentBranch, [skipped(SessionId, current)|Acc], Results)
    ;
        % Kill this session
        kill_session(SessionId, KillResult),
        kill_inactive_sessions(Rest, CurrentBranch, [KillResult|Acc], Results)
    ).

% Emergency session cleanup (use with extreme caution)
emergency_session_cleanup(Result) :-
    % Get current state for restoration
    get_current_branch(CurrentBranch),

    % Check if we have uncommitted changes
    run(command(git(status(['--porcelain']))), StatusResult),
    (StatusResult = ok("") ->
        HasChanges = false
    ;
        HasChanges = true
    ),

    % Stash changes if any
    (HasChanges = true ->
        run(command(git(stash(['push', '-m', 'emergency_session_cleanup']))), StashResult),
        (StashResult = ok(_) ->
            CanRestore = true
        ;
            CanRestore = false,
            Result = error(failed_to_stash_changes(StashResult))
        )
    ;
        CanRestore = false
    ),

    % Switch to main for safety
    run(command(git(checkout(['main']))), CheckoutResult),
    (CheckoutResult = ok(_) ->
        % Delete all session branches
        cleanup_all_session_branches(CleanupResult),

        % Restore original state if possible
        (CurrentBranch \= 'main' ->
            (is_session_branch(CurrentBranch) ->
                % Was in a session - can't restore, stay on main
                RestoreResult = could_not_restore_session_branch(CurrentBranch)
            ;
                % Was on another branch - try to restore
                run(command(git(checkout([CurrentBranch]))), RestoreCheckout),
                RestoreResult = RestoreCheckout
            )
        ;
            RestoreResult = already_on_main
        ),

        % Restore stashed changes if any
        (CanRestore = true ->
            run(command(git(stash(['pop']))), PopResult),
            FinalRestore = PopResult
        ;
            FinalRestore = no_changes_to_restore
        ),

        Result = ok(emergency_cleanup_complete(CleanupResult, RestoreResult, FinalRestore))
    ;
        Result = error(failed_to_switch_to_main(CheckoutResult))
    ).

% Clean up all session branches (low-level)
cleanup_all_session_branches(Result) :-
    % Get all branches and filter session branches
    run(command(git(branch(['--format=%(refname:short)']))), BranchResult),
    (BranchResult = ok(result(BranchOutput, _)) ->
        split_string(BranchOutput, '\n', ' \t', BranchLines),
        include(is_session_branch, BranchLines, SessionBranches),
        delete_session_branches(SessionBranches, DeleteResults),
        Result = ok(deleted_branches(DeleteResults))
    ;
        Result = error(failed_to_list_branches(BranchResult))
    ).

% Helper to identify session branches
is_session_branch(Branch) :-
    sub_string(Branch, 0, 8, _, 'session-').

% Delete multiple session branches
delete_session_branches([], []).
delete_session_branches([Branch|Rest], [Result|Results]) :-
    kill_session_branch(Branch, Result),
    delete_session_branches(Rest, Results).

% Safe session abandonment (alternative to killing)
abandon_current_session_safely(Result) :-
    get_current_branch(CurrentBranch),
    (is_session_branch(CurrentBranch) ->
        % Extract session ID from branch name
        parse_session_from_branch(CurrentBranch, SessionId, true),

        % Stash any changes first
        run(command(git(status(['--porcelain']))), StatusResult),
        (StatusResult = ok("") ->
            HasChanges = false
        ;
            HasChanges = true,
            format(string(StashMsg), 'abandoned_session_~w', [SessionId]),
            run(command(git(stash(['push', '-m', StashMsg]))), _)
        ),

        % Switch to main and delete session
        run(command(git(checkout(['main']))), _),
        kill_session_branch(CurrentBranch, KillResult),

        Result = ok(session_abandoned_safely(SessionId, CurrentBranch, HasChanges, KillResult))
    ;
        Result = error(not_in_session(CurrentBranch))
    ).

% List killable sessions (excludes current session)
list_killable_sessions(KillableSessions) :-
    get_current_branch(CurrentBranch),
    list_session_branches(AllSessionBranches),
    findall(
        session(SessionId, BranchName, unknown_start),
        (
            member(session(SessionId, BranchName), AllSessionBranches),
            BranchName \= CurrentBranch
        ),
        KillableSessions
    ).

% Session command namespace
component(command, ctor, session).

% Session subcommands
component(session, subcommand, start).
component(session, subcommand, close).
component(session, subcommand, execute).
component(session, subcommand, kill).
component(session, subcommand, kill_all_inactive).
component(session, subcommand, emergency_cleanup).
component(session, subcommand, abandon_current).
component(session, subcommand, switch).
component(session, subcommand, suspend).
component(session, subcommand, resume).
component(session, subcommand, list).
component(session, subcommand, stats).
component(session, subcommand, history).
component(session, subcommand, with_commands).
component(session, subcommand, workflow).
component(session, subcommand, feature).
component(session, subcommand, experiment).

% Main session command handler
run(command(session(SubCommand)), RetVal) :-
    run_session_command(SubCommand, RetVal).

% Session command implementations
run_session_command(start(SessionId), RetVal) :-
    start_session(SessionId, RetVal).

run_session_command(start, RetVal) :-
    % Auto-generate session ID
    start_session(SessionId, StartResult),
    RetVal = session_started(SessionId, StartResult).

run_session_command(close(SessionId, Strategy), RetVal) :-
    close_session(SessionId, Strategy, RetVal).

run_session_command(execute(SessionId, Commands), RetVal) :-
    execute_transaction(SessionId, Commands, RetVal).

run_session_command(kill(SessionId), RetVal) :-
    kill_session(SessionId, RetVal).

run_session_command(kill_all_inactive, RetVal) :-
    kill_all_inactive_sessions(RetVal).

run_session_command(emergency_cleanup, RetVal) :-
    emergency_session_cleanup(RetVal).

run_session_command(abandon_current, RetVal) :-
    abandon_current_session_safely(RetVal).

run_session_command(switch(SessionId), RetVal) :-
    switch_session(SessionId, RetVal).

run_session_command(suspend(SessionId), RetVal) :-
    suspend_session(SessionId, RetVal).

run_session_command(resume(SessionId), RetVal) :-
    resume_session(SessionId, RetVal).

run_session_command(list, RetVal) :-
    list_active_sessions(Sessions),
    RetVal = ok(active_sessions(Sessions)).

run_session_command(stats, RetVal) :-
    session_stats(Stats),
    RetVal = ok(Stats).

run_session_command(history(SessionId), RetVal) :-
    show_session_history(SessionId),
    RetVal = ok(history_displayed).

run_session_command(with_commands(Commands), RetVal) :-
    % Quick workflow: start session, execute commands, close with merge
    with_session(Commands, RetVal).

run_session_command(workflow(TransactionsList), RetVal) :-
    % Multi-transaction workflow
    start_session(SessionId, _),
    session_workflow(SessionId, TransactionsList, RetVal).

run_session_command(feature(Name), RetVal) :-
    % Start a feature development session
    start_feature_session(Name, SessionId, RetVal).

run_session_command(feature(Name, Commands), RetVal) :-
    % Start feature session and execute commands
    start_feature_session(Name, SessionId, StartResult),
    (StartResult = ok(_) ->
        execute_transaction(SessionId, Commands, ExecResult),
        RetVal = feature_session_executed(SessionId, StartResult, ExecResult)
    ;
        RetVal = error(feature_start_failed(StartResult))
    ).

run_session_command(experiment(Name), RetVal) :-
    % Start an experimental session
    start_experiment_session(Name, SessionId, RetVal).

run_session_command(experiment(Name, Commands), RetVal) :-
    % Start experiment session and execute commands
    start_experiment_session(Name, SessionId, StartResult),
    (StartResult = ok(_) ->
        execute_transaction(SessionId, Commands, ExecResult),
        RetVal = experiment_session_executed(SessionId, StartResult, ExecResult)
    ;
        RetVal = error(experiment_start_failed(StartResult))
    ).

% Session introspection and safety utilities

% Check if safe to perform dangerous operations
is_safe_state(SafetyCheck) :-
    get_current_branch(CurrentBranch),
    run(command(git(status(['--porcelain']))), StatusResult),

    % Check for uncommitted changes
    (StatusResult = ok("") ->
        HasChanges = false
    ;
        HasChanges = true
    ),

    % Check if in session
    (in_session(_) ->
        InSession = true
    ;
        InSession = false
    ),

    SafetyCheck = safety_state(CurrentBranch, HasChanges, InSession).

% Get session statistics
session_stats(Stats) :-
    list_session_branches(SessionBranches),
    length(SessionBranches, SessionCount),

    % Count total transactions across all sessions (Git-inferred)
    findall(Transaction,
            (member(session(SessionId, _), SessionBranches),
             list_session_transactions(SessionId, SessionTransactions),
             member(Transaction, SessionTransactions)),
            AllTransactions),
    length(AllTransactions, TransactionCount),

    get_current_branch(CurrentBranch),
    (current_session(CurrentSessionId) ->
        CurrentSession = session(CurrentSessionId)
    ;
        CurrentSession = not_in_session
    ),

    Stats = session_statistics(
        total_sessions(SessionCount),
        total_transactions(TransactionCount),
        current_branch(CurrentBranch),
        current_session(CurrentSession)
    ).

% Git branch utilities for session state inference
% ================================================

docstring(get_current_branch,
    {|string(_)||
    Get the current git branch name.
    Format: get_current_branch(BranchName)
    Arguments:
      BranchName: Current git branch name
    Result: true if successful, false otherwise
    ||}).

get_current_branch(BranchName) :-
    run(command(git(rev_parse(['--abbrev-ref', 'HEAD']))), ok(result(Output, _))),
    string_trim(Output, BranchName).

docstring(parse_session_from_branch,
    {|string(_)||
    Parse session information from a branch name.
    Session branches follow the pattern: session-{session_id}
    Format: parse_session_from_branch(BranchName, SessionId, IsSessionBranch)
    Arguments:
      BranchName: Git branch name to parse
      SessionId: Extracted session ID (if session branch)
      IsSessionBranch: true if this is a session branch, false otherwise
    ||}).

parse_session_from_branch(BranchName, SessionId, true) :-
    atom_string(BranchName, BranchStr),
    string_concat("session-", SessionId, BranchStr), !.
parse_session_from_branch(_, _, false).

docstring(is_session_branch,
    {|string(_)||
    Check if a branch name represents a session branch.
    Format: is_session_branch(BranchName)
    Arguments:
      BranchName: Git branch name to check
    Result: true if this is a session branch
    ||}).

is_session_branch(BranchName) :-
    parse_session_from_branch(BranchName, _, true).

docstring(list_session_branches,
    {|string(_)||
    List all session branches in the repository.
    Format: list_session_branches(SessionBranches)
    Arguments:
      SessionBranches: List of session(SessionId, BranchName) terms
    ||}).

list_session_branches(SessionBranches) :-
    run(command(git(branch(['--format=%(refname:short)']))), ok(result(Output, _))),
    split_string(Output, '\n', '\n \t', BranchLines),
    findall(session(SessionId, BranchName),
            (member(BranchName, BranchLines),
             BranchName \= '',
             parse_session_from_branch(BranchName, SessionId, true)),
            SessionBranches).

docstring(current_session,
    {|string(_)||
    Get the current session ID if we're in a session branch.
    Format: current_session(SessionId)
    Arguments:
      SessionId: Current session ID (if in session)
    Result: true if currently in a session, false otherwise
    ||}).

current_session(SessionId) :-
    get_current_branch(BranchName),
    parse_session_from_branch(BranchName, SessionId, true).

docstring(in_session,
    {|string(_)||
    Check if we're currently in a session branch.
    Format: in_session
    Result: true if currently in a session branch
    ||}).

in_session :-
    current_session(_).

% Session state inference utilities
% =================================
