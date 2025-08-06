% Session and Transaction System Tests
:- use_module(library(plunit)).
:- use_module(library(uuid)).

% Load the session system
:- ensure_loaded("../session.pl").

% Test suite for session management
:- begin_tests(session_management).

setup :-
    % Clean up any existing sessions
    retractall(active_session(_, _, _)),
    retractall(session_transaction(_, _, _, _)),
    % Ensure we're on main branch for tests (ignore failures)
    catch(run(command(git(checkout(['main']))), _), _, true).

teardown :-
    % Clean up test sessions and branches
    cleanup_test_sessions.

% Helper to clean up test sessions
cleanup_test_sessions :-
    % Clean up dynamic facts - safer than trying to parse git output
    retractall(active_session(_, _, _)),
    retractall(session_transaction(_, _, _, _)),
    % Try to switch to main (ignore errors)
    catch(run(command(git(checkout(['main']))), _), _, true).

test(start_session_creates_branch, [setup(setup), cleanup(teardown)]) :-
    % Test starting a session always creates new branch
    start_session(SessionId, Result),
    
    % Verify session was created
    assertion(ground(SessionId)),
    assertion(Result = ok(branch_created(_))),
    
    % Verify session is tracked
    assertion(active_session(SessionId, _, _)),
    
    % Verify we're on the session branch
    get_current_branch(CurrentBranch),
    format(string(ExpectedBranch), "session-~w", [SessionId]),
    assertion(CurrentBranch = ExpectedBranch).

test(execute_transaction_success, [setup(setup), cleanup(teardown)]) :-
    % Start a session
    start_session(SessionId, _),
    
    % Execute a simple transaction
    Commands = [command(mkfile("/tmp/test_session_file.txt"))],
    execute_transaction(SessionId, Commands, Result),
    
    % Verify transaction succeeded
    assertion(Result = ok(transaction_committed(_, _, [ok(_)]))),
    
    % Verify transaction is tracked
    list_session_transactions(SessionId, Transactions),
    assertion(length(Transactions, 1)),
    
    % Verify file was created
    assertion(exists_file("/tmp/test_session_file.txt")),
    
    % Clean up
    delete_file("/tmp/test_session_file.txt").

test(execute_transaction_rollback, [setup(setup), cleanup(teardown)]) :-
    % Start a session
    start_session(SessionId, _),
    
    % Execute a transaction that should fail
    Commands = [
        command(mkfile("/tmp/test_session_file1.txt")),
        command(shell(["false"]))  % This will fail
    ],
    execute_transaction(SessionId, Commands, Result),
    
    % Verify transaction failed
    assertion(Result = error(commands_failed(_))),
    
    % Verify rollback occurred - file should not exist
    assertion(\+ exists_file("/tmp/test_session_file1.txt")).

test(close_session_merge, [setup(setup), cleanup(teardown)]) :-
    % Start a session and make a change
    start_session(SessionId, _),
    execute_transaction(SessionId, [command(mkfile("/tmp/test_merge_file.txt"))], _),
    
    % Close session with merge strategy
    close_session(SessionId, merge_to_main, Result),
    
    % Verify merge succeeded
    assertion(Result = ok(merged_and_deleted(_))),
    
    % Verify we're back on main
    get_current_branch(CurrentBranch),
    assertion(CurrentBranch = "main"),
    
    % Verify session is no longer tracked
    assertion(\+ active_session(SessionId, _, _)),
    
    % Verify file still exists (merged to main)
    assertion(exists_file("/tmp/test_merge_file.txt")),
    
    % Clean up
    delete_file("/tmp/test_merge_file.txt").

test(close_session_abandon, [setup(setup), cleanup(teardown)]) :-
    % Start a session and make a change
    start_session(SessionId, _),
    execute_transaction(SessionId, [command(mkfile("/tmp/test_abandon_file.txt"))], _),
    
    % Close session with abandon strategy
    close_session(SessionId, abandon, Result),
    
    % Verify abandon succeeded
    assertion(Result = ok(abandoned(_))),
    
    % Verify we're back on main
    get_current_branch(CurrentBranch),
    assertion(CurrentBranch = "main"),
    
    % Verify session is no longer tracked
    assertion(\+ active_session(SessionId, _, _)),
    
    % Verify file does not exist (abandoned)
    assertion(\+ exists_file("/tmp/test_abandon_file.txt")).

test(session_workflow_convenience, [setup(setup), cleanup(teardown)]) :-
    % Test the with_session convenience predicate
    Commands = [command(mkfile("/tmp/test_workflow_file.txt"))],
    with_session(Commands, Result),
    
    % Verify workflow completed
    assertion(Result = session_completed(_, ok(transaction_committed(_, _, _)), ok(merged_and_deleted(_)))),
    
    % Verify file exists (merged to main)
    assertion(exists_file("/tmp/test_workflow_file.txt")),
    
    % Clean up
    delete_file("/tmp/test_workflow_file.txt").

test(multi_transaction_session, [setup(setup), cleanup(teardown)]) :-
    % Start a session
    start_session(SessionId, _),
    
    % Execute multiple transactions
    execute_transaction(SessionId, [command(mkfile("/tmp/test_multi1.txt"))], Result1),
    execute_transaction(SessionId, [command(mkfile("/tmp/test_multi2.txt"))], Result2),
    
    % Verify both transactions succeeded
    assertion(Result1 = ok(transaction_committed(_, _, _))),
    assertion(Result2 = ok(transaction_committed(_, _, _))),
    
    % Verify both files exist
    assertion(exists_file("/tmp/test_multi1.txt")),
    assertion(exists_file("/tmp/test_multi2.txt")),
    
    % Close session
    close_session(SessionId, merge_to_main, _),
    
    % Clean up
    delete_file("/tmp/test_multi1.txt"),
    delete_file("/tmp/test_multi2.txt").

test(session_history, [setup(setup), cleanup(teardown)]) :-
    % Start a session and make transactions
    start_session(SessionId, _),
    execute_transaction(SessionId, [command(mkfile("/tmp/test_history1.txt"))], _),
    execute_transaction(SessionId, [command(mkfile("/tmp/test_history2.txt"))], _),
    
    % Get session history
    show_session_history(SessionId),
    
    % Verify session has transactions recorded
    list_session_transactions(SessionId, Transactions),
    assertion(length(Transactions, 2)),
    
    % Close session
    close_session(SessionId, abandon, _).

:- end_tests(session_management).

% Test suite for session integration
:- begin_tests(session_integration).

setup :-
    % Clean up any existing sessions
    retractall(active_session(_, _, _)),
    retractall(session_transaction(_, _, _, _)).

teardown :-
    cleanup_test_sessions,
    run(command(git(checkout(['main']))), _).

test(execute_with_active_session, [setup(setup), cleanup(teardown)]) :-
    % Start a session
    start_session(SessionId, _),
    
    % Use the enhanced execute/2 - should work within session
    execute(transaction([command(mkfile("/tmp/test_integrate.txt"))]), Result),
    
    % Verify it executed within the session
    assertion(Result = ok(transaction_committed(_, _, _))),
    
    % Verify transaction is tracked in session
    list_session_transactions(SessionId, Transactions),
    assertion(length(Transactions, 1)),
    
    % Clean up
    close_session(SessionId, abandon, _).

test(execute_without_session, [setup(setup), cleanup(teardown)]) :-
    % Ensure no active session
    assertion(\+ active_session(_, _, _)),
    
    % Use execute/2 without session - should work in legacy mode
    execute(transaction([command(mkfile("/tmp/test_legacy.txt"))]), Result),
    
    % Verify it executed in legacy mode
    assertion(Result = ok([ok(_)])),
    
    % Verify file exists
    assertion(exists_file("/tmp/test_legacy.txt")),
    
    % Clean up
    delete_file("/tmp/test_legacy.txt").

test(session_context_discovery, [setup(setup), cleanup(teardown)]) :-
    % Test without active session
    discover_session_context(Context1),
    assertion(Context1 = "No active session - working on main branch"),
    
    % Test with active session
    start_session(SessionId, _),
    execute_transaction(SessionId, [command(mkfile("/tmp/test_context.txt"))], _),
    
    discover_session_context(Context2),
    assertion(sub_string(Context2, _, _, _, "Active session:")),
    
    % Clean up
    close_session(SessionId, abandon, _).

:- end_tests(session_integration).

% Test suite for advanced session features
:- begin_tests(session_advanced).

setup :-
    retractall(active_session(_, _, _)),
    retractall(session_transaction(_, _, _, _)),
    run(command(git(checkout(['main']))), _).

teardown :-
    cleanup_test_sessions,
    run(command(git(checkout(['main']))), _).

test(feature_session_template, [setup(setup), cleanup(teardown)]) :-
    % Test feature session template
    start_feature_session("user-auth", SessionId, Result),
    
    % Verify feature session started
    assertion(Result = ok(feature_session_started(SessionId, "user-auth"))),
    assertion(active_session(SessionId, _, _)),
    
    % Clean up
    close_session(SessionId, abandon, _).

test(experiment_session_template, [setup(setup), cleanup(teardown)]) :-
    % Test experiment session template
    start_experiment_session("new-algorithm", SessionId, Result),
    
    % Verify experiment session started
    assertion(Result = ok(experiment_session_started(SessionId, "new-algorithm"))),
    assertion(active_session(SessionId, _, _)),
    
    % Clean up
    close_session(SessionId, abandon, _).

test(session_snapshot, [setup(setup), cleanup(teardown)]) :-
    % Start session and make changes
    start_session(SessionId, _),
    execute_transaction(SessionId, [command(mkfile("/tmp/test_snapshot.txt"))], _),
    
    % Create snapshot
    create_session_snapshot(SessionId, "before-refactor", Result),
    
    % Verify snapshot created
    assertion(Result = ok(snapshot_created(_))),
    
    % Verify we're still on session branch
    active_session(SessionId, SessionBranch, _),
    get_current_branch(CurrentBranch),
    assertion(CurrentBranch = SessionBranch),
    
    % Clean up
    close_session(SessionId, abandon, _),
    % Clean up snapshot branch
    run(command(git(branch(['-D', 'session-*-snapshot-before-refactor']))), _).

:- end_tests(session_advanced).

% Tests complete - string_trim is already defined in session.pl
