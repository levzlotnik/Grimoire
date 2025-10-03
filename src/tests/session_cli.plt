% Session CLI Tests
:- use_module(library(plunit)).

% Test suite for session CLI functionality
:- begin_tests(session_cli).

% === SETUP AND CLEANUP ===

setup :-
    % Ensure clean state before each test
    catch(cast(conjure(git(checkout(['main']))), _), _, true),
    cleanup_all_test_branches.

teardown :-
    % Clean up after each test
    catch(cast(conjure(git(checkout(['main']))), _), _, true),
    cleanup_all_test_branches.

cleanup_all_test_branches :-
    % Clean up any session or transition branches from tests
    cast(conjure(git(branch(['--format=%(refname:short)']))), BranchResult),
    (BranchResult = ok(result(BranchOutput, _)) ->
        split_string(BranchOutput, '\n', '\n \t', BranchLines),
        include(is_test_branch, BranchLines, TestBranches),
        maplist(force_delete_branch, TestBranches)
    ; 
        true
    ).

is_test_branch(BranchName) :-
    string_concat("session-", _, BranchName) ;
    string_concat("transition_branch/", _, BranchName).

force_delete_branch(BranchName) :-
    cast(conjure(git(branch(['-D', BranchName]))), _).

% === SESSION SUBCOMMAND TESTS ===

test(session_list_command, [setup(setup), cleanup(teardown)]) :-
    % Test session list command works
    list_all_sessions(Result),
    !,
    assertion(Result = ok(session_list(_))).

test(session_switch_nonexistent, [setup(setup), cleanup(teardown)]) :-
    % Test switching to non-existent session
    switch_to_session('nonexistent-session', Result),
    assertion(Result == error(session_not_found('nonexistent-session'))).

test(session_status_command, [setup(setup), cleanup(teardown)]) :-
    % Test that session status can be called
    cast(conjure(interface(status)), Result),
    !,
    assertion(Result = ok(session_status(_))).

% === CLI ARGUMENT PARSING TESTS ===

% Note: These tests would require loading the grimoire CLI script
% which defines parse_session_args/3. For now, we'll test the underlying
% session functionality that the CLI calls.

% === FORMATTING TESTS ===

test(session_display_formatting, [setup(setup), cleanup(teardown)]) :-
    % Test that session display names are extracted correctly
    % This tests the extract_session_display_name function from grimoire CLI
    % We can't directly test it here since it's in the CLI script,
    % but we can test the session data structures it would format
    perceive(session(list(Sessions))),
    !,
    assertion(is_list(Sessions)).

:- end_tests(session_cli).