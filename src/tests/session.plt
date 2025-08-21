% Clean Session System Tests
:- use_module(library(plunit)).
:- use_module(library(uuid)).

% Test suite for clean session system
:- begin_tests(clean_session_system).

% === SETUP AND CLEANUP ===

setup :-
    % Ensure clean state before each test
    catch(run(command(git(checkout(['main']))), _), _, true),
    cleanup_all_test_branches.

teardown :-
    % Clean up after each test
    catch(run(command(git(checkout(['main']))), _), _, true),
    cleanup_all_test_branches.

cleanup_all_test_branches :-
    % Clean up any session or transition branches from tests
    run(command(git(branch(['--format=%(refname:short)']))), BranchResult),
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
    run(command(git(branch(['-D', BranchName]))), _).

% Helper to create test file
create_test_file(FileName, Content) :-
    setup_call_cleanup(
        open(FileName, write, Stream),
        write(Stream, Content),
        close(Stream)
    ).

% === PATTERN 1 TESTS: Clean state → any session ===

test(clean_to_new_session, [setup(setup), cleanup(teardown)]) :-
    % Verify clean state
    check_working_tree_status(Status),
    assertion(Status = clean),
    
    % Start new session from clean state
    start_session('test-new-session', Result),
    
    % Should use direct checkout pattern
    assertion(Result = ok(session_started('test-new-session', new, clean, direct))),
    
    % Verify session branch was created and we're on it
    get_current_branch(CurrentBranch),
    assertion(CurrentBranch = 'session-test-new-session'), !.

test(clean_to_existing_session, [setup(setup), cleanup(teardown)]) :-
    % Create a session first
    start_session('test-existing', _),
    close_session('test-existing', keep_branch, _),
    
    % Return to main with clean state
    run(command(git(checkout(['main']))), _),
    check_working_tree_status(Status),
    assertion(Status = clean),
    
    % Start existing session from clean state
    start_session('test-existing', Result),
    
    % Should use direct checkout to existing
    assertion(Result = ok(session_started('test-existing', existing, clean, direct))),
    
    % Verify we're on the existing session branch
    get_current_branch(CurrentBranch),
    assertion(CurrentBranch = 'session-test-existing'), !.

% === PATTERN 2 TESTS: Dirty state → existing session ===

test(dirty_to_existing_session, [setup(setup), cleanup(teardown)]) :-
    % Create a session and keep it
    start_session('test-dirty-existing', _),
    close_session('test-dirty-existing', keep_branch, _),
    
    % Return to main and create dirty state
    run(command(git(checkout(['main']))), _),
    create_test_file('dirty_test.txt', 'dirty content'),
    run(command(git(add(['dirty_test.txt']))), _),
    
    % Verify dirty state
    check_working_tree_status(Status),
    assertion(Status = dirty),
    
    % Start existing session from dirty state
    start_session('test-dirty-existing', Result),
    
    % Should use direct checkout (let git handle merge)
    assertion(Result = ok(session_started('test-dirty-existing', existing, dirty, direct_with_merge))),
    
    % Clean up test file
    catch(delete_file('dirty_test.txt'), _, true), !.

% === PATTERN 3 TESTS: Dirty state → NEW session (via transition) ===

test(dirty_to_new_session_via_transition, [setup(setup), cleanup(teardown)]) :-
    % Create dirty state
    create_test_file('dirty_transition_test.txt', 'dirty content'),
    run(command(git(add(['dirty_transition_test.txt']))), _),
    
    % Verify dirty state
    check_working_tree_status(Status),
    assertion(Status = dirty),
    
    % Start NEW session from dirty state
    start_session('test-transition-new', Result),
    
    % Should use transition branch workflow
    assertion(Result = ok(session_started('test-transition-new', new, dirty, via_transition(_)))),
    
    % Verify we're on the new session branch
    get_current_branch(CurrentBranch),
    assertion(CurrentBranch = 'session-test-transition-new'),
    
    % Verify transition branch was created (extract from Result)
    Result = ok(session_started(_, _, _, via_transition(TransitionBranch))),
    run(command(git(branch(['--list', TransitionBranch]))), BranchCheck),
    BranchCheck = ok(result(BranchOutput, _)),
    assertion(BranchOutput \= ""),
    
    % Clean up test file
    catch(delete_file('dirty_transition_test.txt'), _, true), !.

% === SESSION MANAGEMENT TESTS ===

test(session_exists_check, [setup(setup), cleanup(teardown)]) :-
    % Non-existent session
    assertion(\+ session_exists('non-existent')),
    
    % Create session
    start_session('test-exists', _),
    
    % Now should exist
    assertion(session_exists('test-exists')), !.

test(session_branch_naming, [setup(setup), cleanup(teardown)]) :-
    % Test session branch name generation
    session_branch_name('my-session', BranchName),
    assertion(BranchName = 'session-my-session').

test(transition_branch_naming, [setup(setup), cleanup(teardown)]) :-
    % Test transition branch name generation
    transition_branch_name('main', 'new-session', TransitionName),
    assertion(TransitionName = 'transition_branch/main--new-session').

% === SESSION CLOSURE TESTS ===

test(session_merge_to_main, [setup(setup), cleanup(teardown)]) :-
    % Create and work in session
    start_session('test-merge', _),
    create_test_file('session_work.txt', 'session content'),
    run(command(git(add(['session_work.txt']))), _),
    run(command(git(commit(['-m', 'Session work']))), _),
    
    % Close with merge
    close_session('test-merge', merge_to_main, Result),
    assertion(Result = ok(session_closed('test-merge', merged_to_main))),
    
    % Verify we're back on main
    get_current_branch(CurrentBranch),
    assertion(CurrentBranch = 'main'),
    
    % Verify session branch was deleted
    assertion(\+ session_exists('test-merge')),
    
    % Clean up test file
    catch(delete_file('session_work.txt'), _, true), !.

test(session_abandon, [setup(setup), cleanup(teardown)]) :-
    % Create session
    start_session('test-abandon', _),
    create_test_file('abandon_test.txt', 'abandon content'),
    run(command(git(add(['abandon_test.txt']))), _),
    run(command(git(commit(['-m', 'Work to abandon']))), _),
    
    % Abandon session
    close_session('test-abandon', abandon, Result),
    assertion(Result = ok(session_closed('test-abandon', abandoned))),
    
    % Verify we're back on main
    get_current_branch(CurrentBranch),
    assertion(CurrentBranch = 'main'),
    
    % Verify session branch was deleted
    assertion(\+ session_exists('test-abandon')),
    
    % Clean up any remaining test file
    catch(delete_file('abandon_test.txt'), _, true), !.

% === WORKING TREE STATUS TESTS ===

test(working_tree_status_detection, [setup(setup), cleanup(teardown)]) :-
    % Clean state
    check_working_tree_status(Status1),
    assertion(Status1 = clean),
    
    % Create dirty state
    create_test_file('status_test.txt', 'test content'),
    run(command(git(add(['status_test.txt']))), _),
    check_working_tree_status(Status2),
    assertion(Status2 = dirty),
    
    % Clean up
    run(command(git(reset(['HEAD']))), _),
    catch(delete_file('status_test.txt'), _, true), !.

:- end_tests(clean_session_system).