% Session and Transaction System Tests - Transition Branch System
:- use_module(library(plunit)).
:- use_module(library(uuid)).

% Load the session system
:- ensure_loaded("../session.pl").

% Test suite for transition branch system
:- begin_tests(transition_branches).

setup :-
    % Ensure we're on main branch for tests
    catch(run(command(git(checkout(['main']))), _), _, true),
    % Clean up any existing test branches
    cleanup_test_branches,
    % Reset any staged changes but preserve working directory
    catch(run(command(git(reset(['HEAD']))), _), _, true).

teardown :-
    % Clean up test sessions and branches
    cleanup_test_branches,
    % Clean up any test files we created
    catch(delete_file('test_dirty_state.txt'), _, true),
    catch(delete_file('test_changes_new.txt'), _, true),
    catch(delete_file('test_changes_mod.txt'), _, true),
    catch(delete_file('test_logic_check.txt'), _, true),
    catch(delete_file('test_new_file.txt'), _, true),
    catch(delete_file('test_modified.txt'), _, true),
    catch(delete_file('test_dirty_check.txt'), _, true),
    % Reset any staged changes
    catch(run(command(git(reset(['HEAD']))), _), _, true),
    % Return to main
    catch(run(command(git(checkout(['main']))), _), _, true).

% Helper to clean up test branches
cleanup_test_branches :-
    % Clean up any session branches starting with test-
    catch((
        run(command(git(branch(['--format=%(refname:short)']))), BranchResult),
        (BranchResult = ok(BranchOutput) ->
            split_string(BranchOutput, '\n', '\n \t', BranchLines),
            include(is_test_branch, BranchLines, TestBranches),
            maplist(delete_test_branch, TestBranches)
        ; true)
    ), _, true),
    % Clean up any transition branches
    catch(cleanup_orphaned_transitions(_), _, true).

is_test_branch(BranchName) :-
    atom_string(BranchAtom, BranchName),
    (sub_atom(BranchAtom, 0, _, _, 'session-test-') ;
     sub_atom(BranchAtom, 0, _, _, 'transition_branch/')).

delete_test_branch(BranchName) :-
    catch(run(command(git(branch(['-D', BranchName]))), _), _, true).

test(clean_state_session_creation, [setup(setup), cleanup(teardown)]) :-
    % Ensure clean git state
    check_tracked_changes(Status),
    assertion(Status = clean),

    % Create session with clean state - should use direct creation
    start_session_with_transition('test-clean-session', Result),

    % Verify session created successfully
    assertion(Result = ok(session_started('test-clean-session', _, _, direct))),

    % Verify session branch exists
    assertion(session_exists('test-clean-session')),

    % Verify no transition branch was created
    find_transition_for_session('test-clean-session', TransitionBranch),
    assertion(TransitionBranch = none),

    % CRITICAL: Close the session properly
    close_session('test-clean-session', abandon, _).

test(dirty_state_session_creation, [setup(setup), cleanup(teardown)]) :-
    % Create dirty state using simple file operations
    write_file('test_dirty_state.txt', 'initial content'),
    run(command(git(add(['test_dirty_state.txt']))), _),
    write_file('test_dirty_state.txt', 'modified content'),

    % Verify we have dirty tracked changes
    check_tracked_changes(Status),
    assertion(Status = dirty(tracked_changes(_))),

    % Create session with dirty state - should use transition branch
    start_session_with_transition('test-dirty-session', Result),

    % Verify session created with transition
    assertion(Result = ok(session_started('test-dirty-session', _, _, via_transition(_)))),

    % Verify session branch exists
    assertion(session_exists('test-dirty-session')),

    % Verify transition branch was created
    find_transition_for_session('test-dirty-session', TransitionBranch),
    assertion(TransitionBranch \= none),

    % CRITICAL: Close the session properly
    close_session('test-dirty-session', abandon, _),
    % Clean up git state
    run(command(git(reset(['HEAD', 'test_dirty_state.txt']))), _),
    catch(delete_file('test_dirty_state.txt'), _, true).

% Helper to write content to file
write_file(Path, Content) :-
    setup_call_cleanup(
        open(Path, write, Stream),
        write(Stream, Content),
        close(Stream)
    ).

test(transition_branch_naming, [setup(setup), cleanup(teardown)]) :-
    % Test transition branch name generation
    transition_branch_name('main', 'test-session-123', TransitionBranch),
    assertion(TransitionBranch = 'transition_branch/main--session-test-session-123'),

    % Test session branch name generation
    session_branch_name('test-session-456', SessionBranch),
    assertion(SessionBranch = 'session-test-session-456').

test(tracked_changes_parsing, [setup(setup), cleanup(teardown)]) :-
    % Create test files using simple file operations
    write_file('test_changes_new.txt', 'new content'),
    write_file('test_changes_mod.txt', 'initial content'),
    
    % Add files to git and commit
    run(command(git(add(['test_changes_new.txt', 'test_changes_mod.txt']))), _),
    run(command(git(commit(['-m', 'Add test files for parsing test']))), _),
    
    % Modify one file to create dirty state
    write_file('test_changes_mod.txt', 'modified content'),

    % Check parsed status
    check_tracked_changes(Status),
    assertion(Status = dirty(tracked_changes(Changes))),

    % Verify structured terms
    member(modified('test_changes_mod.txt'), Changes),

    % Clean up git state
    run(command(git(reset(['--hard', 'HEAD~1']))), _).

test(transition_branch_management, [setup(setup), cleanup(teardown)]) :-
    % Create a few transition branches manually for testing
    run(command(git(checkout(['-b', 'transition_branch/main--session-test1']))), _),
    run(command(git(checkout(['main']))), _),
    run(command(git(checkout(['-b', 'transition_branch/main--session-test2']))), _),
    run(command(git(checkout(['main']))), _),

    % Test listing transition branches
    list_transition_branches(Transitions),
    assertion(length(Transitions, 2)),
    assertion(member("transition_branch/main--session-test1", Transitions)),
    assertion(member("transition_branch/main--session-test2", Transitions)).

test(should_use_transition_logic, [setup(setup), cleanup(teardown)]) :-
    % Clean state, new session - should not use transition
    check_tracked_changes(Status1),
    assertion(Status1 = clean),
    assertion(\+ should_use_transition('test-new-session')),

    % Create file and add to git to establish tracking
    write_file('test_logic_check.txt', 'initial content'),
    run(command(git(add(['test_logic_check.txt']))), _),
    run(command(git(commit(['-m', 'Add test file for logic check']))), _),

    % Modify file to create dirty state
    write_file('test_logic_check.txt', 'modified content'),

    % Dirty state, new session - should use transition
    assertion(should_use_transition('test-new-session-2')),

    % Clean up git state
    run(command(git(reset(['--hard', 'HEAD~1']))), _).

:- end_tests(transition_branches).

% Test suite for session lifecycle with transitions
:- begin_tests(session_lifecycle).

setup :-
    catch(run(command(git(checkout(['main']))), _), _, true),
    cleanup_test_branches.

teardown :-
    cleanup_test_branches,
    catch(run(command(git(checkout(['main']))), _), _, true).

% Use same cleanup helper
cleanup_test_branches :-
    catch((
        run(command(git(branch(['--format=%(refname:short)']))), BranchResult),
        (BranchResult = ok(BranchOutput) ->
            split_string(BranchOutput, '\n', '\n \t', BranchLines),
            include(is_test_branch, BranchLines, TestBranches),
            maplist(delete_test_branch, TestBranches)
        ; true)
    ), _, true),
    catch(cleanup_orphaned_transitions(_), _, true).

is_test_branch(BranchName) :-
    atom_string(BranchAtom, BranchName),
    (sub_atom(BranchAtom, 0, _, _, 'session-test-') ;
     sub_atom(BranchAtom, 0, _, _, 'transition_branch/')).

delete_test_branch(BranchName) :-
    catch(run(command(git(branch(['-D', BranchName]))), _), _, true).

test(full_session_workflow_clean, [setup(setup), cleanup(teardown)]) :-
    % Full workflow with clean state
    start_session_with_transition('test-workflow-clean', CreateResult),
    assertion(CreateResult = ok(session_started('test-workflow-clean', _, _, direct))),

    % Execute transaction
    execute_transaction('test-workflow-clean', [command(mkfile('/tmp/test_workflow.txt'))], ExecResult),
    assertion(ExecResult = ok(_)),

    % Close session
    close_session('test-workflow-clean', merge_to_main, CloseResult),
    assertion(CloseResult = ok(_)),

    % Verify transition cleanup (should be none since direct creation)
    find_transition_for_session('test-workflow-clean', TransitionBranch),
    assertion(TransitionBranch = none),

    % Clean up file
    catch(delete_file('/tmp/test_workflow.txt'), _, true).

test(full_session_workflow_dirty, [setup(setup), cleanup(teardown)]) :-
    % Create dirty state using proper method
    write_file('test_workflow_dirty.txt', 'initial content'),
    run(command(git(add(['test_workflow_dirty.txt']))), _),
    write_file('test_workflow_dirty.txt', 'modified content'),

    % Full workflow with dirty state
    start_session_with_transition('test-workflow-dirty', CreateResult),
    assertion(CreateResult = ok(session_started('test-workflow-dirty', _, _, via_transition(_)))),

    % Execute transaction
    execute_transaction('test-workflow-dirty', [command(mkfile('/tmp/test_workflow_dirty.txt'))], ExecResult),
    assertion(ExecResult = ok(_)),

    % Close session - should clean up transition branch
    close_session('test-workflow-dirty', merge_to_main, CloseResult),
    assertion(CloseResult = ok(_)),

    % Verify transition was cleaned up
    find_transition_for_session('test-workflow-dirty', TransitionBranch),
    assertion(TransitionBranch = none),

    % Clean up
    run(command(git(checkout(['--', 'test_workflow_dirty.txt']))), _),
    catch(delete_file('/tmp/test_workflow_dirty.txt'), _, true).

:- end_tests(session_lifecycle).

% Test suite for error handling
:- begin_tests(transition_error_handling).

setup :-
    catch(run(command(git(checkout(['main']))), _), _, true).

teardown :-
    catch(run(command(git(checkout(['main']))), _), _, true).

test(session_exists_check, [setup(setup), cleanup(teardown)]) :-
    % Non-existent session
    assertion(\+ session_exists('non-existent-session')),

    % Create session
    start_session_with_transition('test-exists', _),

    % Now it should exist
    assertion(session_exists('test-exists')),

    % Clean up
    catch(run(command(git(branch(['-D', 'session-test-exists']))), _), _, true).

test(git_status_edge_cases, [setup(setup), cleanup(teardown)]) :-
    % Test with completely clean repo
    check_tracked_changes(Status1),
    assertion(Status1 = clean),

    % Test has_dirty_tracked_files
    assertion(\+ has_dirty_tracked_files),

    % Create untracked file (should still be clean for tracked changes)
    run(command(executable_program(path(echo), ['untracked', '>', 'untracked_file.txt'])), _),
    check_tracked_changes(Status2),
    assertion(Status2 = clean),

    % Clean up
    run(command(executable_program(path(rm), ['-f', 'untracked_file.txt'])), _).

:- end_tests(transition_error_handling).
