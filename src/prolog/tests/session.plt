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
    cleanup_test_branches.

teardown :-
    % Clean up test sessions and branches
    cleanup_test_branches,
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
    assertion(TransitionBranch = none).

test(dirty_state_session_creation, [setup(setup), cleanup(teardown)]) :-
    % Create dirty state by modifying README - append text to it
    append_to_file('README.md', '\n# Test dirty state\n'),

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

    % Clean up dirty state
    run(command(git(checkout(['--', 'README.md']))), _).

% Helper to append text to file
append_to_file(Path, Text) :-
    read_file_to_string(Path, Content, []),
    string_concat(Content, Text, NewContent),
    setup_call_cleanup(
        open(Path, write, Stream),
        write(Stream, NewContent),
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
    % Create various types of changes
    write_file('test_new_file.txt', 'test content'),
    run(command(git(add(['test_new_file.txt']))), _),
    append_to_file('README.md', '\n# Modified README\n'),

    % Check parsed status
    check_tracked_changes(Status),
    assertion(Status = dirty(tracked_changes(Changes))),

    % Verify structured terms
    member(added('test_new_file.txt'), Changes),
    member(modified('README.md'), Changes),

    % Clean up
    run(command(git(reset(['HEAD', 'test_new_file.txt']))), _),
    delete_file('test_new_file.txt'),
    run(command(git(checkout(['--', 'README.md']))), _).

test(transition_branch_management, [setup(setup), cleanup(teardown)]) :-
    % Create a few transition branches manually for testing
    run(command(git(checkout(['-b', 'transition_branch/main--session-test1']))), _),
    run(command(git(checkout(['main']))), _),
    run(command(git(checkout(['-b', 'transition_branch/main--session-test2']))), _),
    run(command(git(checkout(['main']))), _),

    % Test listing transition branches
    list_transition_branches(Transitions),
    assertion(length(Transitions, 2)),
    assertion(member('transition_branch/main--session-test1', Transitions)),
    assertion(member('transition_branch/main--session-test2', Transitions)).

test(should_use_transition_logic, [setup(setup), cleanup(teardown)]) :-
    % Clean state, new session - should not use transition
    check_tracked_changes(Status1),
    assertion(Status1 = clean),
    assertion(\+ should_use_transition('test-new-session')),

    % Create dirty state
    append_to_file('README.md', '\n# Test\n'),

    % Dirty state, new session - should use transition
    assertion(should_use_transition('test-new-session-2')),

    % Clean up
    run(command(git(checkout(['--', 'README.md']))), _).

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
    assertion(CreateResult = ok(session_created('test-workflow-clean', direct_creation))),

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
    % Create dirty state
    run(command(executable_program(path(echo), ['# Test workflow dirty', '>>', 'README.md'])), _),

    % Full workflow with dirty state
    start_session_with_transition('test-workflow-dirty', CreateResult),
    assertion(CreateResult = ok(session_created('test-workflow-dirty', transition_used(_)))),

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
    run(command(git(checkout(['--', 'README.md']))), _),
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
