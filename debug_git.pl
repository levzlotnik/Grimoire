% Debug script to isolate git 128 error with more specific tracing
:- ensure_loaded("src/prolog/session.pl").

test_dirty_debug :-
    % Clean setup
    catch(run(command(git(checkout(['main']))), _), _, true),
    catch(run(command(git(reset(['HEAD']))), _), _, true),
    catch(delete_file('test_dirty_state.txt'), _, true),

    % Create dirty state
    write_file('test_dirty_state.txt', 'initial content'),
    run(command(git(add(['test_dirty_state.txt']))), AddResult),
    format('Git add result: ~w~n', [AddResult]),

    write_file('test_dirty_state.txt', 'modified content'),

    % Check git status
    check_tracked_changes(Status),
    format('Git status: ~w~n', [Status]),

    % Test individual transition creation steps
    format('Testing transition creation...~n'),
    SessionId = 'debug-session',
    get_current_branch(SourceBranch),
    format('Source branch: ~w~n', [SourceBranch]),

    % Test transition branch creation
    format(string(TransitionBranch), "transition_branch/~w--session-~w", [SourceBranch, SessionId]),
    format('Creating transition branch: ~w~n', [TransitionBranch]),

    catch(
        run(command(git(checkout(['-b', TransitionBranch]))), CreateResult),
        Error1,
        (format('Branch creation error: ~w~n', [Error1]), fail)
    ),
    format('Branch creation result: ~w~n', [CreateResult]),

    % Test git add -u
    format('Testing git add -u...~n'),
    catch(
        run(command(git(add(['-u']))), AddUResult),
        Error2,
        (format('Git add -u error: ~w~n', [Error2]), fail)
    ),
    format('Git add -u result: ~w~n', [AddUResult]),

    % Cleanup
    catch(run(command(git(checkout(['main']))), _), _, true),
    catch(run(command(git(branch(['-D', TransitionBranch]))), _), _, true),
    catch(run(command(git(reset(['HEAD']))), _), _, true),
    catch(delete_file('test_dirty_state.txt'), _, true).
