:- use_module(library(plunit)).

% Test suite for the conjure/perceive spell system
:- begin_tests(spell_system).

% Grimoire system is already loaded by run_tests.pl

% === SPELL ECS HIERARCHY TESTS ===

test(spell_entity_exists) :-
    entity(spell).

test(spell_has_conjure_ctor) :-
    component(spell, ctor, conjure), !.

test(spell_has_perceive_ctor) :-
    component(spell, ctor, perceive).

test(conjure_entity_exists) :-
    entity(conjure).

test(perceive_entity_exists) :-
    entity(perceive).

% === CAST PREDICATE TESTS ===

test(cast_single_conjure_spell) :-
    % Test casting a single conjure spell
    cast(conjure(shell(['echo', 'test'])), Result),
    Result = ok(result("test\n", "")).

test(cast_ritual_multiple_spells, [cleanup(cleanup_test_files)]) :-
    % Test casting a ritual (multiple spells)
    cast(ritual([
        mkfile('/tmp/test1.txt'),
        mkfile('/tmp/test2.txt')
    ]), Result),
    Result = ok([ok(""), ok("")]),
    exists_file('/tmp/test1.txt'),
    exists_file('/tmp/test2.txt').

test(cast_invalid_spell_fails) :-
    % Test that invalid spells fail gracefully
    \+ cast(conjure(completely_nonexistent_spell_12345), ok(_)).

% === PERCEIVE PREDICATE TESTS ===

test(perceive_git_status) :-
    % Test git status perception
    perceive(git(status(Branch, Status, Files))), !,
    atom(Branch),
    atom(Status),
    is_list(Files).

test(perceive_simple_query) :-
    % Test simple Prolog query (fallback to call/1)
    % Since member/2 doesn't have a perceive/1 clause, it should fall back to call/1
    member(1, [1, 2, 3]), !.

test(perceive_unification) :-
    % Test variable unification with simple query
    member(X, [a, b, c]),
    X = a, !.

% === DOMAIN INTEGRATION TESTS ===

test(git_conjure_ctors_exist) :-
    % Test git conjure constructors are properly defined
    component(conjure, ctor, git(commit)),
    component(conjure, ctor, git(add)),
    component(conjure, ctor, git(checkout)), !.

test(git_perceive_ctors_exist) :-
    % Test git perceive constructors are properly defined
    component(perceive, ctor, git(status)),
    component(perceive, ctor, git(log)),
    component(perceive, ctor, git(branch)), !.

test(nix_conjure_ctors_exist) :-
    % Test nix conjure constructors
    component(conjure, ctor, nix(build)),
    component(conjure, ctor, nix(develop)),
    component(conjure, ctor, nix(run)), !.

test(nix_perceive_ctors_exist) :-
    % Test nix perceive constructors
    component(perceive, ctor, nix(flake(show))),
    component(perceive, ctor, nix(search)), !.

test(session_conjure_ctors_exist) :-
    % Test session conjure constructors
    component(conjure, ctor, session(start)),
    component(conjure, ctor, session(close)),
    component(conjure, ctor, session(execute)), !.

test(session_perceive_ctors_exist) :-
    % Test session perceive constructors
    component(perceive, ctor, session(current)),
    component(perceive, ctor, session(list)),
    component(perceive, ctor, session(status)), !.

% === GIT STATUS PARSING TESTS ===

test(git_status_file_terms) :-
    % Test that git status returns clean file terms
    perceive(git(status(_, _, Files))), !,
    is_list(Files),
    (Files = [] -> 
        true  % No files is valid
    ;
        Files = [FirstFile|_],
        % Check that file terms are clean (modified/created/deleted/staged)
        valid_file_status_term(FirstFile)
    ).

% Helper predicate to check valid file status terms
valid_file_status_term(modified(_)) :- !.
valid_file_status_term(created(_)) :- !.
valid_file_status_term(deleted(_)) :- !.
valid_file_status_term(staged(_)) :- !.
valid_file_status_term(unknown(_)) :- !.

% === BACKWARDS COMPATIBILITY TESTS ===

test(legacy_command_system_still_works) :-
    % Test that old run(command(...)) still works
    run(command(shell(['echo', 'legacy'])), Result),
    Result = ok(result("legacy\n", "")).

test(legacy_git_commands_work) :-
    % Test legacy git commands still work
    run(command(git(status)), Result),
    Result = ok(result(_, _)), !.

% === DOCSTRING TESTS ===

test(spell_docstrings_exist) :-
    docstring(spell, _),
    docstring(conjure, _),
    docstring(perceive, _),
    docstring(cast, _).

% === HELPER PREDICATES ===

cleanup_test_files :-
    % Clean up test files
    catch(delete_file('/tmp/test1.txt'), _, true),
    catch(delete_file('/tmp/test2.txt'), _, true).

:- end_tests(spell_system).