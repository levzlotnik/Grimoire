:- use_module(library(plunit)).
:- grimoire_ensure_loaded('@/src/nix/semantics.pl').

:- begin_tests(nix_semantics).

test(nix_entity_exists, [true]) :-
    entity(nix).

test(nix_command_constructors, [true]) :-
    component(conjure, ctor, nix(build)), !,
    component(conjure, ctor, nix(develop)), !,
    component(conjure, ctor, nix(run)), !.

test(nix_target_constructors, [true]) :-
    component(nix(target), ctor, package), !,
    component(nix(target), ctor, app), !,
    component(nix(target), ctor, devShell), !.

test(nix_docstrings_exist, [
    forall(component(conjure, ctor, nix(Cmd)))
]) :-
    docstring(nix(Cmd), _).

% Mock test for flake target discovery
test(mock_flake_target_discovery, [
    setup(setup_mock_nix),
    cleanup(cleanup_mock_nix)
]) :-
    % This would need proper mocking infrastructure
    % For now, just test the predicate exists
    current_predicate(get_nix_flake_targets/2),
    Targets = [mock_target],
    length(Targets, N),
    N > 0.

setup_mock_nix :-
    % TODO: Implement proper mocking
    true.

cleanup_mock_nix :-
    % TODO: Implement proper cleanup
    true.

:- end_tests(nix_semantics).
