:- use_module(library(plunit)).
:- ensure_loaded('../grimoire.pl').

:- begin_tests(core_ecs).

test(entity_declaration, [true]) :-
    entity(component).

test(component_relation, [true]) :-
    component(component, relation_pattern, ctor).

test(constructor_pattern, [true]) :-
    entity(ctor).

test(docstring_exists, [true]) :-
    docstring(ctor, _).

test(semantic_mounting, [
    setup(create_test_semantic_file),
    cleanup(cleanup_test_semantic_file)
]) :-
    mount_semantic(file('/tmp/test_semantic.pl')),
    mounted_semantic('/tmp/test_semantic.pl', _).

create_test_semantic_file :-
    write_file('/tmp/test_semantic.pl',
        "entity(test_entity).\ncomponent(test_entity, test_component, test_value).\n").

cleanup_test_semantic_file :-
    (exists_file('/tmp/test_semantic.pl') ->
        delete_file('/tmp/test_semantic.pl')
    ; true),
    (mounted_semantic('/tmp/test_semantic.pl', _) ->
        unmount_semantic('/tmp/test_semantic.pl')
    ; true).

test(load_entity_self_binding, [
    setup(setup_self_binding_test),
    cleanup(cleanup_self_binding_test)
]) :-
    % Load entity with self-binding
    load_entity(test_git, semantic(file('src/prolog/tests/self_semantic_fixture.pl'))),

    % Verify self was transformed to test_git
    entity(test_git),
    component(command, ctor, test_git),
    component(test_git, subcommand, status),
    component(test_git, subcommand, diff),

    % Verify docstrings were transformed
    docstring(test_git, "Test entity that refers to itself"),
    docstring(test_git(status), "Show status of self repository"),

    % Verify nested self-references were transformed
    component(test_git, related_command, transaction([
        command(test_git(status)),
        command(test_git(diff))
    ])),

    % Verify source relationship was recorded
    component(test_git, source, semantic(file('src/prolog/tests/self_semantic_fixture.pl'))).

setup_self_binding_test :-
    % Test file already exists at src/prolog/tests/self_semantic_fixture.pl
    true.

cleanup_self_binding_test :-
    % Clean up any asserted facts from the test
    retractall(entity(test_git)),
    retractall(component(test_git, _, _)),
    retractall(component(command, ctor, test_git)),
    retractall(docstring(test_git, _)),
    retractall(docstring(test_git(_), _)),
    % Unmount the semantic file with absolute path
    absolute_file_name('src/prolog/tests/self_semantic_fixture.pl', AbsPath),
    (mounted_semantic(AbsPath, _) ->
        unmount_semantic(AbsPath)
    ; true).

% Test entity declaration patterns
test(absolute_entity_pattern, [true]) :-
    % Validate absolute domain entities are directly available
    entity(git),  % From git.pl - absolute domain entity
    component(git, source, file("git.pl")),
    component(command, ctor, git).

test(relative_entity_pattern_via_load_entity, [
    setup(true),
    cleanup(cleanup_relative_entity_test)
]) :-
    % Test that load_entity properly binds self to target entity
    load_entity(test_project, semantic(file('src/prolog/tests/self_semantic_fixture.pl'))),

    % Verify self was bound to test_project
    entity(test_project),
    component(command, ctor, test_project),

    % Verify original self references were transformed
    \+ entity(self),  % No lingering self entity
    component(test_project, source, semantic(file('src/prolog/tests/self_semantic_fixture.pl'))).

cleanup_relative_entity_test :-
    retractall(entity(test_project)),
    retractall(component(test_project, _, _)),
    retractall(component(command, ctor, test_project)),
    retractall(docstring(test_project, _)),
    retractall(docstring(test_project(_), _)),
    % Unmount the semantic file with absolute path
    absolute_file_name('src/prolog/tests/self_semantic_fixture.pl', AbsPath),
    (mounted_semantic(AbsPath, _) ->
        unmount_semantic(AbsPath)
    ; true).

% Test passive loading functionality
test(passive_loading_basic, [
    setup(true),
    cleanup(cleanup_passive_test)
]) :-
    % Create a passive entity
    passive_load(test_entity, semantic(file('src/prolog/tests/self_semantic_fixture.pl'))),

    % Entity should exist
    entity(test_entity),

    % Should have to_be_loaded component
    component(test_entity, to_be_loaded, semantic(file('src/prolog/tests/self_semantic_fixture.pl'))),

    % Should have default docstring
    docstring(test_entity, Doc),
    sub_string(Doc, _, _, _, "To get a full view"),

    % Explicit loading should work
    load_entity(test_entity, semantic(file('src/prolog/tests/self_semantic_fixture.pl'))),

    % After loading, to_be_loaded should be removed
    \+ component(test_entity, to_be_loaded, _),

    % All components should now be available
    component(test_entity, subcommand, status),
    component(test_entity, subcommand, diff),
    component(command, ctor, test_entity).

cleanup_passive_test :-
    retractall(entity(test_entity)),
    retractall(component(test_entity, _, _)),
    retractall(component(command, ctor, test_entity)),
    retractall(docstring(test_entity, _)),
    retractall(docstring(test_entity(_), _)),
    % Unmount the semantic file with absolute path
    absolute_file_name('src/prolog/tests/self_semantic_fixture.pl', AbsPath),
    (mounted_semantic(AbsPath, _) ->
        unmount_semantic(AbsPath)
    ; true).

% Test passive vs immediate loading
test(passive_vs_immediate_loading, [
    setup(true),
    cleanup(cleanup_passive_immediate_test)
]) :-
    % Load same source both immediately and passively
    load_entity(test_immediate, semantic(file('src/prolog/tests/self_semantic_fixture.pl'))),
    passive_load(test_passive, semantic(file('src/prolog/tests/self_semantic_fixture.pl'))),

    % Immediate should be fully available
    component(test_immediate, subcommand, status),
    component(test_immediate, source, semantic(file('src/prolog/tests/self_semantic_fixture.pl'))),

    % Passive should only have to_be_loaded component initially
    component(test_passive, to_be_loaded, semantic(file('src/prolog/tests/self_semantic_fixture.pl'))),
    \+ component(test_passive, subcommand, _),

    % After explicit loading, passive should behave like immediate
    load_entity(test_passive, semantic(file('src/prolog/tests/self_semantic_fixture.pl'))),
    component(test_passive, subcommand, status),
    \+ component(test_passive, to_be_loaded, _).

cleanup_passive_immediate_test :-
    retractall(entity(test_immediate)),
    retractall(component(test_immediate, _, _)),
    retractall(component(command, ctor, test_immediate)),
    retractall(entity(test_passive)),
    retractall(component(test_passive, _, _)),
    retractall(component(command, ctor, test_passive)),
    retractall(docstring(test_immediate, _)),
    retractall(docstring(test_immediate(_), _)),
    retractall(docstring(test_passive, _)),
    retractall(docstring(test_passive(_), _)),
    % Unmount the semantic file with absolute path
    absolute_file_name('src/prolog/tests/self_semantic_fixture.pl', AbsPath),
    (mounted_semantic(AbsPath, _) ->
        unmount_semantic(AbsPath)
    ; true).

:- end_tests(core_ecs).
