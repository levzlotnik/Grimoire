:- begin_tests(core_ecs).

test(entity_declaration) :-
    % Test basic entity declaration
    load_entity(semantic(file('src/prolog/git.pl'))),
    entity(git).

test(component_relation) :-
    % Test component relationship
    load_entity(semantic(file('src/prolog/git.pl'))),
    component(command, ctor, git).

test(constructor_pattern) :-
    % Test constructor pattern
    load_entity(semantic(file('src/prolog/git.pl'))),
    component(git, subcommand, status).

test(docstring_exists) :-
    % Test docstring availability
    load_entity(semantic(file('src/prolog/git.pl'))),
    docstring(git, _).

test(semantic_mounting) :-
    % Test semantic mounting
    load_entity(semantic(file('src/prolog/git.pl'))),
    absolute_file_name('src/prolog/git.pl', AbsPath),
    mounted_semantic(AbsPath, _).

test(passive_loading) :-
    % Test passive loading works correctly with a fresh entity
    % Use the rust template since it's not loaded by default
    passive_load(test_entity, semantic(file('src/prolog/nix/templates/rust/semantics.pl'))),
    entity(test_entity),
    component(test_entity, to_be_loaded, semantic(file('src/prolog/nix/templates/rust/semantics.pl'))).

:- end_tests(core_ecs).
