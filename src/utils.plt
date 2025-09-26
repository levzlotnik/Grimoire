% PLUnit tests for utils entity

:- begin_tests(utils).

% Utils module is already loaded by grimoire.pl

% Test that utils entity exists
test(entity_exists) :-
    entity(utils).

% Test entity_hierarchy perceive constructor exists
test(entity_hierarchy_constructor) :-
    component(perceive, ctor, entity_hierarchy).

% Test basic entity hierarchy building
test(simple_hierarchy) :-
    % Create a simple test hierarchy
    assertz(component(test_root, child, test_child1)),
    assertz(component(test_root, child, test_child2)),
    assertz(component(test_child1, child, test_grandchild)),
    
    % Build hierarchy
    perceive(entity_hierarchy(test_root, Tree)),
    
    % Verify structure
    Tree = tree(test_root, Children),
    length(Children, 2),
    member(tree(test_child1, GrandChildren), Children),
    member(tree(test_child2, []), Children),
    length(GrandChildren, 1),
    member(tree(test_grandchild, []), GrandChildren),
    
    % Cleanup
    retractall(component(test_root, child, _)),
    retractall(component(test_child1, child, _)).

% Test is_semantics_file predicate
test(is_semantics_file_pl) :-
    is_semantics_file('some/path/semantics.pl').

test(is_semantics_file_plt) :-
    is_semantics_file('test/semantics.plt').

test(not_semantics_file) :-
    \+ is_semantics_file('some/other/file.txt').

% Test semantic_entity_id integration
test(semantic_entity_id_integration) :-
    % This tests that we can find entities by their semantic source
    % The utils entity itself should be findable
    semantic_entity_id(semantic(file(_)), utils).

:- end_tests(utils).