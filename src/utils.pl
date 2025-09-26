% Utils entity - General utilities for the Grimoire system
:- self_entity(utils).

% Entity declarations
entity(entity_hierarchy).

% Documentation
docstring(utils, "General utilities and helper predicates for the Grimoire system.
Provides common functionality used across different domains including
entity hierarchy traversal, tree building, and other shared patterns.").

% Perceive constructor for entity hierarchy
component(perceive, ctor, entity_hierarchy).

% Entity hierarchy perceive pattern
% Builds a canonical tree structure from child components
perceive(entity_hierarchy(RootEntity, Hierarchy)) :-
    build_entity_tree(RootEntity, Hierarchy).

% Build tree recursively from child components only
% Tree structure: tree(Entity, Children) - no docstrings to keep it lean
build_entity_tree(Entity, tree(Entity, Children)) :-
    findall(ChildTree, (
        component(Entity, child, Child),
        build_entity_tree(Child, ChildTree)
    ), Children).

% Documentation for entity_hierarchy
docstring(entity_hierarchy, "Build a hierarchical tree structure from entity child relationships.
    Format: perceive(entity_hierarchy(RootEntity, Tree))
    
    Returns a tree structure where each node is:
    tree(Entity, [ChildTree1, ChildTree2, ...])
    
    Only follows 'child' component relationships. Other domains can subscribe
    their components to the child pattern using rules like:
    component(Entity, child, Child) :- component(Entity, ctor, Child).
    
    Example:
    ?- perceive(entity_hierarchy(project, Tree)).
    Tree = tree(project, [
        tree(project(frontend), [
            tree(project(frontend(src)), [...])
        ]),
        tree(project(backend), [])
    ]).").

% Utility to check if a file is a semantics.pl or semantics.plt file
is_semantics_file(Path) :-
    (sub_atom(Path, _, _, 0, 'semantics.pl') ; sub_atom(Path, _, _, 0, 'semantics.plt')).

% Helper to read lines from a stream
read_lines_from_stream(Stream, Lines) :-
    read_line_to_codes(Stream, Codes),
    (Codes == end_of_file ->
        Lines = []
    ;
        atom_codes(Line, Codes),
        Lines = [Line|RestLines],
        read_lines_from_stream(Stream, RestLines)
    ).