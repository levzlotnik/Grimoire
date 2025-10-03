% Utils entity - General utilities for the Grimoire system
:- self_entity(utils).

% === ENTITY DECLARATIONS ===
entity(entity_hierarchy).
entity(utils(file_processor)).
entity(utils(tree_builder)).
entity(utils(validator)).
entity(utils(collection)).

% === DOCUMENTATION ===
docstring(utils, "
# Subsystem: Utils

## Entity Declaration
`:- self_entity(utils).`

## DSL Patterns
This entity exposes user-friendly DSL fact schemas:

```prolog
% File processing utilities
component(user_entity, has(utils(file_processor)), utils(file_processor(
    type(semantics), filters([extensions(['.pl', '.plt']), pattern('semantics.*')])
))).
% Generatively populates: component(user_entity, utils_processor_type, semantics).

% Tree building utilities
component(user_entity, has(utils(tree_builder)), utils(tree_builder(
    root(user_entity), relationship(child), options([max_depth(10)])
))).
% Generatively populates: component(user_entity, utils_tree_root, user_entity).

% Validation utilities
component(user_entity, has(utils(validator)), utils(validator(
    rules([check_existence, check_format]), on_error(throw)
))).
% Generatively populates: component(user_entity, utils_validation_rules, [check_existence, check_format]).

% Collection utilities
component(user_entity, has(utils(collection)), utils(collection(
    type(entities), operations([filter, map, reduce]), predicate(custom_filter/2)
))).
% Generatively populates: component(user_entity, utils_collection_type, entities).
```

## Verification Patterns
Users can use `please_verify/1` on fact schemas in their own `semantics.plt` files:

```prolog
please_verify(component(user_entity, has(utils(file_processor)),
    utils(file_processor(type(semantics), filters([extensions(['.pl', '.plt'])]))))).
please_verify(component(user_entity, has(utils(validator)),
    utils(validator(rules([check_existence, check_format]), on_error(throw))))).
```

## Purpose
Utils domain provides foundational utility functions and verification patterns for other Grimoire domains:
- Entity hierarchy traversal and tree building
- File processing and pattern matching utilities
- Validation framework for component verification
- Collection operations (map, filter, reduce) on entity data

## Spell Constructors
- **perceive**: `utils(entity_hierarchy(RootEntity))` - Build hierarchical tree from relationships
- **conjure**: `utils(validate(Entity, Rules))` - Validate entity against rules
- **conjure**: `utils(transform(Data, Operation))` - Transform data using map/filter/reduce

## Dependencies
- **ecs_kernel** (L0): Core ECS predicates and verification primitive
- **run_tests** (L1): Test infrastructure for PLUnit execution
- **grimoire** (L1): Spell system and domain loading
").

% === DSL PATTERNS: GENERATIVE EXPANSION ===

% File processor generative expansion
component(Entity, utils_file_extensions, Extensions) :-
    component(Entity, has(utils(file_processor)), utils(file_processor(type(_), filters(FilterList)))),
    member(extensions(Extensions), FilterList).

component(Entity, utils_processor_type, Type) :-
    component(Entity, has(utils(file_processor)), utils(file_processor(type(Type), filters(_)))).

component(Entity, utils_file_pattern, Pattern) :-
    component(Entity, has(utils(file_processor)), utils(file_processor(type(_), filters(FilterList)))),
    member(pattern(Pattern), FilterList).

% Tree builder generative expansion
component(Entity, utils_tree_root, Root) :-
    component(Entity, has(utils(tree_builder)), utils(tree_builder(root(Root), relationship(_), options(_)))).

component(Entity, utils_tree_relationship, Rel) :-
    component(Entity, has(utils(tree_builder)), utils(tree_builder(root(_), relationship(Rel), options(_)))).

component(Entity, utils_tree_max_depth, MaxDepth) :-
    component(Entity, has(utils(tree_builder)), utils(tree_builder(root(_), relationship(_), options(OptionList)))),
    member(max_depth(MaxDepth), OptionList).

% Validator generative expansion
component(Entity, utils_validation_rules, Rules) :-
    component(Entity, has(utils(validator)), utils(validator(rules(Rules), on_error(_)))).

component(Entity, utils_error_handling, ErrorMode) :-
    component(Entity, has(utils(validator)), utils(validator(rules(_), on_error(ErrorMode)))).

% Collection generative expansion
component(Entity, utils_collection_type, Type) :-
    component(Entity, has(utils(collection)), utils(collection(type(Type), operations(_), predicate(_)))).

component(Entity, utils_collection_operations, Operations) :-
    component(Entity, has(utils(collection)), utils(collection(type(_), operations(Operations), predicate(_)))).

component(Entity, utils_collection_predicate, Predicate) :-
    component(Entity, has(utils(collection)), utils(collection(type(_), operations(_), predicate(Predicate)))).

% === SPELL IMPLEMENTATIONS ===

% Entity hierarchy spell (perceive) - short form
register_spell(
    perceive(entity_hierarchy),
    input(entity_hierarchy(entity('RootEntity'))),
    output(either(ok(hierarchy(tree('Tree'))), error(hierarchy_error('Error')))),
    docstring("Build a hierarchical tree structure from entity child relationships.
Returns a tree structure where each node is: tree(Entity, [ChildTree1, ChildTree2, ...])
Only follows 'child' component relationships.")
).

cast(perceive(entity_hierarchy(RootEntity)), Result) :-
    catch(
        (build_entity_tree(RootEntity, Tree),
         Result = ok(hierarchy(Tree))),
        Error,
        Result = error(hierarchy_error(Error))
    ).

% Entity hierarchy spell (perceive) - utils namespace
register_spell(
    perceive(utils(entity_hierarchy)),
    input(utils(entity_hierarchy(entity('RootEntity')))),
    output(either(ok(hierarchy(tree('Tree'))), error(hierarchy_error('Error')))),
    docstring("Build a hierarchical tree structure from entity child relationships.
Returns a tree structure where each node is: tree(Entity, [ChildTree1, ChildTree2, ...])
Only follows 'child' component relationships.")
).

cast(perceive(utils(entity_hierarchy(RootEntity))), Result) :-
    catch(
        (build_entity_tree(RootEntity, Tree),
         Result = ok(hierarchy(Tree))),
        Error,
        Result = error(hierarchy_error(Error))
    ).

% File list spell (perceive)
register_spell(
    perceive(utils(file_list)),
    input(utils(file_list(directory('Directory'), filters('Filters')))),
    output(either(ok(file_list(list('Files'))), error(file_list_error('Error')))),
    docstring("List files in a directory matching specified filters.
Filters can include: extensions(['.pl', '.plt']), pattern('semantics.*')
Returns ok(file_list(Files)) or error(file_list_error(Reason)).")
).

cast(perceive(utils(file_list(Directory, Filters))), Result) :-
    catch(
        (list_files_with_filters(Directory, Filters, Files),
         Result = ok(file_list(Files))),
        Error,
        Result = error(file_list_error(Error))
    ).

% Validation spell (conjure)
register_spell(
    conjure(utils(validate)),
    input(utils(validate(entity('Entity'), rules('Rules')))),
    output(either(ok(validation_passed), error(validation_error('Reason')))),
    docstring("Validate an entity against a set of validation rules.
Available rules: check_existence, check_format, check_permissions.
Returns ok(validation_passed) on success or error(validation_error(Reason)) on failure.")
).

cast(conjure(utils(validate(Entity, Rules))), Result) :-
    catch((validate_entity_with_rules(Entity, Rules), Result = ok(validation_passed)),
          validation_error(_, Reason), Result = error(validation_error(Reason))).

% Transform data spell (conjure)
register_spell(
    conjure(utils(transform)),
    input(utils(transform(data('Data'), operation('Operation')))),
    output(either(ok(transformed('Result')), error(transform_error('Reason')))),
    docstring("Transform data using map, filter, or reduce operations.
Operations: map(Predicate), filter(Predicate), reduce(Predicate, Initial).
Returns ok(transformed(Result)) on success or error(transform_error(Reason)) on failure.")
).

cast(conjure(utils(transform(Data, Operation))), Result) :-
    catch((apply_transformation(Data, Operation, TransformedData), Result = ok(transformed(TransformedData))),
          Error, Result = error(transform_error(Error))).

% === CORE UTILITY PREDICATES ===

% Build tree recursively from child components only
% Tree structure: tree(Entity, Children) - no docstrings to keep it lean
build_entity_tree(Entity, tree(Entity, Children)) :-
    findall(ChildTree, (
        component(Entity, child, Child),
        build_entity_tree(Child, ChildTree)
    ), Children).

% List files with filters (placeholder implementation)
list_files_with_filters(Directory, Filters, Files) :-
    % Basic implementation - can be extended
    exists_directory(Directory),
    directory_files(Directory, AllFiles),
    include(matches_filters(Filters), AllFiles, Files).

matches_filters(Filters, File) :-
    forall(member(Filter, Filters), apply_file_filter(Filter, File)).

apply_file_filter(extensions(Exts), File) :-
    member(Ext, Exts),
    sub_atom(File, _, _, 0, Ext).

apply_file_filter(pattern(Pattern), File) :-
    sub_atom(File, _, _, _, Pattern).

% Validation helper implementations
validate_entity_with_rules(Entity, Rules) :-
    forall(member(Rule, Rules), apply_validation_rule(Entity, Rule)).

apply_validation_rule(Entity, check_existence) :-
    (entity(Entity) -> true ; throw(validation_error(utils, entity_does_not_exist(Entity)))).

apply_validation_rule(Entity, check_format) :-
    (atom(Entity), atom_length(Entity, Length), Length > 0 -> true ;
     throw(validation_error(utils, invalid_entity_format(Entity)))).

apply_validation_rule(_Entity, check_permissions) :-
    % Placeholder - domains can extend this for their specific permission checks
    true.

% Transformation operations
apply_transformation(Data, map(Predicate), Result) :-
    maplist(Predicate, Data, Result).

apply_transformation(Data, filter(Predicate), Result) :-
    include(Predicate, Data, Result).

apply_transformation(Data, reduce(Predicate, Initial), Result) :-
    foldl(Predicate, Data, Initial, Result).

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

% === ENTITY-SPECIFIC DOCSTRINGS ===
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