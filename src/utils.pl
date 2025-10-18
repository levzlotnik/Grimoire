% Utils entity - General utilities for the Grimoire system
:- self_entity(utils).

% === ENTITY DECLARATIONS ===
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
% Tree building utilities (pure ECS traversal, no I/O)
component(user_entity, has(utils(tree_builder)), utils(tree_builder(
    root(user_entity), relationship(child), options([max_depth(10)])
))).
% Generatively populates: component(user_entity, utils_tree_root, user_entity).

% Validation utilities (pure logic, no I/O)
component(user_entity, has(utils(validator)), utils(validator(
    rules([check_existence, check_format]), on_error(throw)
))).
% Generatively populates: component(user_entity, utils_validation_rules, [check_existence, check_format]).

% Collection utilities (pure data transformation, no I/O)
component(user_entity, has(utils(collection)), utils(collection(
    type(entities), operations([filter, map, reduce]), predicate(custom_filter/2)
))).
% Generatively populates: component(user_entity, utils_collection_type, entities).
```

## Verification Patterns
Users can use `please_verify/1` on fact schemas in their own `semantics.plt` files:

```prolog
please_verify(component(user_entity, has(utils(tree_builder)),
    utils(tree_builder(root(user_entity), relationship(child), options([max_depth(10)]))))).
please_verify(component(user_entity, has(utils(validator)),
    utils(validator(rules([check_existence, check_format]), on_error(throw))))).
```

## Purpose
Utils domain provides pure logic utility functions for other Grimoire domains:
- Entity hierarchy traversal and tree building (pure ECS, no I/O)
- Validation framework for component verification (pure logic)
- Collection operations (map, filter, reduce) on entity data (pure transformation)

Note: File processing utilities have been moved to the fs domain.

## Spell Constructors
- **perceive**: `entity_hierarchy(RootEntity)` - Build hierarchical tree from relationships
- **perceive**: `utils(entity_hierarchy(RootEntity))` - Build hierarchical tree from relationships
- **conjure**: `utils(validate(Entity, Rules))` - Validate entity against rules
- **conjure**: `utils(transform(Data, Operation))` - Transform data using map/filter/reduce

## Dependencies
- **ecs_kernel** (L0): Core ECS predicates and verification primitive
- **grimoire** (L1): Spell system (for magic_cast in spell implementations)
").

% === DSL SCHEMA REGISTRATIONS ===

% Tree builder schema - pure ECS traversal
register_dsl_schema(
    utils,
    has(utils(tree_builder)),
    signature(utils(tree_builder(root('Root'), relationship('Rel'), options('Options')))),
    "Build hierarchical tree from entity relationships (pure ECS traversal, no I/O)",
    (
        component(E, has(utils(tree_builder)), utils(tree_builder(root(Root), relationship(Rel), options(Options))))
            ==> component(E, utils_tree_root, Root),
                component(E, utils_tree_relationship, Rel),
                (component(E, utils_tree_max_depth, MD) :- member(max_depth(MD), Options))
            ::  ground(Root),
                atom(Rel),
                is_list(Options)
    )
).

% Validator schema - pure logic validation
register_dsl_schema(
    utils,
    has(utils(validator)),
    signature(utils(validator(rules('Rules'), on_error('ErrorMode')))),
    "Validate entity against rules with configurable error handling (pure logic, no I/O)",
    (
        component(E, has(utils(validator)), utils(validator(rules(Rules), on_error(ErrorMode))))
            ==> component(E, utils_validation_rules, Rules),
                component(E, utils_error_handling, ErrorMode)
            ::  is_list(Rules),
                atom(ErrorMode)
    )
).

% Collection schema - pure data transformation
register_dsl_schema(
    utils,
    has(utils(collection)),
    signature(utils(collection(type('Type'), operations('Operations'), predicate('Predicate')))),
    "Transform collections using map/filter/reduce operations (pure data transformation, no I/O)",
    (
        component(E, has(utils(collection)), utils(collection(type(Type), operations(Operations), predicate(Predicate))))
            ==> component(E, utils_collection_type, Type),
                component(E, utils_collection_operations, Operations),
                component(E, utils_collection_predicate, Predicate)
            ::  atom(Type),
                is_list(Operations),
                callable(Predicate)
    )
).

% === LEAF VERIFICATIONS ===

component(_, utils_tree_root, Root)
    :: ground(Root).

component(_, utils_tree_relationship, Rel)
    :: atom(Rel).

component(_, utils_tree_max_depth, MaxDepth)
    :: integer(MaxDepth),
       MaxDepth > 0.

component(_, utils_validation_rules, Rules)
    :: is_list(Rules),
       subset(Rules, [check_existence, check_format, check_permissions]).

component(_, utils_error_handling, ErrorMode)
    :: member(ErrorMode, [throw, return, log]).

component(_, utils_collection_type, Type)
    :: atom(Type).

component(_, utils_collection_operations, Operations)
    :: is_list(Operations),
       subset(Operations, [filter, map, reduce]).

component(_, utils_collection_predicate, Predicate)
    :: callable(Predicate).

% === SPELL IMPLEMENTATIONS ===

% Entity hierarchy spell (perceive) - utils namespace only
% Note: Short form perceive(entity_hierarchy) removed due to grimoire.pl term_expansion
% limitations with atomic spell domains. Use perceive(utils(entity_hierarchy(...))) instead.
register_spell(
    perceive(utils(entity_hierarchy)),
    input(utils(entity_hierarchy(entity('RootEntity')))),
    output(either(ok(hierarchy(tree('Tree'))), error(hierarchy_error('Error')))),
    "Build hierarchical tree structure from entity child relationships (pure ECS, no I/O)",
    [],
    implementation(perceive(utils(entity_hierarchy(RootEntity))), Result, (
        catch(
            (build_entity_tree(RootEntity, Tree),
             Result = ok(hierarchy(Tree))),
            Error,
            Result = error(hierarchy_error(Error))
        )
    ))
).

% Validation spell (conjure)
register_spell(
    conjure(utils(validate)),
    input(utils(validate(entity('Entity'), rules('Rules')))),
    output(either(ok(validation_passed), error(validation_error('Reason')))),
    "Validate entity against rules (pure logic, no I/O)",
    [],
    implementation(conjure(utils(validate(Entity, Rules))), Result, (
        catch(
            (validate_entity_with_rules(Entity, Rules),
             Result = ok(validation_passed)),
            validation_error(_, Reason),
            Result = error(validation_error(Reason))
        )
    ))
).

% Transform data spell (conjure)
register_spell(
    conjure(utils(transform)),
    input(utils(transform(data('Data'), operation('Operation')))),
    output(either(ok(transformed('Result')), error(transform_error('Reason')))),
    "Transform data using map/filter/reduce (pure logic, no I/O)",
    [],
    implementation(conjure(utils(transform(Data, Operation))), Result, (
        catch(
            (apply_transformation(Data, Operation, TransformedData),
             Result = ok(transformed(TransformedData))),
            Error,
            Result = error(transform_error(Error))
        )
    ))
).

% === CORE UTILITY PREDICATES ===

% Build tree recursively from child components only
% Tree structure: tree(Entity, Children) - no docstrings to keep it lean
build_entity_tree(Entity, tree(Entity, Children)) :-
    findall(ChildTree, (
        component(Entity, child, Child),
        build_entity_tree(Child, ChildTree)
    ), Children).

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

% === ENTITY-SPECIFIC DOCSTRINGS ===
docstring(entity_hierarchy, "Build a hierarchical tree structure from entity child relationships.
    Format: perceive(entity_hierarchy(RootEntity))

    Returns a tree structure where each node is:
    tree(Entity, [ChildTree1, ChildTree2, ...])

    Only follows 'child' component relationships. Other domains can subscribe
    their components to the child pattern using rules like:
    component(Entity, child, Child) :- component(Entity, ctor, Child).

    Example:
    ?- magic_cast(perceive(entity_hierarchy(project)), Result).
    Result = ok(hierarchy(tree(project, [
        tree(project(frontend), [
            tree(project(frontend(src)), [...])
        ]),
        tree(project(backend), [])
    ]))).").
