% PLUnit tests for utils domain (Phase 3)
:- use_module(library(plunit)).

% Load domain semantics first
:- grimoire_ensure_loaded('@/src/utils.pl').

% Load test entities from file-based knowledge
:- load_entity(semantic(file('@/src/tests/utils_test_entities.pl'))).

% === PLUNIT TESTS ===

:- begin_tests(utils).

% Test tree builder DSL pattern
test(utils_tree_builder_verification, [setup(setup_tree_builder), cleanup(cleanup_tree_builder)]) :-
    % Verify using please_verify - DSL schema generates verify clause automatically
    user:please_verify(component(test_entity(utils_tree_builder), has(utils(tree_builder)),
        utils(tree_builder(root(test_root), relationship(child), options([max_depth(10)]))))),
    % Verify generative expansion worked
    user:please_verify(component(test_entity(utils_tree_builder), utils_tree_root, test_root)),
    user:please_verify(component(test_entity(utils_tree_builder), utils_tree_relationship, child)),
    user:please_verify(component(test_entity(utils_tree_builder), utils_tree_max_depth, 10)), !.

% Test validator DSL pattern
test(utils_validator_verification, [setup(setup_validator), cleanup(cleanup_validator)]) :-
    % Verify using please_verify
    user:please_verify(component(test_entity(utils_validator), has(utils(validator)),
        utils(validator(rules([check_existence, check_format]), on_error(throw))))),
    % Verify generative expansion worked
    user:please_verify(component(test_entity(utils_validator), utils_validation_rules, [check_existence, check_format])),
    user:please_verify(component(test_entity(utils_validator), utils_error_handling, throw)), !.

% Test collection DSL pattern
test(utils_collection_verification, [setup(setup_collection), cleanup(cleanup_collection)]) :-
    % Verify using please_verify
    user:please_verify(component(test_entity(utils_collection), has(utils(collection)),
        utils(collection(type(entities), operations([filter, map]), predicate(is_atom/1))))),
    % Verify generative expansion worked
    user:please_verify(component(test_entity(utils_collection), utils_collection_type, entities)),
    user:please_verify(component(test_entity(utils_collection), utils_collection_operations, [filter, map])), !.

% === SPELL IMPLEMENTATION TESTS ===

% Test entity hierarchy building
test(simple_hierarchy, [setup(setup_hierarchy), cleanup(cleanup_hierarchy)]) :-
    % Build hierarchy
    user:magic_cast(perceive(utils(entity_hierarchy(test_root))), Result),
    assertion(Result = ok(hierarchy(Tree))),

    % Verify structure (deterministic checks)
    Tree = tree(test_root, Children),
    length(Children, 2),
    assertion(member(tree(test_child1, GrandChildren), Children)),
    assertion(member(tree(test_child2, []), Children)),
    length(GrandChildren, 1),
    assertion(member(tree(test_grandchild, []), GrandChildren)).

% Note: Short form test removed - perceive(entity_hierarchy) not supported
% due to grimoire.pl term_expansion limitations with atomic spell domains.

% Test validation spell
test(validate_spell_success, [setup(setup_validate_entity), cleanup(cleanup_validate_entity)]) :-
    % Entity exists and should validate
    user:magic_cast(conjure(utils(validate(validate_test_entity, [check_existence, check_format]))), Result),
    assertion(Result == ok(validation_passed)).

test(validate_spell_failure) :-
    % Nonexistent entity should fail validation
    user:magic_cast(conjure(utils(validate(nonexistent_entity, [check_existence]))), Result),
    assertion(Result = error(validation_error(_))).

% Test transform spell
test(transform_spell_map) :-
    user:magic_cast(conjure(utils(transform([1, 2, 3], map(succ)))), Result),
    assertion(Result == ok(transformed([2, 3, 4]))).

% === SETUP/CLEANUP HELPERS ===

setup_tree_builder :-
    % Entity already loaded from file
    true.

cleanup_tree_builder :-
    % No cleanup needed
    true.

setup_validator :-
    % Entity already loaded from file
    true.

cleanup_validator :-
    % No cleanup needed
    true.

setup_collection :-
    % Entity already loaded from file
    true.

cleanup_collection :-
    % No cleanup needed
    true.

setup_validate_entity :-
    % Entity already loaded from file
    true.

cleanup_validate_entity :-
    % No cleanup needed
    true.

setup_hierarchy :-
    % Entities already loaded from file
    true.

cleanup_hierarchy :-
    % No cleanup needed
    true.

:- end_tests(utils).
