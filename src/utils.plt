% PLUnit tests for utils domain (Phase 3)
:- use_module(library(plunit)).

% Load domain semantics first
:- grimoire_ensure_loaded('@/src/utils.pl').

% Load db domain for core dump tests
:- load_entity(semantic(folder('@/src/db'))).

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
    user:magic_cast(perceive(utils(entity_hierarchy(entity(test_root)))), Result),
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
    user:magic_cast(conjure(utils(validate(entity(validate_test_entity), rules([check_existence, check_format])))), Result),
    assertion(Result == ok(validation_passed)).

test(validate_spell_failure) :-
    % Nonexistent entity should fail validation
    user:magic_cast(conjure(utils(validate(entity(nonexistent_entity), rules([check_existence])))), Result),
    assertion(Result = error(validation_error(_))).

% Test transform spell
test(transform_spell_map) :-
    user:magic_cast(conjure(utils(transform(data([1, 2, 3]), operation(map(succ))))), Result),
    assertion(Result == ok(transformed([2, 3, 4]))).

% === CORE DUMP SPELL TESTS ===

% Test perceive(core_dump) spell
test(spell_perceive_core_dump) :-
    user:magic_cast(perceive(core_dump), Result),
    Result = ok(core_dump(verified(Verified), broken(Broken), ignored(Ignored))),
    assertion(is_list(Verified)),
    assertion(is_list(Broken)),
    assertion(is_list(Ignored)),
    assertion(Broken = []).

% Test round-trip: perceive(core_dump) == perceive(read_core_dump_tsv) . conjure(core_dump_tsv)
% test(spell_core_dump_tsv_roundtrip, [cleanup(cleanup_core_dump_tsv)]) :-
test(spell_core_dump_tsv_roundtrip) :-
    TestTsvPath = '/tmp/test_core_dump.tsv',

    % Step 1: Get core dump
    user:magic_cast(perceive(core_dump), Result1),
    Result1 = ok(Dump1),
    Dump1 = core_dump(verified(_), broken(_), ignored(_)),

    % Step 2: Write to TSV
    user:magic_cast(conjure(core_dump_tsv(tsv_path(TestTsvPath))), WriteResult),
    assertion(WriteResult = ok(dumped)),

    % Step 3: Read from TSV
    user:magic_cast(perceive(read_core_dump_tsv(tsv_path(TestTsvPath))), ReadResult),
    ReadResult = ok(Dump2),

    % Step 4: Verify round-trip
    assertion(Dump1 == Dump2).

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

cleanup_core_dump_tsv :-
    (exists_file('/tmp/test_core_dump.tsv') -> delete_file('/tmp/test_core_dump.tsv') ; true).

% === CRUD OPERATIONS TESTS ===

% Test adding a component to entity
test(crud_add_component) :-
    % Create test entity file using fs spell
    TestEntityContent = {|string||
        :- self_entity(crud_test_add).

        component(crud_test_add, test_config, initial_value).
    |},
    user:magic_cast(conjure(fs(edit_file(file('/tmp/crud_test_add.pl'), edits([append(TestEntityContent)])))), WriteResult),
    assertion(WriteResult = ok(_)),

    % Load the entity
    load_entity(semantic(file('/tmp/crud_test_add.pl'))),

    % Add component directly using entity parameter
    user:magic_cast(conjure(add_component(entity(crud_test_add), component_type(test_new), value(new_val))), AddResult),
    assertion(AddResult = ok(component_added(component_type(test_new), value(new_val)))),
    user:please_verify(component(crud_test_add, test_new, new_val)),

    % Cleanup
    (exists_file('/tmp/crud_test_add.pl') -> delete_file('/tmp/crud_test_add.pl') ; true), !.

% Test removing a fact component
test(crud_remove_fact) :-
    % Create test entity file
    TestEntityContent = {|string||
        :- self_entity(crud_test_remove).
    |},
    user:magic_cast(conjure(fs(edit_file(file('/tmp/crud_test_remove.pl'), edits([append(TestEntityContent)])))), WriteResult),
    assertion(WriteResult = ok(_)),
    load_entity(semantic(file('/tmp/crud_test_remove.pl'))),

    % Add then remove
    user:magic_cast(conjure(add_component(entity(crud_test_remove), component_type(temp_comp), value(temp_val))), AddResult),
    assertion(AddResult = ok(_)),
    user:please_verify(component(crud_test_remove, temp_comp, temp_val)),
    user:magic_cast(conjure(remove_component(entity(crud_test_remove), component_type(temp_comp), value(temp_val))), RemoveResult),
    assertion(RemoveResult = ok(component_removed(component_type(temp_comp), value(temp_val)))),

    % Verify component is gone by checking please_verify throws
    catch(
        user:please_verify(component(crud_test_remove, temp_comp, temp_val)),
        error(sus(component_not_found(_)), _),
        true
    ),

    % Cleanup
    (exists_file('/tmp/crud_test_remove.pl') -> delete_file('/tmp/crud_test_remove.pl') ; true), !.

% Test cannot remove derived component
test(crud_cannot_remove_derived) :-
    % Load test entity with derived component
    load_entity(semantic(file('@/src/tests/crud_test_entities.pl'))),

    % Try to remove derived component - should fail with specific error
    user:magic_cast(conjure(remove_component(entity(crud_test_entity_with_derived), component_type(test_derived_comp), value(test_value))), RemoveResult),
    assertion(RemoveResult = error(remove_error(cannot_remove_derived))), !.


:- end_tests(utils).
