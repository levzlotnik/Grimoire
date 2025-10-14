% PLUnit tests for utils entity
:- use_module(library(plunit)).

% Load test entities from file-based knowledge
:- load_entity(semantic(file('@/src/tests/utils_test_entities.pl'))).

% === DISCRIMINATIVE FLOW: VERIFICATION IMPLEMENTATIONS ===
% This is the .plt (test/verification) side following dual ECS pattern

% === VERIFY/1 OVERLOADS FOR UTILS DSL PATTERNS ===

% File processor verification
verify(component(Entity, has(utils(file_processor)), utils(file_processor(type(Type), filters(Filters))))) :-
    % Already exists via please_verify, now verify semantics
    please_verify(component(Entity, utils_processor_type, Type)),
    verify_file_processor_config(Type, Filters).

% Tree builder verification
verify(component(Entity, has(utils(tree_builder)), utils(tree_builder(root(Root), relationship(Rel), options(Options))))) :-
    % Already exists via please_verify, now verify semantics
    please_verify(component(Entity, utils_tree_root, Root)),
    please_verify(component(Entity, utils_tree_relationship, Rel)),
    verify_tree_builder_config(Root, Rel, Options).

% Validator verification
verify(component(Entity, has(utils(validator)), utils(validator(rules(Rules), on_error(ErrorMode))))) :-
    % Already exists via please_verify, now verify semantics
    please_verify(component(Entity, utils_validation_rules, Rules)),
    please_verify(component(Entity, utils_error_handling, ErrorMode)),
    verify_validator_config(Rules, ErrorMode).

% Collection verification
verify(component(Entity, has(utils(collection)), utils(collection(type(Type), operations(Operations), predicate(Predicate))))) :-
    % Already exists via please_verify, now verify semantics
    please_verify(component(Entity, utils_collection_type, Type)),
    please_verify(component(Entity, utils_collection_operations, Operations)),
    verify_collection_config(Type, Operations, Predicate).

% === PRIMITIVE COMPONENT VERIFICATIONS ===

% Verify expanded component: utils_processor_type
verify(component(_Entity, utils_processor_type, Type)) :-
    % Already exists via please_verify
    member(Type, [semantics, general, custom]).

% Verify expanded component: utils_file_extensions
verify(component(_Entity, utils_file_extensions, Extensions)) :-
    % Already exists via please_verify
    is_list(Extensions),
    forall(member(Ext, Extensions), (atom(Ext), sub_atom(Ext, 0, 1, _, '.'))).

% Verify expanded component: utils_file_pattern
verify(component(_Entity, utils_file_pattern, Pattern)) :-
    % Already exists via please_verify
    atom(Pattern).

% Verify expanded component: utils_tree_root
verify(component(_Entity, utils_tree_root, Root)) :-
    % Already exists via please_verify
    entity(Root).

% Verify expanded component: utils_tree_relationship
verify(component(_Entity, utils_tree_relationship, Rel)) :-
    % Already exists via please_verify
    atom(Rel).

% Verify expanded component: utils_tree_max_depth
verify(component(_Entity, utils_tree_max_depth, MaxDepth)) :-
    % Already exists via please_verify
    integer(MaxDepth), MaxDepth > 0.

% Verify expanded component: utils_validation_rules
verify(component(_Entity, utils_validation_rules, Rules)) :-
    % Already exists via please_verify
    is_list(Rules),
    subset(Rules, [check_existence, check_format, check_permissions]).

% Verify expanded component: utils_error_handling
verify(component(_Entity, utils_error_handling, ErrorMode)) :-
    % Already exists via please_verify
    member(ErrorMode, [throw, return, log]).

% Verify expanded component: utils_collection_type
verify(component(_Entity, utils_collection_type, Type)) :-
    % Already exists via please_verify
    atom(Type).

% Verify expanded component: utils_collection_operations
verify(component(_Entity, utils_collection_operations, Operations)) :-
    % Already exists via please_verify
    is_list(Operations),
    subset(Operations, [filter, map, reduce]).

% Verify expanded component: utils_collection_predicate
verify(component(_Entity, utils_collection_predicate, Predicate)) :-
    % Already exists via please_verify
    callable(Predicate).

% === DOMAIN-SPECIFIC VERIFICATION PREDICATES ===

% Verify file processor configuration
verify_file_processor_config(Type, Filters) :-
    % Type must be valid
    member(Type, [semantics, general, custom]),
    % Filters must be a list
    is_list(Filters),
    % Each filter must be valid
    forall(member(Filter, Filters), verify_file_filter(Filter)).

verify_file_filter(extensions(Exts)) :-
    is_list(Exts),
    forall(member(Ext, Exts), (atom(Ext), sub_atom(Ext, 0, 1, _, '.'))).

verify_file_filter(pattern(Pat)) :-
    atom(Pat).

% Verify tree builder configuration
verify_tree_builder_config(Root, Rel, Options) :-
    % Root must be an entity
    entity(Root),
    % Relationship must be an atom
    atom(Rel),
    % Options must be a list
    is_list(Options),
    % Verify each option
    forall(member(Opt, Options), verify_tree_option(Opt)).

verify_tree_option(max_depth(D)) :-
    integer(D), D > 0.

% Verify validator configuration
verify_validator_config(Rules, ErrorMode) :-
    % Rules must be a list
    is_list(Rules),
    % All rules must be known
    subset(Rules, [check_existence, check_format, check_permissions]),
    % Error mode must be valid
    member(ErrorMode, [throw, return, log]).

% Verify collection configuration
verify_collection_config(Type, Operations, Predicate) :-
    % Type must be an atom
    atom(Type),
    % Operations must be a list
    is_list(Operations),
    % All operations must be known
    subset(Operations, [filter, map, reduce]),
    % Predicate must be a callable term
    callable(Predicate).

% === PLUNIT TESTS ===

:- begin_tests(utils).

% Test utils entity exists
test(entity_exists) :-
    user:please_verify(component(utils, defined, true)), !.

% Debug tests removed - no longer needed

% === DSL PATTERN VERIFICATION TESTS ===

% Test file processor DSL pattern
test(utils_file_processor_verification, [setup(setup_file_processor), cleanup(cleanup_file_processor)]) :-
    % Verify it using please_verify
    user:please_verify(component(test_entity(utils_file_processor), has(utils(file_processor)),
        utils(file_processor(type(semantics), filters([extensions(['.pl', '.plt'])]))))),
    % Verify generative expansion worked
    user:please_verify(component(test_entity(utils_file_processor), utils_processor_type, semantics)),
    user:please_verify(component(test_entity(utils_file_processor), utils_file_extensions, ['.pl', '.plt'])), !.

% Test tree builder DSL pattern
test(utils_tree_builder_verification, [setup(setup_tree_builder), cleanup(cleanup_tree_builder)]) :-
    % Verify it using please_verify
    user:please_verify(component(test_entity(utils_tree_builder), has(utils(tree_builder)),
        utils(tree_builder(root(test_root), relationship(child), options([max_depth(10)]))))),
    % Verify generative expansion worked
    user:please_verify(component(test_entity(utils_tree_builder), utils_tree_root, test_root)),
    user:please_verify(component(test_entity(utils_tree_builder), utils_tree_relationship, child)),
    user:please_verify(component(test_entity(utils_tree_builder), utils_tree_max_depth, 10)), !.

% Test validator DSL pattern
test(utils_validator_verification, [setup(setup_validator), cleanup(cleanup_validator)]) :-
    % Verify it using please_verify
    user:please_verify(component(test_entity(utils_validator), has(utils(validator)),
        utils(validator(rules([check_existence, check_format]), on_error(throw))))),
    % Verify generative expansion worked
    user:please_verify(component(test_entity(utils_validator), utils_validation_rules, [check_existence, check_format])),
    user:please_verify(component(test_entity(utils_validator), utils_error_handling, throw)), !.

% Test collection DSL pattern
test(utils_collection_verification, [setup(setup_collection), cleanup(cleanup_collection)]) :-
    % Verify it using please_verify
    user:please_verify(component(test_entity(utils_collection), has(utils(collection)),
        utils(collection(type(entities), operations([filter, map]), predicate(is_atom/1))))),
    % Verify generative expansion worked
    user:please_verify(component(test_entity(utils_collection), utils_collection_type, entities)),
    user:please_verify(component(test_entity(utils_collection), utils_collection_operations, [filter, map])), !.

% === SPELL CONSTRUCTOR TESTS ===

% Test spell format registrations exist
test(spell_format_registrations) :-
    register_spell(perceive(entity_hierarchy), input(_), output(_), _),
    register_spell(perceive(utils(entity_hierarchy)), input(_), output(_), _),
    register_spell(perceive(utils(file_list)), input(_), output(_), _),
    register_spell(conjure(utils(validate)), input(_), output(_), _),
    register_spell(conjure(utils(transform)), input(_), output(_), _), !.

% === SPELL IMPLEMENTATION TESTS ===

% Test entity hierarchy building
test(simple_hierarchy, [setup(setup_hierarchy), cleanup(cleanup_hierarchy)]) :-
    % Build hierarchy
    user:magic_cast(perceive(utils(entity_hierarchy(test_root))), Result),
    assertion(Result = ok(hierarchy(Tree))),

    % Verify structure
    Tree = tree(test_root, Children),
    length(Children, 2),
    member(tree(test_child1, GrandChildren), Children),
    member(tree(test_child2, []), Children),
    length(GrandChildren, 1),
    member(tree(test_grandchild, []), GrandChildren), !.

% Test validation spell
test(validate_spell_success, [setup(setup_validate_entity), cleanup(cleanup_validate_entity)]) :-
    % Entity exists and should validate
    user:magic_cast(conjure(utils(validate(validate_test_entity, [check_existence, check_format]))), Result),
    assertion(Result == ok(validation_passed)), !.

test(validate_spell_failure) :-
    % Nonexistent entity should fail validation
    user:magic_cast(conjure(utils(validate(nonexistent_entity, [check_existence]))), Result),
    assertion(Result = error(validation_error(_))), !.

% Test transform spell
test(transform_spell_map) :-
    user:magic_cast(conjure(utils(transform([1, 2, 3], map(succ)))), Result),
    assertion(Result == ok(transformed([2, 3, 4]))), !.

% === UTILITY FUNCTION TESTS ===

% Test is_semantics_file predicate
test(is_semantics_file_pl) :-
    is_semantics_file('some/path/semantics.pl'), !.

test(is_semantics_file_plt) :-
    is_semantics_file('test/semantics.plt').

test(not_semantics_file) :-
    \+ is_semantics_file('some/other/file.txt').

% Test semantic_entity_id integration
test(semantic_entity_id_integration) :-
    % This tests that we can find entities by their semantic source
    % The utils entity itself should be findable
    semantic_entity_id(semantic(file(_)), utils), !.

% === SETUP/CLEANUP HELPERS ===

setup_file_processor :-
    % Entity already loaded from file
    true.

cleanup_file_processor :-
    % No cleanup needed
    true.

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