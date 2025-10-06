% Test entities for utils domain tests
% This file contains declarative entity/component definitions for testing

:- self_entity(test_entity(utils)).

% Declare test entities for different test scenarios
entity(test_entity(utils_file_processor)).
entity(test_entity(utils_tree_builder)).
entity(test_entity(utils_validator)).
entity(test_entity(utils_collection)).
entity(test_root).

% File processor test entity
component(test_entity(utils_file_processor), has(utils(file_processor)),
    utils(file_processor(type(semantics), filters([extensions(['.pl', '.plt'])])))).

% Tree builder test entity
component(test_entity(utils_tree_builder), has(utils(tree_builder)),
    utils(tree_builder(root(test_root), relationship(child), options([max_depth(10)])))).

% Validator test entity
component(test_entity(utils_validator), has(utils(validator)),
    utils(validator(rules([check_existence, check_format]), on_error(throw)))).

% Collection test entity
component(test_entity(utils_collection), has(utils(collection)),
    utils(collection(type(entities), operations([filter, map]), predicate(is_atom/1)))).

docstring(test_entity(utils), "Test entity container for utils domain verification tests").
docstring(test_entity(utils_file_processor), "Test entity for utils file processor verification").
docstring(test_entity(utils_tree_builder), "Test entity for utils tree builder verification").
docstring(test_entity(utils_validator), "Test entity for utils validator verification").
docstring(test_entity(utils_collection), "Test entity for utils collection verification").
