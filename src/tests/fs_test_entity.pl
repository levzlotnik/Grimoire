% Test entity for fs domain tests
% This file contains declarative entity/component definitions for testing

:- self_entity(test_entity(fs)).

% Sub-entities for different test scenarios
entity(mock_entity).
entity(content_test_entity).
entity(perm_test_entity).
entity(fail_entity).
entity(content_fail_entity).

% Main test entity with fs(structure) component
component(test_entity(fs), has(fs(structure)), fs(structure([
    file('test_file.txt'),
    folder('test_dir', [file('nested_file.txt')])
]))).

% Content test entity
component(content_test_entity, has(fs(file_content)), fs(file_content(
    'content_test.txt', contains(["hello", "world"])
))).

% Permissions test entity
component(perm_test_entity, has(fs(permissions)), fs(permissions(
    'test_script.sh', executable
))).

% Mock entity for verify test
component(mock_entity, fs_test_prop, test_value).

% Failure test entities (for negative test cases)
component(fail_entity, has(fs(structure)), fs(structure([
    file('nonexistent.txt')
]))).

component(content_fail_entity, has(fs(file_content)), fs(file_content(
    'content_test.txt', contains(["missing"])
))).

docstring(test_entity(fs), "Test entity container for fs domain verification tests").
