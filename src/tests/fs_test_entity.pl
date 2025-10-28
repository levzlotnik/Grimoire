% Test entity for fs domain tests
% This file contains declarative entity/component definitions for testing

:- self_entity(test_entity(fs)).

% Sub-entities for different test scenarios
entity(mock_entity).
entity(perm_test_entity).
entity(fail_entity).

% Main test entity with fs(structure) component
component(test_entity(fs), has(fs(structure)), fs(structure([
    file('test_file.txt'),
    folder('test_dir', [file('nested_file.txt')])
]))).
component(test_entity(fs), core_dump_ignorelist, [fs_structure_file, fs_structure_folder, has(fs(structure))]).

% Permissions test entity
component(perm_test_entity, has(fs(permissions)), fs(permissions(
    'test_script.sh', executable
))).
component(perm_test_entity, core_dump_ignorelist, [fs_permission_requirement, has(fs(permissions))]).

% Mock entity for verify test
component(mock_entity, fs_test_prop, test_value).

% Failure test entities (for negative test cases)
component(fail_entity, has(fs(structure)), fs(structure([
    file('nonexistent.txt')
]))).
component(fail_entity, core_dump_ignorelist, [fs_structure_file, has(fs(structure))]).

docstring(test_entity(fs), "Test entity container for fs domain verification tests").
