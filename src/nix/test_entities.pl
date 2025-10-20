% Test entities for nix domain
:- self_entity(test_entities(nix)).

% Basic test entity with flake reference
entity(test_nix_entity).
component(test_nix_entity, has(nix(flake)), nix(flake(ref('./src/nix/test_flake')))).

docstring(test_nix_entity, "Test entity with nix flake reference for testing DSL expansion").

% Build target test entity
entity(test_build_entity).
component(test_build_entity, has(nix(build_target)), nix(build_target('.#default'))).

docstring(test_build_entity, "Test entity with build target for testing build target DSL").

% Dev environment test entity
entity(test_dev_entity).
component(test_dev_entity, has(nix(dev_env)), nix(dev_env(shell('default')))).

docstring(test_dev_entity, "Test entity with dev environment for testing dev env DSL").

% Flake ref test entities
entity(test_flake_ref).
component(test_flake_ref, nix_flake_ref, './src/nix/test_flake').

entity(test_bad_flake).
component(test_bad_flake, nix_flake_ref, '').

docstring(test_flake_ref, "Test entity with valid flake ref").
docstring(test_bad_flake, "Test entity with invalid flake ref").

% Build target path test entities
entity(test_build).
component(test_build, nix_flake_ref, './src/nix/test_flake').
component(test_build, nix_build_target_path, 'hello').

entity(test_bad_build).
component(test_bad_build, nix_flake_ref, './src/nix/test_flake').
component(test_bad_build, nix_build_target_path, 'nonexistent_target_xyz').

docstring(test_build, "Test entity with valid build target").
docstring(test_bad_build, "Test entity with invalid build target").

% Dev shell test entity
entity(test_shell).
component(test_shell, nix_flake_ref, './src/nix/test_flake').
component(test_shell, nix_dev_env_shell, 'default').

docstring(test_shell, "Test entity with valid dev shell").
