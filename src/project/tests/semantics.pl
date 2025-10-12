% Test entities for project domain tests
% Load test entities from subdirectories

:- load_entity(semantic(folder('./test_web_app'))).
:- load_entity(semantic(folder('./test_cli_tool'))).
:- load_entity(semantic(folder('./test_library'))).
:- load_entity(semantic(folder('./test_project_with_context'))).
:- load_entity(semantic(folder('./test_project_no_git'))).
:- load_entity(semantic(folder('./test_project_invalid_type'))).

% Test project for mkproject spell (filesystem-based testing)
% Path will be used in setup/cleanup
component(test_mkproject_target, test_path, '/tmp/grimoire_test_project_create').
component(test_mkproject_magic_target, test_path, '/tmp/grimoire_test_magic_project').
