% Test entities for project domain tests
% This file contains declarative entity/component definitions for testing

:- self_entity(test_entity(project)).

% Test web application project
entity(test_web_app).

component(test_web_app, has(project(app)), project(app([
    type(web_service),
    git(repository(origin('https://github.com/test/web-app.git'))),
    nix(flake(ref('.'))),
    fs(structure([sources(['src/**/*.py']), configs(['*.toml'])]))
]))).

% Test CLI tool project
entity(test_cli_tool).

component(test_cli_tool, has(project(app)), project(app([
    type(cli_tool),
    git(repository(origin('https://github.com/test/cli-tool.git'))),
    nix(flake(ref('.')))
]))).

% Test library project
entity(test_library).

component(test_library, has(project(app)), project(app([
    type(library),
    git(repository(origin('https://github.com/test/library.git'))),
    nix(flake(ref('.')))
]))).

% Test project with build context
entity(test_project_with_context).

component(test_project_with_context, has(project(app)), project(app([
    type(web_service),
    git(repository(origin('https://github.com/test/context-app.git'))),
    nix(flake(ref('.')))
]))).

component(test_project_with_context, has(project(context(build))), project(context(build([
    nix(develop(shell('default'))),
    outputs(['dist/'])
])))).

% Test project without git (for negative testing)
entity(test_project_no_git).

component(test_project_no_git, has(project(app)), project(app([
    type(cli_tool),
    nix(flake(ref('.')))
]))).

% Test project with invalid type (for negative testing)
entity(test_project_invalid_type).

component(test_project_invalid_type, has(project(app)), project(app([
    type(invalid_type),
    git(repository(origin('https://github.com/test/invalid.git')))
]))).

% Test project for mkproject spell (filesystem-based testing)
% Path will be used in setup/cleanup
component(test_mkproject_target, test_path, '/tmp/grimoire_test_project_create').
component(test_mkproject_magic_target, test_path, '/tmp/grimoire_test_magic_project').

docstring(test_entity(project), "Test entity container for project domain verification tests").
docstring(test_web_app, "Test web service project with full configuration").
docstring(test_cli_tool, "Test CLI tool project").
docstring(test_library, "Test library project").
docstring(test_project_with_context, "Test project with build context configuration").
docstring(test_project_no_git, "Test project without git repository").
docstring(test_project_invalid_type, "Test project with invalid type for negative testing").
