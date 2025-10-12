% Test project without git (for negative testing)
:- self_entity(test_project_no_git).

component(test_project_no_git, has(project(app)), project(app([
    type(cli_tool),
    nix(flake(ref('.')))
]))).

docstring(test_project_no_git, "Test project without git repository").
