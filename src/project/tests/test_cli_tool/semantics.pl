% Test CLI tool project
:- self_entity(test_cli_tool).

component(test_cli_tool, has(project(app)), project(app([
    type(cli_tool),
    git(repository(origin('https://github.com/test/cli-tool.git'))),
    nix(flake(ref('.')))
]))).

docstring(test_cli_tool, "Test CLI tool project").
