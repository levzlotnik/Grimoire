% Test project with build context
:- self_entity(test_project_with_context).

component(test_project_with_context, has(project(app)), project(app([
    type(web_service),
    git(repository(origin('https://github.com/test/context-app.git'))),
    nix(flake(ref('.')))
]))).

component(test_project_with_context, has(project(context(build))), project(context(build([
    nix(develop(shell('default'))),
    outputs(['dist/'])
])))).

docstring(test_project_with_context, "Test project with build context configuration").
