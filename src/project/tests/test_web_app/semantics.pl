% Test web application project
:- self_entity(test_web_app).

component(test_web_app, has(project(app)), project(app([
    type(web_service),
    git(repository(origin('https://github.com/test/web-app.git'))),
    nix(flake(ref('.'))),
    fs(structure([
        glob('src/**/*.py'),
        glob('*.toml')
    ]))
]))).

docstring(test_web_app, "Test web service project with full configuration").
