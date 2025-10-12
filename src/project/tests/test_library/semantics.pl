% Test library project
:- self_entity(test_library).

component(test_library, has(project(app)), project(app([
    type(library),
    git(repository(origin('https://github.com/test/library.git'))),
    nix(flake(ref('.')))
]))).

docstring(test_library, "Test library project").
