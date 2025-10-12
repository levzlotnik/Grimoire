% Test project with invalid type (for negative testing)
:- self_entity(test_project_invalid_type).

component(test_project_invalid_type, has(project(app)), project(app([
    type(invalid_type),
    git(repository(origin('https://github.com/test/invalid.git')))
]))).

docstring(test_project_invalid_type, "Test project with invalid type for negative testing").
