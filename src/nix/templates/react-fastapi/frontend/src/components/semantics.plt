% PLUnit tests for Components semantics

:- begin_tests(components_semantics).

:- ensure_loaded(semantics).

test(components_entity_declared) :-
    entity(react_fastapi_template(frontend(src(components)))).

test(components_have_docstring) :-
    docstring(react_fastapi_template(frontend(src(components))), _).

test(component_files_exist) :-
    entity(react_fastapi_template(frontend(src(components(source(file('./Navigation.tsx'))))))),
    entity(react_fastapi_template(frontend(src(components(source(file('./DeviceDistributionPieChart.tsx'))))))),
    entity(react_fastapi_template(frontend(src(components(source(file('./StatCard.tsx'))))))).

test(components_have_docstrings) :-
    docstring(react_fastapi_template(frontend(src(components(source(file('./Navigation.tsx')))))), _),
    docstring(react_fastapi_template(frontend(src(components(source(file('./DeviceDistributionPieChart.tsx')))))), _).

test(component_categories_defined) :-
    component(react_fastapi_template(frontend(src(components))), category, navigation),
    component(react_fastapi_template(frontend(src(components))), category, charts).

test(actual_component_files_exist) :-
    exists_file('Navigation.tsx'),
    exists_file('DeviceDistributionPieChart.tsx'),
    exists_file('StatCard.tsx').

:- end_tests(components_semantics).