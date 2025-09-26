% PLUnit tests for Types semantics

:- begin_tests(types_semantics).

:- ensure_loaded(semantics).

test(types_entity_declared) :- entity(react_fastapi_template(frontend(src(types)))).
test(types_have_docstring) :- docstring(react_fastapi_template(frontend(src(types))), _).
test(api_types_exist) :- entity(react_fastapi_template(frontend(src(types(source(file('./api.ts'))))))).
test(type_categories_defined) :- component(react_fastapi_template(frontend(src(types))), category, dashboard_types).
test(actual_type_files_exist) :- exists_file('api.ts').

:- end_tests(types_semantics).