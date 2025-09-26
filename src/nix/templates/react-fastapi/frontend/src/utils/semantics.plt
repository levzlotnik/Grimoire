% PLUnit tests for Utils semantics

:- begin_tests(utils_semantics).

:- ensure_loaded(semantics).

test(utils_entity_declared) :- entity(react_fastapi_template(frontend(src(utils)))).
test(utils_have_docstring) :- docstring(react_fastapi_template(frontend(src(utils))), _).

:- end_tests(utils_semantics).