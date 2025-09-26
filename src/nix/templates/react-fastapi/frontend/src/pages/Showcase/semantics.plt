% PLUnit tests for Showcase semantics

:- begin_tests(showcase_semantics).

:- ensure_loaded(semantics).

test(showcase_entity_declared) :- entity(react_fastapi_template(frontend(src(pages(showcase))))).
test(showcase_has_docstring) :- docstring(react_fastapi_template(frontend(src(pages(showcase)))), _).
test(showcase_files_exist) :- entity(react_fastapi_template(frontend(src(pages(showcase(source(file('./index.tsx')))))))).
test(showcase_categories_defined) :- component(react_fastapi_template(frontend(src(pages(showcase)))), category, buttons).
test(actual_showcase_files_exist) :- exists_file('index.tsx'), exists_file('ButtonsShowcase.tsx').

:- end_tests(showcase_semantics).