% PLUnit tests for Pages semantics

:- begin_tests(pages_semantics).

:- ensure_loaded(semantics).
:- ensure_loaded(Documentation/semantics).
:- ensure_loaded(Showcase/semantics).

test(pages_entity_declared) :- entity(react_fastapi_template(frontend(src(pages)))).
test(pages_have_docstring) :- docstring(react_fastapi_template(frontend(src(pages))), _).
test(page_files_exist) :- entity(react_fastapi_template(frontend(src(pages(source(file('./Landing.tsx')))))))).
test(page_subdirs_exist) :- entity(react_fastapi_template(frontend(src(pages(documentation))))).
test(actual_page_files_exist) :- exists_file('Landing.tsx'), exists_file('Dashboard.tsx').

:- end_tests(pages_semantics).