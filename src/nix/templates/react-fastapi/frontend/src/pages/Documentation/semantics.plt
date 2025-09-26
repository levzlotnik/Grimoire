% PLUnit tests for Documentation semantics

:- begin_tests(documentation_semantics).

:- ensure_loaded(semantics).

test(documentation_entity_declared) :- entity(react_fastapi_template(frontend(src(pages(documentation))))).
test(documentation_has_docstring) :- docstring(react_fastapi_template(frontend(src(pages(documentation)))), _).
test(doc_files_exist) :- entity(react_fastapi_template(frontend(src(pages(documentation(source(file('./index.tsx')))))))).
test(doc_sections_defined) :- component(react_fastapi_template(frontend(src(pages(documentation)))), section, getting_started).
test(actual_doc_files_exist) :- exists_file('index.tsx'), exists_file('GettingStarted.tsx').

:- end_tests(documentation_semantics).