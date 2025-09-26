% PLUnit tests for Frontend src semantics

:- begin_tests(frontend_src_semantics).

:- ensure_loaded(semantics).
:- ensure_loaded(components/semantics).
:- ensure_loaded(pages/semantics).
:- ensure_loaded(services/semantics).
:- ensure_loaded(types/semantics).
:- ensure_loaded(assets/semantics).
:- ensure_loaded(utils/semantics).

test(src_entity_declared) :-
    entity(react_fastapi_template(frontend(src))).

test(src_has_docstring) :-
    docstring(react_fastapi_template(frontend(src)), _).

test(src_children_defined) :-
    component(react_fastapi_template(frontend(src)), child, react_fastapi_template(frontend(src(components)))),
    component(react_fastapi_template(frontend(src)), child, react_fastapi_template(frontend(src(pages)))),
    component(react_fastapi_template(frontend(src)), child, react_fastapi_template(frontend(src(services)))).

test(src_files_exist) :-
    entity(react_fastapi_template(frontend(src(source(file('./App.tsx')))))),
    entity(react_fastapi_template(frontend(src(source(file('./main.tsx')))))),
    entity(react_fastapi_template(frontend(src(source(file('./App.css')))))).

test(actual_src_files_exist) :-
    exists_file('App.tsx'),
    exists_file('main.tsx'),
    exists_file('App.css'),
    exists_file('index.css'),
    exists_directory('components'),
    exists_directory('pages').

:- end_tests(frontend_src_semantics).