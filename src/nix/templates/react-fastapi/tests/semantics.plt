% PLUnit tests for Tests directory semantics

:- begin_tests(tests_semantics).

:- ensure_loaded(semantics).

test(tests_entity_declared) :-
    entity(react_fastapi_template(tests)).

test(tests_have_docstring) :-
    docstring(react_fastapi_template(tests), _).

test(test_files_exist) :-
    entity(react_fastapi_template(tests(source(file('./test_main.py'))))),
    entity(react_fastapi_template(tests(source(file('./test_websocket.py'))))),
    entity(react_fastapi_template(tests(source(file('./__init__.py'))))).

test(test_files_have_docstrings) :-
    docstring(react_fastapi_template(tests(source(file('./test_main.py')))), _),
    docstring(react_fastapi_template(tests(source(file('./test_websocket.py')))), _),
    docstring(react_fastapi_template(tests(source(file('./__init__.py')))), _).

test(test_framework_defined) :-
    component(react_fastapi_template(tests), framework, pytest),
    component(react_fastapi_template(tests), test_client, fastapi_testclient).

test(test_types_defined) :-
    component(react_fastapi_template(tests), test_types, Types),
    is_list(Types),
    member(unit_tests, Types),
    member(integration_tests, Types).

test(actual_test_files_exist) :-
    exists_file('test_main.py'),
    exists_file('test_websocket.py'),
    exists_file('__init__.py').

:- end_tests(tests_semantics).