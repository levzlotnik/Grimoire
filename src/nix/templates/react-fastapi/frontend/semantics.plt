% PLUnit tests for Frontend semantics

:- begin_tests(frontend_semantics).

% Load semantic file and subdirectories
:- ensure_loaded(semantics).
:- ensure_loaded(src/semantics).

% Test entity declaration
test(frontend_entity_declared) :-
    entity(react_fastapi_template(frontend)).

% Test docstring
test(frontend_has_docstring) :-
    docstring(react_fastapi_template(frontend), _).

% Test configuration file entities
test(config_files_exist) :-
    entity(react_fastapi_template(frontend(source(file('./package.json'))))),
    entity(react_fastapi_template(frontend(source(file('./vite.config.ts'))))),
    entity(react_fastapi_template(frontend(source(file('./tsconfig.json'))))),
    entity(react_fastapi_template(frontend(source(file('./index.html'))))),
    entity(react_fastapi_template(frontend(source(file('./Dockerfile'))))).

% Test config file docstrings
test(config_files_have_docstrings) :-
    docstring(react_fastapi_template(frontend(source(file('./package.json')))), _),
    docstring(react_fastapi_template(frontend(source(file('./vite.config.ts')))), _),
    docstring(react_fastapi_template(frontend(source(file('./tsconfig.json')))), _),
    docstring(react_fastapi_template(frontend(source(file('./index.html')))), _),
    docstring(react_fastapi_template(frontend(source(file('./Dockerfile')))), _).

% Test child relationships
test(frontend_children_defined) :-
    component(react_fastapi_template(frontend), child, react_fastapi_template(frontend(src))),
    component(react_fastapi_template(frontend), child, react_fastapi_template(frontend(source(file('./package.json'))))),
    component(react_fastapi_template(frontend), child, react_fastapi_template(frontend(source(file('./vite.config.ts'))))).

% Test technology stack
test(technology_stack) :-
    component(react_fastapi_template(frontend), framework, react),
    component(react_fastapi_template(frontend), language, typescript),
    component(react_fastapi_template(frontend), build_tool, vite),
    component(react_fastapi_template(frontend), css_framework, tailwind).

% Test that src entity is defined in submodule
test(src_entity_exists) :-
    entity(react_fastapi_template(frontend(src))).

% Test actual files exist
test(actual_frontend_files_exist) :-
    exists_file('package.json'),
    exists_file('vite.config.ts'),
    exists_file('tsconfig.json'),
    exists_file('index.html'),
    exists_file('Dockerfile'),
    exists_directory('src').

:- end_tests(frontend_semantics).