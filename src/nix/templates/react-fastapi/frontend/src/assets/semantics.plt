% PLUnit tests for Assets semantics

:- begin_tests(assets_semantics).

:- ensure_loaded(semantics).

test(assets_entity_declared) :- entity(react_fastapi_template(frontend(src(assets)))).
test(assets_have_docstring) :- docstring(react_fastapi_template(frontend(src(assets))), _).
test(react_svg_exists) :- entity(react_fastapi_template(frontend(src(assets(source(file('./react.svg'))))))).
test(asset_types_defined) :- component(react_fastapi_template(frontend(src(assets))), type, logos).
test(actual_asset_files_exist) :- exists_file('react.svg').

:- end_tests(assets_semantics).