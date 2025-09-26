% PLUnit tests for Services semantics

:- begin_tests(services_semantics).

:- ensure_loaded(semantics).

test(services_entity_declared) :- entity(react_fastapi_template(frontend(src(services)))).
test(services_have_docstring) :- docstring(react_fastapi_template(frontend(src(services))), _).
test(api_client_exists) :- entity(react_fastapi_template(frontend(src(services(source(file('./apiClient.ts'))))))).
test(services_capabilities) :- component(react_fastapi_template(frontend(src(services))), capability, dashboard_api).
test(actual_service_files_exist) :- exists_file('apiClient.ts').

:- end_tests(services_semantics).