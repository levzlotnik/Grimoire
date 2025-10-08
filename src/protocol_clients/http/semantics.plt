% PLUnit tests for HTTP client subdomain
:- use_module(library(plunit)).

:- begin_tests(protocol_client_http).

% Test HTTP service registration
test(http_service_registration, [
    setup(setup_http_test),
    cleanup(cleanup_http_test),
    condition(python_available)
]) :-
    magic_cast(conjure(protocol_client(http(register(
        service(test_api),
        base_url("https://api.test.com"),
        endpoints([endpoint(get_user, "/users/:id", get)])
    )))), Result),
    assertion(Result = ok(service_registered(test_api))).

% Test HTTP list services
test(http_list_services, [
    setup(setup_http_test),
    cleanup(cleanup_http_test),
    condition(python_available)
]) :-
    % Register a test service first
    magic_cast(conjure(protocol_client(http(register(
        service(test_list_api),
        base_url("https://test.com"),
        endpoints([])
    )))), RegResult),
    assertion(RegResult = ok(service_registered(test_list_api))),
    % List services
    magic_cast(perceive(protocol_client(http(list_services))), Result),
    assertion(Result = ok(services(_Services))).

:- end_tests(protocol_client_http).

% === SETUP/CLEANUP ===

setup_http_test :-
    % Create test directory
    make_directory_path('/tmp/test_http').

cleanup_http_test :-
    % Clean up test directory
    (   exists_directory('/tmp/test_http')
    ->  delete_directory_and_contents('/tmp/test_http')
    ;   true
    ).

% Check if Python is available
python_available :-
    catch(py_call(sys:version, _), _, fail).
