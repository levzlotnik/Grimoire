% Mock HTTP Response Test Fixture
:- self_entity(mock_http_response).

% Declare expected HTTP response
component(mock_http_response, expected_response, response(
    status(200),
    body(#{
        data => "test data",
        success => true
    }),
    service(mock_api)
)).

component(mock_http_response, test_service, mock_api).
component(mock_http_response, test_endpoint, test_endpoint).
component(mock_http_response, test_params, #{id => "123"}).

docstring(mock_http_response, "Mock HTTP response fixture for testing").
