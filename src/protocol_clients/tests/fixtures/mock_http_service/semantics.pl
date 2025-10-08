% Mock HTTP Service Test Fixture
:- self_entity(mock_http_service).

% Declare mock HTTP service entity
component(mock_http_service, has(protocol_client(http)), protocol_client(http(
    service(mock_service),
    base_url("https://mock.test.com"),
    auth(none),
    endpoints([
        endpoint(test_endpoint, "/test", get)
    ])
))).

% Mock expected response
component(mock_http_service, expected_response, #{
    status => 200,
    body => #{message => "test response"}
}).

docstring(mock_http_service, "Mock HTTP service fixture for testing").
