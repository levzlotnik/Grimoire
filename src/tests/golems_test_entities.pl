% Test entities for golems domain
% File-based test knowledge - NO runtime assertz/retract

:- self_entity(test_entities(golems)).

% Test golem entities for different scenarios
entity(test_golem(basic)).
entity(test_golem(with_delegation)).
entity(test_golem(with_parser)).

% Basic test golem configuration
component(test_golem(basic), config, #{
    model: 'test-model',
    temperature: 0.7,
    max_tokens: 1000
}).

component(test_golem(basic), output_parser, none).

% Test golem with delegation capabilities
component(test_golem(with_delegation), config, #{
    model: 'test-model-advanced',
    temperature: 0.5,
    max_tokens: 2000
}).

component(test_golem(with_delegation), output_parser, none).

component(test_golem(with_delegation), can_delegate_to, test_golem(basic)).

% Test golem with output parser
component(test_golem(with_parser), config, #{
    model: 'test-model-structured',
    temperature: 0.3,
    max_tokens: 1500
}).

component(test_golem(with_parser), output_parser, structured_response).

% Test task execution scenarios
entity(test_task(simple)).
entity(test_task(complex)).
entity(test_task(with_error)).

% Simple test task
component(test_task(simple), input_data, #{
    prompt: 'Test prompt',
    context: 'Test context'
}).

component(test_task(simple), expected_success, true).

% Complex test task with multiple steps
component(test_task(complex), input_data, #{
    prompt: 'Multi-step task',
    steps: ['step1', 'step2', 'step3']
}).

component(test_task(complex), expected_success, true).

% Test task that should produce an error
component(test_task(with_error), input_data, #{
    invalid_field: 'causes error'
}).

component(test_task(with_error), expected_success, false).

% Test thought logging
entity(test_thought).

component(test_thought, content, 'Test reasoning step for AI agent').
component(test_thought, expected_logged, true).

% Docstrings for test entities
docstring(test_entities(golems), "File-based test entities for golems domain - configurations, tasks, and scenarios").
docstring(test_golem(basic), "Basic test golem with minimal configuration").
docstring(test_golem(with_delegation), "Test golem with delegation capabilities to other golems").
docstring(test_golem(with_parser), "Test golem with structured output parsing").
docstring(test_task(simple), "Simple test task for golem execution").
docstring(test_task(complex), "Complex multi-step test task").
docstring(test_task(with_error), "Test task designed to produce an error for error handling validation").
docstring(test_thought, "Test thought for session logging validation").
