% Test Runner Golem
% QA engineer focused on testing, test automation, and quality assurance

:- self_entity(golem(test_runner)).

% Structured output parser
component(golem(test_runner), output_parser, parse_test_result).

parse_test_result(TestResultObj, test_result(Passed, Failed, Skipped, Failures, Coverage, TestPlan, TestCases, Recommendations)) :-
    % Extract fields from TestResult Pydantic model using py_call
    py_call(TestResultObj:passed, Passed),
    py_call(TestResultObj:failed, Failed),
    py_call(TestResultObj:skipped, Skipped),
    py_call(TestResultObj:failures, Failures),
    py_call(TestResultObj:coverage, Coverage),
    py_call(TestResultObj:test_plan, TestPlan),
    py_call(TestResultObj:test_cases, TestCases),
    py_call(TestResultObj:recommendations, Recommendations).

% Hierarchical relationship
component(golem(test_runner), supervisor, golem(project_manager)).

% Auto-discovered tools
component(golem(test_runner), available_tools, Tools) :-
    get_golem_tools(golem(test_runner), Tools).

% Docstring generated automatically by generic golem docstring rule in parent semantics.pl