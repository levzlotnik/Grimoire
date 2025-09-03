% Tests for Python bridge pattern
% Validates proper separation and functionality

:- begin_tests(bridge_domain).

% Test that bridge module exports expected predicates
test(bridge_exports) :-
    current_predicate(python_bridge:get_domain_tools/2),
    current_predicate(python_bridge:execute_domain_task/3),
    current_predicate(python_bridge:get_python_instance/2).

% Test entity declaration
test(entity_exists) :-
    entity(example_service).

% Test component structure
test(component_structure) :-
    component(example_service, available_operations, _).

% Test conjure constructor
test(conjure_ctor) :-
    component(conjure, ctor, domain_task).

% Test docstrings exist
test(docstrings) :-
    docstring(bridge_domain, Doc1),
    atom_string(Doc1, _),
    docstring(example_service, Doc2),
    atom_string(Doc2, _).

% Test Python instance creation (if Python available)
test(python_instance, [condition(python_available)]) :-
    get_python_instance(test_entity, PyObj),
    PyObj \= [].

% Test safe execution pattern
test(safe_execution) :-
    % This should not fail even if Python is not available
    \+ execute_domain_task(nonexistent, invalid_input, error(_)).

:- end_tests(bridge_domain).

% Helper for conditional tests
python_available :-
    catch(
        py_call('sys':version, _),
        _,
        fail
    ).