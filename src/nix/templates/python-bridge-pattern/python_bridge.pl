% Python bridge module - handles all py_call interactions
% Demonstrates tabled Python object instances and exception handling

:- module(python_bridge, [
    get_domain_tools/2,
    execute_domain_task/3,
    get_python_instance/2
]).

% Table Python instances for performance
:- table get_python_instance/2.

% Get or create a Python domain service instance
get_python_instance(Entity, PyObj) :-
    % Gather configuration from Prolog components if needed
    % In this template, we use simple instantiation
    py_call('bridge_domain.core':'DomainService'(Entity), PyObj).

% Get tools/operations from Python service - prolog-safe interface
get_domain_tools(Entity, Tools) :-
    get_python_instance(Entity, PyObj),
    py_call(PyObj:get_available_operations(), Tools).

% Execute domain task via Python - with exception handling
execute_domain_task(Entity, Input, Result) :-
    get_python_instance(Entity, PyObj),
    catch(
        (py_call(PyObj:execute_task(Input), PyResult),
         Result = ok(PyResult)),
        Error,
        Result = error(python_exception(Error))
    ).

% Helper predicate for safe Python calls
safe_py_call(Goal, ok(Result)) :-
    catch(
        py_call(Goal, Result),
        Error,
        fail
    ).
safe_py_call(Goal, error(Exception)) :-
    catch(
        py_call(Goal, _),
        Exception,
        true
    ).