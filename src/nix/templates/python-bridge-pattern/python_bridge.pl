% Python bridge module - handles all py_call interactions
% Demonstrates tabled Python object instances and exception handling

:- module(python_bridge, [
    get_domain_tools/2,
    execute_domain_task/3,  
    get_python_instance/2,
    ensure_python_bridge_domain/0
]).

% Initialize janus with the correct Python from our environment immediately
:- (   getenv('PYTHON_EXECUTABLE', PythonExe)
   ->  py_initialize(PythonExe, [], [])
   ;   true
   ).

% Table Python instances for performance
:- table get_python_instance/2.

% Track whether bridge_domain has been imported
:- dynamic bridge_domain_imported/0.

% Ensure bridge_domain Python module is available (idempotent)
ensure_python_bridge_domain :-
    (   bridge_domain_imported
    ->  true  % Already imported
    ;   py_call(sys:version, Version),
        py_call(sys:executable, Executable),
        py_call(sys:path, Path),
        format('Python version: ~w~n', [Version]),
        format('Python executable: ~w~n', [Executable]),
        format('Python sys.path: ~w~n', [Path]),
        py_import(bridge_domain, []),
        assertz(bridge_domain_imported)
    ).

% Get or create a Python domain service instance
get_python_instance(Entity, PyObj) :-
    ensure_python_bridge_domain,
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
        _Error,
        fail
    ).
safe_py_call(Goal, error(Exception)) :-
    catch(
        py_call(Goal, _),
        Exception,
        true
    ).