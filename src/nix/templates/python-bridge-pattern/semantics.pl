% Domain semantics with Python bridge pattern
% This template demonstrates clean separation between Prolog logic and Python execution

:- self_entity(bridge_domain).

% Import prolog-safe predicates from python bridge
:- use_module('python_bridge.pl', [
    get_domain_tools/2,
    execute_domain_task/3,
    get_python_instance/2
]).

% Example entity with Python-backed functionality
entity(example_service).

% Component that uses Python bridge for computation
component(example_service, available_operations, Operations) :-
    get_domain_tools(example_service, Operations).

% Spell that delegates to Python
component(conjure, ctor, domain_task).

% Execute domain task through bridge
cast(conjure(domain_task(Entity, Input)), Result) :-
    execute_domain_task(Entity, Input, Result).

% Docstrings
docstring(bridge_domain,
   {|string(_)||
   Template domain demonstrating Python-Prolog bridge pattern.
   Shows clean separation between Prolog logic and Python execution.
   All Python interactions are isolated in python_bridge.pl module.
   |}).

docstring(example_service,
   {|string(_)||
   Example service entity that uses Python bridge for operations.
   Available operations are discovered dynamically from Python class.
   |}).