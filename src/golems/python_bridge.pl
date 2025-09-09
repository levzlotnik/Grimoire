% Python bridge module - handles all py_call interactions for golems
% Implements tabled Python object instances and exception handling for AI agents

:- module(python_bridge, [
    get_golem_tools/2,
    execute_golem_task/7,
    get_golem_python_instance/2,
    log_thought_to_session/2,
    ensure_python_grimoire_golems/0
]).

% Initialize janus with the correct Python from our environment immediately
:- (   getenv('PYTHON_EXECUTABLE', PythonExe)
   ->  py_initialize(PythonExe, [], [])
   ;   true
   ).

% Table Python instances for performance
:- table get_golem_python_instance/2.

% Track whether grimoire_golems has been imported
:- dynamic grimoire_golems_imported/0.

% Don't initialize at load time - let it happen on first use

% Ensure grimoire_golems Python module is available (idempotent)
ensure_python_grimoire_golems :-
    (   grimoire_golems_imported
    ->  true  % Already imported
    ;   % Check Python environment (sys is already available in janus)
        py_call(sys:version, Version),
        py_call(sys:executable, Executable),
        py_call(sys:path, Path),
        format('Python version: ~w~n', [Version]),
        format('Python executable: ~w~n', [Executable]),
        format('Python sys.path: ~w~n', [Path]),
        % Import grimoire_golems - fail fast if not available
        py_import(grimoire_golems, []),
        assertz(grimoire_golems_imported)
    ).

% Get or create a Python Golem instance with full configuration from Prolog
get_golem_python_instance(golem(Id), GolemObj) :-
    ensure_python_grimoire_golems,
    % Gather all configuration from Prolog components
    component(golem(Id), llm_config, LLMConfigDict),
    component(golem(Id), role, Role),
    % Get current session ID for this execution context
    get_current_session_id(SessionId),
    % Create Python Golem instance with real config
    py_call('grimoire_golems.core.golem':'Golem'(Id, LLMConfigDict, Role, SessionId), GolemObj).

% Get current session ID from session system
get_current_session_id(SessionId) :-
    perceive(session(current(SessionId))).

% Get tools from instantiated Golem - prolog-safe interface
get_golem_tools(golem(Id), Tools) :-
    get_golem_python_instance(golem(Id), GolemObj),
    py_call(GolemObj:tools(), Tools).

% Execute a golem task via Python
execute_golem_task(Id, LLMConfigDict, Role, InputSchema, OutputSchema, Inputs, RetVal) :-
    ensure_python_grimoire_golems,
    % Send to Python execution layer with all config - fail fast
    py_call('grimoire_golems.manager':execute_task(
        Id, LLMConfigDict, Role, InputSchema, OutputSchema, Inputs
    ), PyResult),
    parse_py_result(PyResult, RetVal).

% Parse Python results into Prolog return values
parse_py_result(task_started(TaskObj), ok(py_obj(TaskObj))).
parse_py_result(error(Reason), error(python(Reason))).
parse_py_result(success(Output), ok(Output)).

% Log thoughts to session database using existing think command
log_thought_to_session(Content, RetVal) :-
    % Use the existing think command from session.pl
    cast(conjure(think(Content)), RetVal).