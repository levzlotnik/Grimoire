% Python bridge module - handles all py_call interactions for golems
% All data passes as dicts between Prolog and Python

:- module(python_bridge, [
    get_golem_tools/2,
    execute_golem_task/3,
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

% Get or create a Python Golem instance from module
get_golem_python_instance(golem(Id), GolemObj) :-
    ensure_python_grimoire_golems,
    % Import the golem directly from its module
    atom_string(Id, IdStr),
    py_call(IdStr:golem, GolemObj).

% Get current session ID from session system
get_current_session_id(SessionId) :-
    (   perceive(session(current(SessionId)))
    ->  true
    ;   SessionId = 'default'
    ).

% Get tools from instantiated Golem - returns list of dicts
get_golem_tools(golem(Id), Tools) :-
    get_golem_python_instance(golem(Id), GolemObj),
    py_call(GolemObj:get_tools(), Tools).

% Execute a golem task via Python with dicts
execute_golem_task(Id, InputDict, OutputDict) :-
    ensure_python_grimoire_golems,
    % Get golem instance
    get_golem_python_instance(golem(Id), GolemObj),
    % Execute task synchronously (returns dict)
    py_call(GolemObj:execute_task_sync(InputDict), OutputDict).

% Log thoughts to session database using existing think command
log_thought_to_session(Content, RetVal) :-
    % Use the existing think command from session.pl
    cast(conjure(think(Content)), RetVal).