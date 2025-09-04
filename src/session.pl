% File-Based Session System with Command Accumulation
% Sessions are workspaces that accumulate commands, with SQLite logging and ECS integration
:- use_module(library(uuid)).
:- use_module(library(filesex)).
:- load_entity(semantic(folder("@/src/db"))).

% === ENTITIES AND COMPONENTS ===

:- self_entity(session).
entity(think).

% Session command entities
entity(session(start)).
entity(session(delete)).
entity(session(close)).
entity(session(switch)).
entity(session(commit)).
entity(session(rollback)).
entity(session(commit_accumulated)).
entity(session(current)).
entity(session(list)).
entity(session(status)).
entity(session(history)).

component(session, subcommand, start).
component(session, subcommand, delete).
component(session, subcommand, close).
component(session, subcommand, switch).
component(session, subcommand, commit).
component(session, subcommand, rollback).
component(session, subcommand, current).
component(session, subcommand, list).
component(session, subcommand, status).
component(session, subcommand, history).
component(session, subcommand, commit_accumulated).

% Session conjure/perceive constructors for spell system
component(conjure, ctor, session(start)).
component(conjure, ctor, session(delete)).
component(conjure, ctor, session(close)).
component(conjure, ctor, session(switch)).
component(conjure, ctor, session(commit)).
component(conjure, ctor, session(rollback)).
component(conjure, ctor, session(commit_accumulated)).
component(perceive, ctor, session(current)).
component(perceive, ctor, session(list)).
component(perceive, ctor, session(status)).
component(perceive, ctor, session(history)).

% Think command - conjure (state-changing: adds thought to session)
component(conjure, ctor, think).

% === CORE WORKSPACE PATHS ===

% Get Grimoire root directory: directory from which './grimoire' script is run
grimoire_root_directory(RootDir) :-
    (getenv('GRIMOIRE_ROOT', CustomRoot) ->
        RootDir = CustomRoot
    ;
        working_directory(CurrentDir, CurrentDir),
        RootDir = CurrentDir
    ).

% Get Grimoire data directory: ${GRIMOIRE_DATA:-$HOME/.grimoire}
grimoire_data_directory(DataDir) :-
    (getenv('GRIMOIRE_DATA', CustomDataDir) ->
        DataDir = CustomDataDir
    ;
        getenv('HOME', HomeDir),
        format(atom(DataDir), '~w/.grimoire', [HomeDir])
    ).

% Session workspace directory
session_workspace_path(SessionId, WorkspacePath) :-
    grimoire_data_directory(DataDir),
    format(atom(WorkspacePath), '~w/sessions/~w', [DataDir, SessionId]).

% Session commands database path
session_commands_db_path(SessionId, DbPath) :-
    session_workspace_path(SessionId, WorkspacePath),
    format(atom(DbPath), '~w/commands.db', [WorkspacePath]).

% Session state file path
session_state_file_path(SessionId, StatePath) :-
    session_workspace_path(SessionId, WorkspacePath),
    format(atom(StatePath), '~w/state.pl', [WorkspacePath]).

% === SESSION COMMAND IMPLEMENTATIONS ===

% Start session without ID (generate UUID)
cast(conjure(session(start)), RetVal) :-
    uuid(SessionId),
    cast(conjure(session(start(SessionId))), RetVal).

% Start session with specific ID
cast(conjure(session(start(SessionId))), RetVal) :-
    initialize_session_workspace(SessionId),
    initialize_session_database(SessionId),
    initialize_session_state_file(SessionId),
    set_current_session_id(SessionId),  % Set as current session
    format('🔮 Session ~w started~n', [SessionId]),
    RetVal = ok(session_started(SessionId)).

% Delete session completely
cast(conjure(session(delete(SessionId))), RetVal) :-
    session_workspace_path(SessionId, WorkspacePath),
    (exists_directory(WorkspacePath) ->
        delete_directory_and_contents(WorkspacePath),
        RetVal = ok(session_deleted(SessionId))
    ;
        RetVal = error(session_not_found(SessionId))
    ).

% Show session history
perceive(session(history(Commands))) :-
    get_current_session_id(SessionId),
    get_session_command_history(SessionId, Commands).

% Commit accumulated commands (placeholder)
cast(conjure(session(commit_accumulated(Message))), RetVal) :-
    format('Committing accumulated commands: ~w~n', [Message]),
    RetVal = ok(commit_placeholder(Message)).

% Think command implementations
cast(conjure(think(ThoughtString)), RetVal) :-
    string(ThoughtString),
    log_command_to_session(think, ThoughtString, thought_recorded),
    RetVal = ok(thought_recorded(ThoughtString)).

cast(conjure(think(ThoughtAtom)), RetVal) :-
    atom(ThoughtAtom),
    atom_string(ThoughtAtom, ThoughtString),
    log_command_to_session(think, ThoughtString, thought_recorded),
    RetVal = ok(thought_recorded(ThoughtString)).

% === SESSION WORKSPACE INITIALIZATION ===

% Initialize session workspace directory
initialize_session_workspace(SessionId) :-
    session_workspace_path(SessionId, WorkspacePath),
    (exists_directory(WorkspacePath) ->
        true
    ;
        make_directory_path(WorkspacePath)
    ).

% Initialize session database with schema and ECS registration
initialize_session_database(SessionId) :-
    session_commands_db_path(SessionId, DbPath),
    session_workspace_path(SessionId, WorkspacePath),
    format(atom(SchemaFile), '~w/commands.schema.sql', [WorkspacePath]),

    % Create schema file
    SessionSchema =
        "CREATE TABLE IF NOT EXISTS commands (\n    id INTEGER PRIMARY KEY AUTOINCREMENT,\n    timestamp TEXT NOT NULL,\n    command_type TEXT NOT NULL,\n    command_term TEXT NOT NULL,\n    result TEXT,\n    source TEXT DEFAULT 'user'\n);\nCREATE INDEX IF NOT EXISTS idx_timestamp ON commands(timestamp);\nCREATE INDEX IF NOT EXISTS idx_command_type ON commands(command_type);\nCREATE INDEX IF NOT EXISTS idx_source ON commands(source);",

    open(SchemaFile, write, Stream),
    write(Stream, SessionSchema),
    close(Stream),

    % Create database using db creation command
    format(atom(SessionDbId), 'session_~w', [SessionId]),
    cast(conjure(db(create(SessionDbId, DbPath, schema(file(SchemaFile))))), _).

% Initialize session state file
initialize_session_state_file(SessionId) :-
    session_state_file_path(SessionId, StatePath),
    (exists_file(StatePath) ->
        true
    ;
        open(StatePath, write, Stream),
        format(Stream, '%% Session state file for session ~w~n', [SessionId]),
        format(Stream, '%% Entity loads and persistent state~n~n', []),
        close(Stream)
    ).

% === SESSION STATE MANAGEMENT ===

% Dynamic predicate to track current session
:- dynamic current_session_id/1.

% Get current session ID
get_current_session_id(SessionId) :-
    (current_session_id(SessionId) ->
        true
    ;
        % Default to main if no session is set
        SessionId = main
    ).

% Set current session ID (for testing and session switching)
set_current_session_id(SessionId) :-
    retractall(current_session_id(_)),
    assertz(current_session_id(SessionId)).

% Add entity load to session state
add_entity_load_to_session(SessionId, EntitySpec) :-
    session_state_file_path(SessionId, StatePath),
    open(StatePath, append, Stream),
    format(Stream, '%% Entity loaded: ~w~n', [EntitySpec]),
    format(Stream, ':- load_entity(~q).~n~n', [EntitySpec]),
    close(Stream).

% Load session state file
load_session_state_file(SessionId) :-
    session_state_file_path(SessionId, StatePath),
    (exists_file(StatePath) ->
        consult(StatePath)
    ;
        true
    ).

% === COMMAND LOGGING ===

% Log command to current session database
log_command_to_session(CommandType, CommandTerm, Result) :-
    get_current_session_id(SessionId),
    (SessionId = main ->
        true  % Main session doesn't log commands
    ;
        log_command_to_session_db(SessionId, CommandType, CommandTerm, Result)
    ).

% Log command to specific session database
log_command_to_session_db(SessionId, CommandType, CommandTerm, Result) :-
    session_commands_db_path(SessionId, DbPath),
    (exists_file(DbPath) ->
        catch(
            (% Get current timestamp
             get_time(TimeStamp),
             format_time(atom(FormattedTime), '%Y-%m-%d %H:%M:%S', TimeStamp),

             % Escape command term for SQL
             term_string(CommandTerm, CommandTermStr),
             escape_sql_string(CommandTermStr, EscapedCommandTerm),
             term_string(Result, ResultStr),
             escape_sql_string(ResultStr, EscapedResult),

             % Insert command into database
             format(atom(InsertSQL),
                'INSERT INTO commands (timestamp, command_type, command_term, result) VALUES (\'~w\', \'~w\', \'~w\', \'~w\')',
                [FormattedTime, CommandType, EscapedCommandTerm, EscapedResult]),
             sqlite3_exec(DbPath, InsertSQL)),
            Error,
            format('Warning: Failed to log command: ~w~n', [Error])
        )
    ;
        format('Warning: Session database not found: ~w~n', [DbPath])
    ).

% === DATABASE QUERY FUNCTIONS ===

% Get command history from session database
get_session_command_history(SessionId, Commands) :-
    (SessionId = main ->
        Commands = []  % Main session has no accumulated commands
    ;
        session_commands_db_path(SessionId, DbPath),
        (exists_file(DbPath) ->
            catch(
                (% Query command history
                 sqlite3_query(DbPath,
                     'SELECT timestamp, command_type, command_term, result, source FROM commands ORDER BY timestamp DESC',
                     QueryResults),
                 maplist(json_to_command_entry, QueryResults, Commands)),
                Error,
                (format('Warning: Failed to query command history: ~w~n', [Error]),
                 Commands = [])
            )
        ;
            Commands = []  % No database file
        )
    ).

% Convert JSON query result to command entry
json_to_command_entry(JsonDict, command_entry(Timestamp, CommandType, CommandTerm, Result, Source)) :-
    get_dict(timestamp, JsonDict, Timestamp),
    get_dict(command_type, JsonDict, CommandType),
    get_dict(command_term, JsonDict, CommandTerm),
    get_dict(result, JsonDict, Result),
    get_dict(source, JsonDict, Source).

% Print command entry
print_command_entry(command_entry(Timestamp, CommandType, CommandTerm, Result, Source)) :-
    format('[~w] ~w: ~w -> ~w (source: ~w)~n', [Timestamp, CommandType, CommandTerm, Result, Source]).

% === PERCEIVE PREDICATES ===

% Perceive current session
perceive(session(current(SessionId))) :-
    get_current_session_id(SessionId).

% Perceive session list
perceive(session(list(Sessions))) :-
    grimoire_data_directory(DataDir),
    format(atom(SessionsDir), '~w/sessions', [DataDir]),
    (exists_directory(SessionsDir) ->
        directory_files(SessionsDir, AllFiles),
        exclude(=(.), AllFiles, Files1),
        exclude(=(..), Files1, SessionDirs),
        maplist(atom_string, SessionDirs, Sessions)
    ;
        Sessions = []
    ).

% Perceive session status
perceive(session(status(Status))) :-
    get_current_session_id(SessionId),
    session_workspace_path(SessionId, WorkspacePath),
    (exists_directory(WorkspacePath) ->
        Status = active(SessionId)
    ;
        Status = inactive(SessionId)
    ).

% Perceive session history
perceive(session(history(Commands))) :-
    get_current_session_id(SessionId),
    get_session_command_history(SessionId, Commands).

% === SESSION CLI PREDICATES ===

% List all sessions
list_all_sessions(Result) :-
    grimoire_data_directory(DataDir),
    format(atom(SessionsDir), '~w/sessions', [DataDir]),
    (exists_directory(SessionsDir) ->
        directory_files(SessionsDir, AllFiles),
        exclude(=(.), AllFiles, Files1),
        exclude(=(..), Files1, SessionDirs),
        maplist(atom_string, SessionDirs, Sessions),
        Result = ok(session_list(Sessions))
    ;
        Result = ok(session_list([]))
    ).

% Switch to session
switch_to_session(SessionId, Result) :-
    session_workspace_path(SessionId, WorkspacePath),
    (exists_directory(WorkspacePath) ->
        Result = ok(session_switched(SessionId))
    ;
        Result = error(session_not_found(SessionId))
    ).

% Close session
close_session(SessionId, Strategy, Result) :-
    session_workspace_path(SessionId, WorkspacePath),
    (exists_directory(WorkspacePath) ->
        (Strategy = abandon ->
            % Just remove workspace directory for abandon
            delete_directory_and_contents(WorkspacePath),
            Result = ok(session_closed(SessionId, abandon))
        ;
            % For merge_to_main, would normally merge but just clean up for now
            delete_directory_and_contents(WorkspacePath),
            Result = ok(session_closed(SessionId, merge_to_main))
        )
    ;
        Result = error(session_not_found(SessionId))
    ).

% === DOCSTRINGS ===

docstring(session, "File-based session system with SQLite command logging. Sessions accumulate commands in workspace directories with commands.db logs and state.pl files.").
docstring(think, "Record reasoning/thought process. Takes a string argument for LLM audit trails.").

docstring(session(start),
    {|string(_)||
    Start new session workspace.
    Creates session directory with commands.db, state.pl, and schema files.
    Format: session(start) or session(start(SessionId)).
        SessionId - unique session identifier (auto-generated if not provided)
    |}
).

docstring(session(delete),
    {|string(_)||
    Delete session workspace completely.
    Removes session directory and all associated files.
    Format: session(delete(SessionId)).
        SessionId - session identifier to delete
    |}
).

docstring(session(close),
    {|string(_)||
    Close current session workspace.
    Finalizes session state and switches back to main context.
    Format: session(close).
    |}
).

docstring(session(switch),
    {|string(_)||
    Switch to a different session workspace.
    Changes the active session context.
    Format: session(switch(SessionId)).
        SessionId - session identifier to switch to
    |}
).

docstring(session(commit),
    {|string(_)||
    Commit current session changes to git.
    Creates a git commit with accumulated session changes.
    Format: session(commit(Message)).
        Message - commit message for the changes
    |}
).

docstring(session(rollback),
    {|string(_)||
    Roll back session changes.
    Reverts accumulated session changes using git reset.
    Format: session(rollback).
    |}
).

docstring(session(current),
    {|string(_)||
    Get current active session identifier.
    Returns the currently active session ID.
    Format: session(current(SessionId)).
        SessionId - unifies with current session identifier
    |}
).

docstring(session(list),
    {|string(_)||
    List all available sessions.
    Returns a list of all session identifiers.
    Format: session(list(SessionIds)).
        SessionIds - unifies with list of session identifiers
    |}
).

docstring(session(status),
    {|string(_)||
    Show status of current session.
    Returns session status information including workspace state.
    Format: session(status(StatusInfo)).
        StatusInfo - unifies with session status details
    |}
).

docstring(session(history),
    {|string(_)||
    Show command history for current session.
    Displays accumulated commands from session database.
    Format: session(history).
    |}
).

docstring(session(commit_accumulated),
    {|string(_)||
    Commit accumulated session commands (placeholder).
    Future implementation will handle command batching and persistence.
    Format: session(commit_accumulated(Message)).
        Message - commit message for the accumulated commands
    |}
).

