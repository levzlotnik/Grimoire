% Session domain - persistent state management across Grimoire sessions
% Provides session creation, switching, state persistence, and command history

:- use_module(library(pcre)).

:- self_entity(session, "Session domain - manage persistent state and command history").

% === ENTITY DECLARATIONS ===

entity(session(create)).
entity(session(switch)).
entity(session(delete)).
entity(session(export)).
entity(session(import)).
entity(session(load_entity)).
entity(session(unload_entity)).
entity(session(history)).

% === BOOT-TIME INITIALIZATION ===
% Automatically load current session when Grimoire starts

boot_current_session :-
    catch(
        (grimoire_data_path(DataPath),
         atomic_list_concat([DataPath, '/cli-session'], CliSessionPath),
         (exists_file(CliSessionPath)
         -> (read_file_to_string(CliSessionPath, SessionIdStr, []),
             % Trim whitespace manually
             split_string(SessionIdStr, "", " \t\n\r", [SessionIdTrimmed]),
             atom_string(SessionId, SessionIdTrimmed),
             component(session(SessionId), session_semantics_path, SemanticsPath),
             (exists_file(SemanticsPath)
             -> (load_entity(semantic(file(SemanticsPath))),
                 % Reload all tracked entities
                 forall(
                     component(session(SessionId), has(loaded_entity), Semantic),
                     load_entity(Semantic)
                 ),
                 format('Loaded session: ~w~n', [SessionId]))
             ; format('Warning: Session ~w semantics.pl not found~n', [SessionId])))
         ; format('No active session (cli-session file missing)~n', []))),
        Error,
        format('Error loading session: ~w~n', [Error])
    ).

% Call boot on initialization
:- initialization(boot_current_session, now).

% === COMPONENT SCHEMAS ===

% Session entity DSL schema - derives paths from self semantic folder
register_dsl_schema(
    session,
    has(session),
    signature(session('SessionId')),
    "Session entity with semantics folder and derived file paths",
    (
        component(session(SessionId), self, semantic(folder(SessionFolder)))
            ==> (component(session(SessionId), session_directory, SessionFolder)),
                (component(session(SessionId), session_semantics_path, SemanticsPath) :-
                    atomic_list_concat([SessionFolder, '/semantics.pl'], SemanticsPath)),
                (component(session(SessionId), session_activity_log_path, LogPath) :-
                    atomic_list_concat([SessionFolder, '/activity_log.jsonl'], LogPath))
    )
).

% Validate session directory exists
component(_, session_directory, Path)
    :: atom(Path),
       exists_directory(Path).

% Validate semantics.pl exists
component(_, session_semantics_path, Path)
    :: atom(Path),
       exists_file(Path).

% Validate activity log exists
component(_, session_activity_log_path, Path)
    :: atom(Path),
       exists_file(Path).

% Get current session ID from cli-session file
current_session_id(SessionId) :-
    grimoire_data_path(DataPath),
    atomic_list_concat([DataPath, '/cli-session'], CliSessionPath),
    exists_file(CliSessionPath),
    read_file_to_string(CliSessionPath, SessionIdStr, []),
    % Trim whitespace manually
    split_string(SessionIdStr, "", " \t\n\r", [SessionIdTrimmed]),
    atom_string(SessionId, SessionIdTrimmed).

% === COMPONENT SCHEMAS ===

% Validate loaded_entity component
component(_, has(loaded_entity), Semantic)
    :: is_valid_semantic_spec(Semantic).

is_valid_semantic_spec(semantic(file(Path))) :- (atom(Path) ; string(Path)).
is_valid_semantic_spec(semantic(folder(Path))) :- (atom(Path) ; string(Path)).

% === POST-HOOK FOR SESSION PERSISTENCE ===
% Multifile hook that logs ALL spells to activity_log.jsonl
% and persists session_persistent spells to semantics.pl

cast_post_hook(SpellTerm, Result, (
    % Only log if we have an active session
    (current_session_id(SessionId)
    -> (% Log ALL spells to activity log
        log_spell_to_commands_db(SessionId, SpellTerm, Result),

        % Check if spell is session_persistent
        (find_matching_spell_sig(SpellTerm, SpellSig)
        -> (component(SpellSig, spell_options, Options)
           -> (member(session_persistent(HasSchema), Options)
              -> (Result = ok(DomainData)
                 -> persist_and_verify(SessionId, HasSchema, DomainData)
                 ; true)  % Don't persist errors
              ; true)
           ; true)
        ; true))
    ; true)  % No session active, skip logging
)).

% Persist spell result to semantics.pl and verify immediately
persist_and_verify(SessionId, HasSchema, DomainData) :-
    % Get session semantics file path
    component(session(SessionId), session_semantics_path, SemanticsPath),

    % Generate component fact
    ComponentFact = component(session(SessionId), HasSchema, DomainData),

    % Append to semantics.pl
    open(SemanticsPath, append, Stream),
    format(Stream, '~n~w.~n', [ComponentFact]),
    close(Stream),

    % Assert the component into KB so it's available immediately
    assertz(ComponentFact),

    % Validate immediately - runtime verification catches incompatibility
    catch(
        please_verify(ComponentFact),
        Error,
        (format('ERROR: Persisted component failed verification: ~w~n', [Error]),
         throw(Error))
    ).

% Log spell invocation to activity_log.jsonl
log_spell_to_commands_db(SessionId, SpellTerm, Result) :-
    component(session(SessionId), session_activity_log_path, LogPath),
    catch(
        (% Serialize terms
         term_string(SpellTerm, SpellStr),
         term_string(Result, ResultStr),

         % Get timestamp
         get_time(Timestamp),
         format_time(string(TimestampStr), '%Y-%m-%dT%H:%M:%S%z', Timestamp),

         % Create JSON object
         format(string(JsonLine), '{"timestamp":"~w","spell":"~w","result":"~w"}~n',
                [TimestampStr, SpellStr, ResultStr]),

         % Append to log file
         open(LogPath, append, Stream),
         write(Stream, JsonLine),
         close(Stream)),
        LogError,
        format('Warning: Failed to log spell to activity_log.jsonl: ~w~n', [LogError])
    ).

% === SPELL REGISTRATIONS ===

% Create new session
register_spell(
    conjure(session(create)),
    input(session(create(id('SessionId')))),
    output(ok(session(id('SessionId'), path('Path')))),
    "Create a new session directory and initialize empty state",
    [],
    implementation(conjure(session(create(id(SessionId)))), Result, (
        % Calculate session path (can't use component - session doesn't exist yet)
        grimoire_data_path(DataPath),
        atomic_list_concat([DataPath, '/session-', SessionId], SessionPath),

        % Check if session already exists
        (exists_directory(SessionPath)
        -> throw(error(session_already_exists(SessionId),
                      context(session(create), 'Session directory already exists')))
        ; true),

        % Create session directory
        make_directory(SessionPath),

        % Initialize semantics.pl with self_entity directive
        atomic_list_concat([SessionPath, '/semantics.pl'], SemanticsPath),
        format(string(InitialContent),
            ":- self_entity(session('~w')).~n~n", [SessionId]),
        open(SemanticsPath, write, SemStream),
        write(SemStream, InitialContent),
        close(SemStream),

        % Initialize activity_log.jsonl (empty file)
        atomic_list_concat([SessionPath, '/activity_log.jsonl'], LogPath),
        open(LogPath, write, LogStream),
        close(LogStream),

        % Load the semantics file to activate the session entity
        load_entity(semantic(file(SemanticsPath))),

        Result = ok(session(id(SessionId), path(SessionPath)))
    ))
).

% Switch to different session (updates cli-session file)
register_spell(
    conjure(session(switch)),
    input(session(switch(id('SessionId')))),
    output(ok(session(switched(id('SessionId'))))),
    "Switch the active CLI session",
    [],
    implementation(conjure(session(switch(id(SessionId)))), Result, (
        % Verify session exists using component system
        please_verify(component(session(SessionId), session_directory, _)),

        % Update cli-session file
        grimoire_data_path(DataPath),
        atomic_list_concat([DataPath, '/cli-session'], CliSessionPath),
        atom_string(SessionId, SessionIdStr),
        open(CliSessionPath, write, Stream),
        write(Stream, SessionIdStr),
        close(Stream),

        Result = ok(session(switched(id(SessionId))))
    ))
).

% Delete session
register_spell(
    conjure(session(delete)),
    input(session(delete(id('SessionId')))),
    output(ok(session(deleted(id('SessionId'))))),
    "Delete a session directory and all its data",
    [],
    implementation(conjure(session(delete(id(SessionId)))), Result, (
        % Verify session exists using component system
        please_verify(component(session(SessionId), session_directory, SessionPath)),

        % If deleting active session, clear cli-session file
        (current_session_id(CurrentSessionId)
        -> (SessionId = CurrentSessionId
           -> (grimoire_data_path(DataPath),
               atomic_list_concat([DataPath, '/cli-session'], CliSessionPath),
               (exists_file(CliSessionPath) -> delete_file(CliSessionPath) ; true))
           ; true)
        ; true),

        % Delete directory
        delete_directory_and_contents(SessionPath),

        Result = ok(session(deleted(id(SessionId))))
    ))
).

% Export session to tar.gz archive
register_spell(
    conjure(session(export)),
    input(session(export(id('SessionId'), destination('DestPath')))),
    output(ok(session(exported(archive('ArchivePath'))))),
    "Export session to tar.gz archive",
    [],
    implementation(conjure(session(export(id(SessionId), destination(DestPath)))), Result, (
        % Verify session exists using component system
        please_verify(component(session(SessionId), session_directory, SessionPath)),

        % Verify destination directory exists
        (exists_directory(DestPath)
        -> true
        ; throw(error(destination_not_found(DestPath),
                     context(session(export), 'Destination directory does not exist')))),

        % Create archive using SessionPath from component
        atomic_list_concat([DestPath, '/session-', SessionId, '.tar.gz'], ArchivePath),
        file_directory_name(SessionPath, DataPath),
        file_base_name(SessionPath, SessionDirName),

        % Use process_create for tar
        process_create(path(tar), [
            '-czf', ArchivePath,
            '-C', DataPath,
            SessionDirName
        ], []),

        Result = ok(session(exported(archive(ArchivePath))))
    ))
).

% Import session from tar.gz archive
register_spell(
    conjure(session(import)),
    input(session(import(archive('ArchivePath')))),
    output(ok(session(imported(id('SessionId'))))),
    "Unpack session archive into sessions directory",
    [],
    implementation(conjure(session(import(archive(ArchivePath)))), Result, (
        grimoire_data_path(DataPath),

        % Verify archive exists
        (exists_file(ArchivePath)
        -> true
        ; throw(error(archive_not_found(ArchivePath),
                     context(session(import), 'Archive file does not exist')))),

        % Extract session ID from archive name
        file_base_name(ArchivePath, ArchiveFile),
        atom_string(ArchiveFile, ArchiveStr),
        (re_matchsub("session-(?<id>[^.]+)", ArchiveStr, Sub, [])
        -> get_dict(id, Sub, SessionIdStr),
           atom_string(SessionId, SessionIdStr)
        ; throw(error(invalid_archive_name(ArchivePath),
                     context(session(import), 'Archive must be named session-{ID}.tar.gz')))),

        % Unpack archive
        process_create(path(tar), [
            '-xzf', ArchivePath,
            '-C', DataPath
        ], []),

        % Verify unpacked session using component system
        please_verify(component(session(SessionId), session_directory, _SessionPath)),
        Result = ok(session(imported(id(SessionId))))
    ))
).

% Load entity into session and track for boot replay
register_spell(
    conjure(session(load_entity)),
    input(session(load_entity(semantic('Semantic')))),
    output(ok(semantic('Semantic'))),
    "Load an entity into the session and track it for auto-loading on boot",
    [session_persistent(has(loaded_entity))],
    implementation(conjure(session(load_entity(Semantic))), Result, (
        % Load immediately (runtime)
        load_entity(Semantic),

        % Return semantic which gets auto-persisted by post-hook as:
        % component(session(SessionId), has(loaded_entity), Semantic)
        Result = ok(Semantic)
    ))
).

% Unload entity (stub - not implemented)
register_spell(
    conjure(session(unload_entity)),
    input(session(unload_entity(semantic('Semantic')))),
    output(error(not_implemented('unload_entity'))),
    "Remove a loaded entity from session tracking (NOT IMPLEMENTED - please report your use case)",
    [],
    implementation(conjure(session(unload_entity(_Semantic))), _Result, (
        throw(error(not_implemented, context(session(unload_entity),
            'This feature is not yet implemented. Please report your use case.')))
    ))
).

% Query session command history
register_spell(
    perceive(session(history)),
    input(session(history(options('Options')))),
    output(ok(history(commands('Commands')))),
    "Query session command history from activity_log.jsonl",
    [],
    implementation(perceive(session(history(Options))), Result, (
        current_session_id(SessionId),
        component(session(SessionId), session_activity_log_path, LogPath),

        % Read all lines from JSONL file
        (exists_file(LogPath)
        -> (read_file_to_string(LogPath, Content, []),
            split_string(Content, "\n", "", Lines),
            % Filter empty lines and parse JSON
            exclude(=(""), Lines, NonEmptyLines),
            maplist(parse_jsonl_line, NonEmptyLines, AllCommands),
            % Apply limit if specified
            (member(limit(Limit), Options)
            -> (length(AllCommands, Total),
                (Total > Limit
                -> (Skip is Total - Limit,
                    length(Prefix, Skip),
                    append(Prefix, Commands, AllCommands))
                ; Commands = AllCommands))
            ; Commands = AllCommands),
            Result = ok(history(commands(Commands))))
        ; Result = ok(history(commands([]))))
    ))
).

% Parse JSONL line to command structure
parse_jsonl_line(Line, command(timestamp(Timestamp), spell(SpellTerm), result(ResultTerm))) :-
    % Simple JSON parsing - extract values between quotes
    re_matchsub('"timestamp":"(?<ts>[^"]+)"', Line, TSMatch, []),
    re_matchsub('"spell":"(?<sp>[^"]+)"', Line, SpMatch, []),
    re_matchsub('"result":"(?<rs>[^"]+)"', Line, RsMatch, []),
    get_dict(ts, TSMatch, Timestamp),
    get_dict(sp, SpMatch, SpellStr),
    get_dict(rs, RsMatch, ResultStr),
    term_string(SpellTerm, SpellStr),
    term_string(ResultTerm, ResultStr).
