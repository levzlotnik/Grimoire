% Session domain - persistent state management across Grimoire sessions
% Provides session creation, switching, state persistence, and command history

:- use_module(library(pcre)).
:- use_module(library(readutil)).
:- use_module(library(http/json)).

:- multifile(cast_post_hook/3).

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

% === COMPONENT EXPANSION RULES ===

% Session entity - derives paths from self semantic folder
component(session(SessionId), self, semantic(folder(SessionFolder)))
    ==> component(session(SessionId), session_directory, SessionFolder),
        (component(session(SessionId), session_semantics_path, SemanticsPath) :-
            atomic_list_concat([SessionFolder, '/semantics.pl'], SemanticsPath)),
        (component(session(SessionId), session_activity_log_path, LogPath) :-
            atomic_list_concat([SessionFolder, '/activity_log.jsonl'], LogPath)).

% === COMPONENT VALIDATION ===

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

% === HELPERS ===

% Get current active session ID from cli-session file
current_session_id(SessionId) :-
    grimoire_data_path(DataPath),
    atomic_list_concat([DataPath, '/cli-session'], CliSessionPath),
    exists_file(CliSessionPath),
    read_file_to_string(CliSessionPath, SessionIdStr, []),
    split_string(SessionIdStr, "", " \t\n\r", [SessionIdTrimmed]),
    atom_string(SessionId, SessionIdTrimmed).

% Append component fact to session semantics file and reload
append_component_to_session_file(FilePath, ComponentTerm) :-
    open(FilePath, append, Stream),
    writeq(Stream, ComponentTerm),
    write(Stream, '.\n'),
    close(Stream),
    reload_entity(semantic(file(FilePath))).

% Remove component fact from session semantics file and reload
remove_component_from_session_file(FilePath, ComponentTerm) :-
    read_file_to_terms(FilePath, AllTerms, []),
    exclude(=(ComponentTerm), AllTerms, FilteredTerms),
    open(FilePath, write, Stream),
    forall(member(Term, FilteredTerms),
        (writeq(Stream, Term),
         write(Stream, '.\n'))),
    close(Stream),
    reload_entity(semantic(file(FilePath))).

% === SESSION HOOKS ===

% Log spell casts to activity log
cast_post_hook(SpellTerm, Result, log_spell_to_activity_log(SpellTerm, Result)).

log_spell_to_activity_log(SpellTerm, Result) :-
    % Only log if there's an active session
    (current_session_id(SessionId)
    -> (component(session(SessionId), session_activity_log_path, LogPath),
        % Convert Prolog terms to strings
        get_time(Timestamp),
        term_string(SpellTerm, SpellStr),
        term_string(Result, ResultStr),
        atom_json_dict(JsonLine, _{
            timestamp: Timestamp,
            spell: SpellStr,
            result: ResultStr
        }, [as(string)]),
        % Append to log file
        open(LogPath, append, Stream),
        write(Stream, JsonLine),
        nl(Stream),
        close(Stream))
    ; true).  % No active session, skip logging

log_spell_to_activity_log(_, _).  % Catch-all for errors

% Auto-persist components to session when spell has session_persistent option
cast_post_hook(SpellTerm, Result, persist_if_session_persistent(SpellTerm, Result)).

persist_if_session_persistent(SpellTerm, Result) :-
    % Only persist on successful results
    Result = ok(ResultData),

    % Extract spell signature: conjure(db(create(...))) → conjure(db(create))
    SpellTerm =.. [Verb, InnerTerm],
    InnerTerm =.. [Domain, ActionTerm],
    functor(ActionTerm, Action, _),
    DomainAction =.. [Domain, Action],
    SpellSig =.. [Verb, DomainAction],

    % Check if spell has session_persistent option
    (component(SpellSig, spell_options, Options)
    -> (member(session_persistent(ComponentProperty), Options)
       -> (% We have session_persistent option, check if there's an active session
           current_session_id(SessionId)
          -> (% Get semantics path
              component(session(SessionId), session_semantics_path, SemanticsPath),
              % Create component term: component(session(SessionId), PropertyName, PropertyValue)
              % For db(create), option is has(db(sqlite)) and result is db(sqlite(file(...)))
              % So we create: component(session(SessionId), has(db(sqlite)), db(sqlite(file(...))))
              ComponentTerm = component(session(SessionId), ComponentProperty, ResultData),
              % Append to session file
              append_component_to_session_file(SemanticsPath, ComponentTerm))
          ; true)  % No active session, skip persistence
       ; true)  % No session_persistent option, skip
    ; true).  % No spell_options, skip

persist_if_session_persistent(_, _).  % Catch-all for non-ok results

% === SPELL REGISTRATIONS ===

% Create new session
register_spell(
    conjure(session(create)),
    input(session(create(id('SessionId')))),
    output(ok(session(id('SessionId'), path('Path')))),
    "Create a new session directory and initialize empty state",
    [],
    implementation(conjure(session(create(id(SessionId)))), Result, (
        % Calculate session path
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

        % Load the session semantics file to activate the session entity
        load_entity(semantic(file(SemanticsPath))),

        Result = ok(session(id(SessionId), path(SessionPath)))
    ))
).

% Switch to different session (updates cli-session file and manages KB state)
register_spell(
    conjure(session(switch)),
    input(session(switch(id('SessionId')))),
    output(ok(session(switched(id('SessionId'))))),
    "Switch the active CLI session, unloading old session and loading new one",
    [],
    implementation(conjure(session(switch(id(SessionId)))), Result, (
        % Get target session path
        grimoire_data_path(DataPath),
        atomic_list_concat([DataPath, '/session-', SessionId], SessionPath),

        % Verify target session directory exists
        (exists_directory(SessionPath)
        -> true
        ; throw(error(session_not_found(SessionId),
                     context(session(switch), 'Session directory does not exist')))),

        % Get target session semantics path
        atomic_list_concat([SessionPath, '/semantics.pl'], NewSemanticsPath),

        % Unload old session if one is active
        (current_session_id(OldSessionId)
        -> (component(session(OldSessionId), self, OldSessionSemantic)
           -> unload_entity(OldSessionSemantic)
           ; true)
        ; true),

        % Load new session
        load_entity(semantic(file(NewSemanticsPath))),

        % Update cli-session file
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
    "Delete a session directory and all its data, unloading from KB first",
    [],
    implementation(conjure(session(delete(id(SessionId)))), Result, (
        % Get session path
        grimoire_data_path(DataPath),
        atomic_list_concat([DataPath, '/session-', SessionId], SessionPath),

        % Verify session exists
        (exists_directory(SessionPath)
        -> true
        ; throw(error(session_not_found(SessionId),
                     context(session(delete), 'Session directory does not exist')))),

        % Unload session if loaded
        (component(session(SessionId), self, SessionSemantic)
        -> unload_entity(SessionSemantic)
        ; true),

        % If this is the active session, clear cli-session file
        (current_session_id(ActiveId)
        -> (SessionId = ActiveId
           -> (atomic_list_concat([DataPath, '/cli-session'], CliSessionPath),
               (exists_file(CliSessionPath) -> delete_file(CliSessionPath) ; true))
           ; true)
        ; true),

        % Delete directory
        delete_directory_and_contents(SessionPath),

        Result = ok(session(deleted(id(SessionId))))
    ))
).

% Export session to archive
register_spell(
    conjure(session(export)),
    input(session(export(id('SessionId'), destination('DestPath')))),
    output(ok(session(exported(archive('ArchivePath'))))),
    "Export session to tar.gz archive",
    [],
    implementation(conjure(session(export(id(SessionId), destination(DestPath)))), Result, (
        % Get session directory
        grimoire_data_path(DataPath),
        atomic_list_concat([DataPath, '/session-', SessionId], SessionDir),

        % Verify session exists
        (exists_directory(SessionDir)
        -> true
        ; throw(error(session_not_found(SessionId),
                     context(session(export), 'Session directory does not exist')))),

        % Verify destination exists
        (exists_directory(DestPath)
        -> true
        ; throw(error(destination_not_found(DestPath),
                     context(session(export), 'Destination directory does not exist')))),

        % Create archive path
        atomic_list_concat([DestPath, '/session-', SessionId, '.tar.gz'], ArchivePath),

        % Get parent directory and folder name
        file_directory_name(SessionDir, ParentDir),
        file_base_name(SessionDir, FolderName),

        % Create tar.gz archive
        process_create(path(tar), ['-czf', ArchivePath, '-C', ParentDir, FolderName], []),

        Result = ok(session(exported(archive(ArchivePath))))
    ))
).

% Import session from archive
register_spell(
    conjure(session(import)),
    input(session(import(archive('ArchivePath')))),
    output(ok(session(imported(id('SessionId'))))),
    "Unpack session archive into sessions directory",
    [],
    implementation(conjure(session(import(archive(ArchivePath)))), Result, (
        % Get grimoire data path
        grimoire_data_path(DataPath),

        % Verify archive exists
        (exists_file(ArchivePath)
        -> true
        ; throw(error(archive_not_found(ArchivePath),
                     context(session(import), 'Archive file does not exist')))),

        % Extract session ID from archive filename
        file_base_name(ArchivePath, ArchiveName),
        atom_string(ArchiveName, ArchiveStr),
        (re_matchsub("session-(?<id>[^.]+)", ArchiveStr, Match, [])
        -> get_dict(id, Match, SessionIdStr),
           atom_string(SessionId, SessionIdStr)
        ; throw(error(invalid_archive_name(ArchivePath),
                     context(session(import), 'Archive must be named session-{ID}.tar.gz')))),

        % Extract archive
        process_create(path(tar), ['-xzf', ArchivePath, '-C', DataPath], []),

        % Load the imported session's semantics.pl
        atomic_list_concat([DataPath, '/session-', SessionId], SessionPath),
        atomic_list_concat([SessionPath, '/semantics.pl'], SemanticsPath),
        load_entity(semantic(file(SemanticsPath))),

        % Verify it was imported correctly
        please_verify(component(session(SessionId), session_directory, _)),

        Result = ok(session(imported(id(SessionId))))
    ))
).

% Load entity into session and track it
register_spell(
    conjure(session(load_entity)),
    input(session(load_entity(semantic('Semantic')))),
    output(ok(semantic('Semantic'))),
    "Load an entity into the session and track it for auto-loading on boot",
    [],
    implementation(conjure(session(load_entity(semantic(Semantic)))), Result, (
        % Must have an active session
        (current_session_id(SessionId)
        -> true
        ; throw(error(no_active_session,
                     context(session(load_entity), 'No active session')))),

        % Load the entity
        load_entity(semantic(Semantic)),

        % Append to semantics.pl and reload
        component(session(SessionId), session_semantics_path, SemanticsPath),
        append_component_to_session_file(SemanticsPath,
            component(session(SessionId), has(loaded_entity), semantic(Semantic))),

        Result = ok(semantic(Semantic))
    ))
).

% Unload entity from session
register_spell(
    conjure(session(unload_entity)),
    input(session(unload_entity(semantic('Semantic')))),
    output(ok(unloaded(semantic('Semantic')))),
    "Unload an entity from the session and remove tracking",
    [],
    implementation(conjure(session(unload_entity(semantic(Semantic)))), Result, (
        % Must have an active session
        (current_session_id(SessionId)
        -> true
        ; throw(error(no_active_session,
                     context(session(unload_entity), 'No active session')))),

        % Unload the entity
        unload_entity(semantic(Semantic)),

        % Remove from semantics.pl and reload
        component(session(SessionId), session_semantics_path, SemanticsPath),
        remove_component_from_session_file(SemanticsPath,
            component(session(SessionId), has(loaded_entity), semantic(Semantic))),

        Result = ok(unloaded(semantic(Semantic)))
    ))
).

% Query command history
register_spell(
    perceive(session(history)),
    input(session(history(options('Options')))),
    output(ok(history(commands('Commands')))),
    "Get command history from activity log",
    [],
    implementation(perceive(session(history(options(Options)))), Result, (
        % Must have an active session
        (current_session_id(SessionId)
        -> true
        ; throw(error(no_active_session,
                     context(session(history), 'No active session')))),

        % Get activity log path
        component(session(SessionId), session_activity_log_path, LogPath),

        % Parse options
        (member(limit(Limit), Options) -> true ; Limit = 10),

        % Read and parse JSONL
        (exists_file(LogPath)
        -> read_file_to_string(LogPath, LogContent, [])
        ; LogContent = ""),

        split_string(LogContent, "\n", "", AllLines),
        exclude(=(""), AllLines, Lines),

        % Take last N lines
        reverse(Lines, ReversedLines),
        length(ReversedLines, TotalLines),
        ActualLimit is min(Limit, TotalLines),
        (ActualLimit > 0
        -> (length(LimitedLines, ActualLimit),
            append(LimitedLines, _, ReversedLines),
            reverse(LimitedLines, Commands))
        ; Commands = []),

        Result = ok(history(commands(Commands)))
    ))
).
