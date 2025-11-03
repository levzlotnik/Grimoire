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
    output(either(
        ok(session(id('SessionId'), path('Path'))),
        error(session_error('Reason'))
    )),
    "Create a new session directory and initialize empty state",
    [],
    implementation(conjure(session(create(id(SessionId)))), Result, (
        % Calculate session path
        grimoire_data_path(DataPath),
        atomic_list_concat([DataPath, '/session-', SessionId], SessionPath),

        % Check if session already exists
        (exists_directory(SessionPath)
        -> Result = error(session_error(session_already_exists(SessionId)))
        ; (% Create session directory
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

           Result = ok(session(id(SessionId), path(SessionPath)))))
    ))
).

% Switch to different session (updates cli-session file and manages KB state)
register_spell(
    conjure(session(switch)),
    input(session(switch(id('SessionId')))),
    output(either(
        ok(session(switched(id('SessionId')))),
        error(session_error('Reason'))
    )),
    "Switch the active CLI session, unloading old session and loading new one",
    [],
    implementation(conjure(session(switch(id(SessionId)))), Result, (
        % Get target session path
        grimoire_data_path(DataPath),
        atomic_list_concat([DataPath, '/session-', SessionId], SessionPath),

        % Verify target session directory exists
        (exists_directory(SessionPath)
        -> (% Get target session semantics path
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

            Result = ok(session(switched(id(SessionId)))))
        ; Result = error(session_error(session_not_found(SessionId))))
    ))
).

% Delete session
register_spell(
    conjure(session(delete)),
    input(session(delete(id('SessionId')))),
    output(either(
        ok(session(deleted(id('SessionId')))),
        error(session_error('Reason'))
    )),
    "Delete a session directory and all its data, unloading from KB first",
    [],
    implementation(conjure(session(delete(id(SessionId)))), Result, (
        % Get session path
        grimoire_data_path(DataPath),
        atomic_list_concat([DataPath, '/session-', SessionId], SessionPath),

        % Verify session exists
        (exists_directory(SessionPath)
        -> (% Unload session if loaded
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

            Result = ok(session(deleted(id(SessionId)))))
        ; Result = error(session_error(session_not_found(SessionId))))
    ))
).

% Export session to archive
register_spell(
    conjure(session(export)),
    input(session(export(id('SessionId'), destination('DestPath')))),
    output(either(
        ok(session(exported(archive('ArchivePath')))),
        error(session_error('Reason'))
    )),
    "Export session to tar.gz archive",
    [],
    implementation(conjure(session(export(id(SessionId), destination(DestPath)))), Result, (
        % Get session directory
        grimoire_data_path(DataPath),
        atomic_list_concat([DataPath, '/session-', SessionId], SessionDir),

        % Verify session exists
        (exists_directory(SessionDir)
        -> (% Verify destination exists
            (exists_directory(DestPath)
            -> (% Create archive path
                atomic_list_concat([DestPath, '/session-', SessionId, '.tar.gz'], ArchivePath),

                % Get parent directory and folder name
                file_directory_name(SessionDir, ParentDir),
                file_base_name(SessionDir, FolderName),

                % Create tar.gz archive
                process_create(path(tar), ['-czf', ArchivePath, '-C', ParentDir, FolderName], []),

                Result = ok(session(exported(archive(ArchivePath)))))
            ; Result = error(session_error(destination_not_found(DestPath)))))
        ; Result = error(session_error(session_not_found(SessionId))))
    ))
).

% Import session from archive
register_spell(
    conjure(session(import)),
    input(session(import(archive('ArchivePath')))),
    output(either(
        ok(session(imported(id('SessionId')))),
        error(session_error('Reason'))
    )),
    "Unpack session archive into sessions directory",
    [],
    implementation(conjure(session(import(archive(ArchivePath)))), Result, (
        % Get grimoire data path
        grimoire_data_path(DataPath),

        % Verify archive exists
        (exists_file(ArchivePath)
        -> (% Extract session ID from archive filename
            file_base_name(ArchivePath, ArchiveName),
            atom_string(ArchiveName, ArchiveStr),
            (re_matchsub("session-(?<id>[^.]+)", ArchiveStr, Match, [])
            -> (get_dict(id, Match, SessionIdStr),
                atom_string(SessionId, SessionIdStr),

                % Extract archive
                process_create(path(tar), ['-xzf', ArchivePath, '-C', DataPath], []),

                % Load the imported session's semantics.pl
                atomic_list_concat([DataPath, '/session-', SessionId], SessionPath),
                atomic_list_concat([SessionPath, '/semantics.pl'], SemanticsPath),
                load_entity(semantic(file(SemanticsPath))),

                % Verify it was imported correctly
                please_verify(component(session(SessionId), session_directory, _)),

                Result = ok(session(imported(id(SessionId)))))
            ; Result = error(session_error(invalid_archive_name(ArchivePath)))))
        ; Result = error(session_error(archive_not_found(ArchivePath))))
    ))
).

% Load entity into session and track it
register_spell(
    conjure(session(load_entity)),
    input(session(load_entity(semantic('Semantic')))),
    output(either(
        ok(semantic('Semantic')),
        error(session_error('Reason'))
    )),
    "Load an entity into the session and track it for auto-loading on boot",
    [],
    implementation(conjure(session(load_entity(semantic(Semantic)))), Result, (
        % Check for active session
        (current_session_id(SessionId)
        -> (% Load the entity
            load_entity(semantic(Semantic)),

            % Append to semantics.pl and reload
            component(session(SessionId), session_semantics_path, SemanticsPath),
            append_component_to_session_file(SemanticsPath,
                component(session(SessionId), has(loaded_entity), semantic(Semantic))),

            Result = ok(semantic(Semantic)))
        ; Result = error(session_error(no_active_session)))
    ))
).

% Unload entity from session
register_spell(
    conjure(session(unload_entity)),
    input(session(unload_entity(semantic('Semantic')))),
    output(either(
        ok(unloaded(semantic('Semantic'))),
        error(session_error('Reason'))
    )),
    "Unload an entity from the session and remove tracking",
    [],
    implementation(conjure(session(unload_entity(semantic(Semantic)))), Result, (
        % Check for active session
        (current_session_id(SessionId)
        -> (% Unload the entity
            unload_entity(semantic(Semantic)),

            % Remove from semantics.pl and reload
            component(session(SessionId), session_semantics_path, SemanticsPath),
            remove_component_from_session_file(SemanticsPath,
                component(session(SessionId), has(loaded_entity), semantic(Semantic))),

            Result = ok(unloaded(semantic(Semantic))))
        ; Result = error(session_error(no_active_session)))
    ))
).

% Focus on entity by name
register_spell(
    conjure(session(focus_entity)),
    input(session(focus_entity(entity('Entity')))),
    output(either(
        ok(focused(entity('Entity'))),
        error(focus_error('Reason'))
    )),
    "Set focused entity for current session by entity name",
    [],
    implementation(conjure(session(focus_entity(entity(Entity)))), Result, (
        % Check for active session
        (current_session_id(SessionId)
        -> (% Verify entity exists
            (entity(Entity)
            -> (% Get session semantics path
                component(session(SessionId), session_semantics_path, SemanticsPath),

                % Remove old focus if exists
                (component(session(SessionId), focused_entity, _OldEntity)
                -> remove_component_from_session_file(SemanticsPath,
                       component(session(SessionId), focused_entity, _))
                ; true),

                % Set new focus
                append_component_to_session_file(SemanticsPath,
                    component(session(SessionId), focused_entity, Entity)),

                Result = ok(focused(entity(Entity))))
            ; Result = error(focus_error(entity_not_found(Entity)))))
        ; Result = error(focus_error(no_active_session)))
    ))
).

% Focus on entity by path
register_spell(
    conjure(session(focus_path)),
    input(session(focus_path(path('Path')))),
    output(either(
        ok(focused(entity('Entity'))),
        error(focus_error('Reason'))
    )),
    "Set focused entity for current session by path lookup via self component",
    [],
    implementation(conjure(session(focus_path(path(Path)))), Result, (
        % Check for active session
        (current_session_id(SessionId)
        -> (% Convert to absolute path
            absolute_file_name(Path, AbsPath),

            % Find entity with matching self component
            findall(E, component(E, self, semantic(folder(AbsPath))), Matches),
            (Matches = []
            -> Result = error(focus_error(no_entity_for_path(AbsPath)))
            ; Matches = [Entity]
            -> (% Get session semantics path
                component(session(SessionId), session_semantics_path, SemanticsPath),

                % Remove old focus if exists
                (component(session(SessionId), focused_entity, _OldEntity)
                -> remove_component_from_session_file(SemanticsPath,
                       component(session(SessionId), focused_entity, _))
                ; true),

                % Set new focus
                append_component_to_session_file(SemanticsPath,
                    component(session(SessionId), focused_entity, Entity)),

                Result = ok(focused(entity(Entity))))
            ; Result = error(focus_error(ambiguous_entities(Matches)))))
        ; Result = error(focus_error(no_active_session)))
    ))
).

% Get focused entity with structured info
register_spell(
    perceive(session(focused)),
    input(session(focused)),
    output(either(
        ok(focused_entity(entity('Entity'), loc('Location'), key_comps('KeyComps'))),
        error(no_entity_focused)
    )),
    "Get currently focused entity with structured information",
    [],
    implementation(perceive(session(focused)), Result, (
        % Check for active session
        (current_session_id(SessionId)
        -> (% Get focused entity
            (component(session(SessionId), focused_entity, Entity)
            -> (% Get entity location via self component
                (component(Entity, self, Location)
                -> true
                ; Location = unknown),

                % Collect key has(...) components
                findall(has(Domain), component(Entity, has(Domain), _), KeyComps),

                Result = ok(focused_entity(entity(Entity), loc(Location), key_comps(KeyComps))))
            ; Result = error(no_entity_focused)))
        ; Result = error(no_entity_focused))
    ))
).

% Clear focused entity
register_spell(
    conjure(session(unfocus)),
    input(session(unfocus)),
    output(either(
        ok(unfocused),
        error(session_error('Reason'))
    )),
    "Clear focused entity for current session",
    [],
    implementation(conjure(session(unfocus)), Result, (
        % Check for active session
        (current_session_id(SessionId)
        -> (% Get session semantics path
            component(session(SessionId), session_semantics_path, SemanticsPath),

            % Remove focus if exists
            (component(session(SessionId), focused_entity, _)
            -> remove_component_from_session_file(SemanticsPath,
                   component(session(SessionId), focused_entity, _))
            ; true),

            Result = ok(unfocused))
        ; Result = error(session_error(no_active_session)))
    ))
).

% Get session status with focused entity info
register_spell(
    perceive(session(status)),
    input(session(status)),
    output(ok(session_status(id('SessionId'), path('Path'), focused('Focused'), active('Active')))),
    "Get current session status including focused entity",
    [],
    implementation(perceive(session(status)), Result, (
        % Get active session ID
        (current_session_id(SessionId)
        -> Active = true
        ; (SessionId = none, Active = false)),

        % Get session path if active
        (Active = true
        -> (component(session(SessionId), session_directory, SessionPath),
            % Get focused entity if exists
            (component(session(SessionId), focused_entity, FocusedEntity)
            -> Focused = entity(FocusedEntity)
            ; Focused = none))
        ; (SessionPath = none, Focused = none)),

        Result = ok(session_status(id(SessionId), path(SessionPath), focused(Focused), active(Active)))
    ))
).

% Query command history
register_spell(
    perceive(session(history)),
    input(session(history(options('Options')))),
    output(either(
        ok(history(commands('Commands'))),
        error(session_error('Reason'))
    )),
    "Get command history from activity log",
    [],
    implementation(perceive(session(history(options(Options)))), Result, (
        % Check for active session
        (current_session_id(SessionId)
        -> (% Get activity log path
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

            Result = ok(history(commands(Commands))))
        ; Result = error(session_error(no_active_session)))
    ))
).
