% Python bridge module - handles all py_call interactions for golems
% All data passes as dicts between Prolog and Python

:- module(python_bridge, [
    get_golem_tools/2,
    execute_golem_task/3,
    execute_golem_task_parsed/3,
    get_golem_python_instance/2,
    log_thought_to_session/2,
    ensure_python_grimoire_golems/0,
    parse_golem_messages/2
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
        % Add current directory to Python path for golems module
        grimoire_resolve_path('./', GolemsPath),
        py_add_lib_dir(GolemsPath, first),
        % py_call(sys:version, Version),
        % py_call(sys:executable, Executable),
        % py_call(sys:path, Path),
        % format('DEBUG: Using Python ~w at ~w~n', [Version, Executable]),
        % format('DEBUG: Python sys.path: ~w~n', [Path]),
        % Import grimoire_golems and golems module
        py_import(golems, []),
        % Mark as imported
        assertz(grimoire_golems_imported)
    ).

% Get or create a Python Golem instance from golems module
get_golem_python_instance(golem(Id), GolemObj) :-
    ensure_python_grimoire_golems,
    py_call(golems:Id, GolemObj).

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

% Execute a golem task via Python with structured response
execute_golem_task(Id, InputDict, golem_response(Output, Messages, GolemId, SessionId)) :-
    ensure_python_grimoire_golems,
    % Get golem instance
    get_golem_python_instance(golem(Id), GolemObj),
    % Execute task synchronously (returns GolemResponse)
    catch(
        (
            py_call(GolemObj:execute_task_sync(InputDict), GolemResponse),
            % Extract structured data from GolemResponse
            py_call(GolemResponse:output, Output),
            py_call(GolemResponse:messages, Messages),
            py_call(GolemResponse:golem_id, GolemId),
            py_call(GolemResponse:session_id, SessionId)
        ),
        error(python_error(ErrorType, ExceptionObj), Context),
        (
            translate_python_error(ErrorType, ExceptionObj, ErrorMessage),
            format('DEBUG: Python ~w: ~w~n', [ErrorType, ErrorMessage]),
            throw(error(python_error(ErrorType, ExceptionObj), Context))
        )
    ).

% Translate Python exception object to readable message
translate_python_error(ErrorType, ExceptionObj, ErrorMessage) :-
    catch(
        py_call(str(ExceptionObj), ErrorMessage),
        _,
        ErrorMessage = ErrorType
    ).

% Log thoughts to session database using existing think command
log_thought_to_session(Content, RetVal) :-
    % Use the existing think command from session.pl
    magic_cast(conjure(think(Content)), RetVal).

% Execute a golem task with full parsing (output parser + message parsing)
execute_golem_task_parsed(Id, InputDict, golem_response(ParsedOutput, ParsedMessages, GolemId, SessionId)) :-
    % Execute raw task
    execute_golem_task(Id, InputDict, golem_response(RawOutput, RawMessages, GolemId, SessionId)),
    % Parse messages to structured Prolog terms
    parse_golem_messages(RawMessages, ParsedMessages),
    % Check for output parser and apply it
    (   component(golem(Id), output_parser, Parser)
    ->  % Parse the output into Prolog term
        call(Parser, RawOutput, ParsedOutput)
    ;   % No parser, return raw output
        ParsedOutput = RawOutput
    ).

% Parse messages from golem_response
parse_golem_messages(Messages, ParsedMessages) :-
    maplist(parse_single_message, Messages, ParsedMessages).

% Parse a single ModelMessage (request or response)
parse_single_message(Message, ParsedMessage) :-
    catch(
        (
            % Try to extract message type and content using py_call
            py_call(getattr(Message, 'kind'), Kind),
            py_call(getattr(Message, 'parts'), Parts),
            % Parse based on message kind
            (   Kind == 'request'
            ->  parse_request_parts(Parts, ParsedParts),
                ParsedMessage = model_request(ParsedParts)
            ;   Kind == 'response'
            ->  parse_response_parts(Parts, ParsedParts),
                py_call(getattr(Message, 'usage'), Usage),
                ParsedMessage = model_response(ParsedParts, Usage)
            ;   ParsedMessage = unknown_message(Kind)
            )
        ),
        _Error,
        ParsedMessage = unparseable_message(Message)
    ).

% Parse request parts (SystemPromptPart, UserPromptPart, ToolReturnPart, etc.)
parse_request_parts(Parts, ParsedParts) :-
    maplist(parse_request_part, Parts, ParsedParts).

parse_request_part(Part, ParsedPart) :-
    catch(
        (
            py_call(getattr(Part, 'part_kind'), PartKind),
            (   PartKind == 'system-prompt'
            ->  py_call(getattr(Part, 'content'), Content),
                py_call(getattr(Part, 'timestamp'), Timestamp),
                ParsedPart = system_prompt(Content, Timestamp)
            ;   PartKind == 'user-prompt'
            ->  py_call(getattr(Part, 'content'), Content),
                py_call(getattr(Part, 'timestamp'), Timestamp),
                ParsedPart = user_prompt(Content, Timestamp)
            ;   PartKind == 'tool-return'
            ->  py_call(getattr(Part, 'tool_name'), ToolName),
                py_call(getattr(Part, 'content'), Content),
                py_call(getattr(Part, 'tool_call_id'), ToolCallId),
                py_call(getattr(Part, 'timestamp'), Timestamp),
                ParsedPart = tool_return(ToolName, Content, ToolCallId, Timestamp)
            ;   ParsedPart = unknown_request_part(PartKind)
            )
        ),
        _Error,
        ParsedPart = unparseable_part(Part)
    ).

% Parse response parts (TextPart, ToolCallPart, etc.)
parse_response_parts(Parts, ParsedParts) :-
    maplist(parse_response_part, Parts, ParsedParts).

parse_response_part(Part, ParsedPart) :-
    catch(
        (
            py_call(getattr(Part, 'part_kind'), PartKind),
            (   PartKind == 'text'
            ->  py_call(getattr(Part, 'content'), Content),
                % Optional id field
                (catch(py_call(getattr(Part, 'id'), Id), _, Id = null) -> true; Id = null),
                ParsedPart = text(Content, Id)
            ;   PartKind == 'tool-call'
            ->  py_call(getattr(Part, 'tool_name'), ToolName),
                py_call(getattr(Part, 'args'), Args),
                py_call(getattr(Part, 'tool_call_id'), ToolCallId),
                ParsedPart = tool_call(ToolName, Args, ToolCallId)
            ;   PartKind == 'thinking'
            ->  py_call(getattr(Part, 'content'), Content),
                % Optional fields
                (catch(py_call(getattr(Part, 'id'), Id), _, Id = null) -> true; Id = null),
                (catch(py_call(getattr(Part, 'signature'), Signature), _, Signature = null) -> true; Signature = null),
                (catch(py_call(getattr(Part, 'provider_name'), ProviderName), _, ProviderName = null) -> true; ProviderName = null),
                ParsedPart = thinking(Content, Id, Signature, ProviderName)
            ;   ParsedPart = unknown_response_part(PartKind)
            )
        ),
        _Error,
        ParsedPart = unparseable_part(Part)
    ).

% Ensure Python setup happens when this module is loaded
:- ensure_python_grimoire_golems.
