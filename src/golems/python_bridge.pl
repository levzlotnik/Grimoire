% Python bridge module - handles all py_call interactions for golems
% All data passes as dicts between Prolog and Python

:- module(python_bridge, [
    % Registry and helpers
    list_golem_ids/1,
    get_golem_tools/2,
    golem_tools/2,
    % Execution
    execute_golem_task/3,
    execute_golem_task_parsed/3,
    % Instances
    get_golem_python_instance/2,
    % Logging / session (stubbed)
    log_thought_to_session/2,
    ensure_session_state_loaded/0,  % Stubbed - session being reworked
    get_current_session_id/1,  % Stubbed - session being reworked
    % Init and parsing
    ensure_python_grimoire_golems/0,
    parse_golem_messages/2,
    decode_test_result/2
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

% Minimal local resolver to avoid dependency on external grimoire_resolve_path/2
% Resolves "./" to the directory of this source file; otherwise returns the path unchanged.
grimoire_resolve_path('./', Resolved) :-
    prolog_load_context(file, File),
    file_directory_name(File, Resolved).
grimoire_resolve_path(Path, Path).

% Ensure grimoire_golems Python module is available (idempotent)
ensure_python_grimoire_golems :-
    (   grimoire_golems_imported
    ->  true  % Already imported
    ;   % Import grimoire_golems from nix environment (already in Python path)
        py_import(grimoire_golems, []),
        % Add current directory to Python path for golems module
        grimoire_resolve_path('./', GolemsPath),
        py_add_lib_dir(GolemsPath, first),
        % Import golems module to trigger registration
        py_import(golems, []),
        % Also import Python operator module for getitem()
        py_import(operator, []),
        % Mark as imported
        assertz(grimoire_golems_imported)
    ).

% Get or create a Python Golem instance from grimoire_golems.core registry
get_golem_python_instance(golem(Id), GolemObj) :-
    ensure_python_grimoire_golems,
    py_call(grimoire_golems:core:get_golem(Id), GolemObj).

% === Registry and decoding helpers ===

% List available golem IDs from the Python registry, decoded to a Prolog list of atoms
list_golem_ids(Ids) :-
    ensure_python_grimoire_golems,
    catch(
        (
            py_call(grimoire_golems:core:list_golems(), PyIds),
            py_list_to_atoms(PyIds, Ids)
        ),
        _,
        Ids = []
    ).

% Decode a Python list[str] into a Prolog list[atom]
py_list_to_atoms(PyList, Atoms) :-
    % Obtain length
    py_call(builtins:len(PyList), Len),
    (   Len =:= 0
    ->  Atoms = []
    ;   H is Len - 1,
        numlist(0, H, Indexes),
        findall(A,
            (
                member(I, Indexes),
                py_call(operator:getitem(PyList, I), PyItem),
                py_call(str(PyItem), S),
                atom_string(A, S)
            ),
            Atoms)
    ).

% golem_tools/2: Prolog-friendly decoded tools list
% Falls back to [] if Python bridge fails
golem_tools(Id, Tools) :-
    catch(
        (
            get_golem_tools(golem(Id), PyTools),
            py_tools_to_prolog(PyTools, Tools)
        ),
        _,
        Tools = []
    ).

% For now, convert Python list to a list of opaque items by stringifying each
% Adjust this to a structured decoding if tool schema is required
py_tools_to_prolog(PyList, Tools) :-
    py_call(builtins:len(PyList), Len),
    (   Len =:= 0
    ->  Tools = []
    ;   H is Len - 1,
        numlist(0, H, Indexes),
        findall(Term,
            (
                member(I, Indexes),
                py_call(operator:getitem(PyList, I), PyItem),
                % Convert to string then to atom to avoid Python object leaking
                py_call(str(PyItem), S),
                atom_string(Term, S)
            ),
            Tools)
    ).

% Get tools from instantiated Golem - returns list of dicts
get_golem_tools(golem(Id), Tools) :-
    get_golem_python_instance(golem(Id), GolemObj),
    py_call(GolemObj:get_tools(), Tools).

% Execute a golem task via Python with dict-encoded response (Janus maps Python dicts to SWI dicts)
execute_golem_task(Id, InputDict, golem_response(Output, Messages, GolemId, SessionId)) :-
    ensure_python_grimoire_golems,
    % Get golem instance
    get_golem_python_instance(golem(Id), GolemObj),
    % Execute task synchronously (returns a Python dict that Janus maps to a SWI dict)
    catch(
        (
            py_call(GolemObj:execute_task_sync_prolog(InputDict), Dict),
            % Destructure SWI dict returned from Python using dot notation
            Output = Dict.output,
            Messages = Dict.messages,
            GolemId = Dict.golem_id,
            SessionId = Dict.session_id
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

% Session state - stubbed out until session system is reworked
ensure_session_state_loaded :-
    % TODO: Re-enable when session system is reworked
    true.

% Get current session ID - stubbed out until session system is reworked
get_current_session_id('main').

% Log thoughts - stubbed out until session system is reworked
log_thought_to_session(Content, RetVal) :-
    % TODO: Re-enable when session system is reworked
    RetVal = ok(thought_logged(Content)).

% Execute a golem task with full parsing (output parser + message parsing)
% Note: Output is already a typed dict from encode_to_prolog_dict, no custom parsing needed
execute_golem_task_parsed(Id, InputDict, golem_response(Output, ParsedMessages, GolemId, SessionId)) :-
    % Execute raw task
    execute_golem_task(Id, InputDict, golem_response(Output, RawMessages, GolemId, SessionId)),
    % Parse messages to structured Prolog terms
    parse_golem_messages(RawMessages, ParsedMessages).

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

% === Decoders for per-golem Python objects ===

% Move decoding of TestRunner's TestResult object out of semantics.pl
decode_test_result(PyObj, test_result(Passed, Failed, Skipped, Failures, Coverage, TestPlan, TestCases, Recommendations)) :-
    catch(py_call(PyObj:passed, Passed), _, Passed = 0),
    catch(py_call(PyObj:failed, Failed), _, Failed = 0),
    catch(py_call(PyObj:skipped, Skipped), _, Skipped = 0),
    catch(py_call(PyObj:failures, Failures), _, Failures = []),
    catch(py_call(PyObj:coverage, Coverage), _, Coverage = 0),
    catch(py_call(PyObj:test_plan, TestPlan), _, TestPlan = []),
    catch(py_call(PyObj:test_cases, TestCases), _, TestCases = []),
    catch(py_call(PyObj:recommendations, Recommendations), _, Recommendations = []).

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
