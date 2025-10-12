% MCP Python Bridge - ALL py_call + decoding happens here
% Keeps semantics.pl pure Prolog

:- module(mcp_python_bridge, [
    mcp_register_server/3,
    mcp_call_tool/4,
    mcp_list_tools/2,
    mcp_list_servers/1,
    ensure_mcp_server_registered/3
]).

:- use_module(library(janus)).

% Ensure Python MCP client module is imported
:- dynamic mcp_client_imported/0.

ensure_python_mcp_client :-
    (   mcp_client_imported
    ->  true
    ;   grimoire_resolve_path('./', McpPath),
        py_add_lib_dir(McpPath, first),
        py_import(mcp_client, [as(mcp_client)]),
        assertz(mcp_client_imported)
    ).

% Register MCP server (uses FastMCP auto-reflection)
mcp_register_server(Server, Transport, Command) :-
    ensure_python_mcp_client,
    % Convert Prolog command list to Python list
    encode_command(Command, PyCommand),
    py_call(mcp_client:register_server(Server, Transport, PyCommand)).

% Call MCP tool via FastMCP
mcp_call_tool(Server, Tool, Args, DecodedResult) :-
    ensure_python_mcp_client,
    % Encode args to Python dict
    encode_mcp_args(Args, PyArgs),
    % Call Python MCP client
    py_call(mcp_client:call_tool(Server, Tool, PyArgs), PyResult),
    % Decode result to Prolog terms
    decode_mcp_result(PyResult, DecodedResult).

% List tools from MCP server (auto-reflected)
mcp_list_tools(Server, Tools) :-
    ensure_python_mcp_client,
    py_call(mcp_client:list_tools(Server), PyTools),
    decode_tool_list(PyTools, Tools).

% List all registered MCP servers
mcp_list_servers(Servers) :-
    ensure_python_mcp_client,
    py_call(mcp_client:list_servers(), PyServers),
    decode_server_list(PyServers, Servers).

% Ensure server is registered (for verification)
ensure_mcp_server_registered(Server, Transport, Command) :-
    ensure_python_mcp_client,
    encode_command(Command, PyCommand),
    py_call(mcp_client:ensure_registered(Server, Transport, PyCommand)).

% === ENCODING PREDICATES ===

% Encode command list
encode_command(Command, PyCommand) :-
    is_list(Command),
    PyCommand = Command.  % Lists pass through in janus

% Encode MCP args to Python dict (janus converts dicts automatically)
encode_mcp_args(PrologDict, PrologDict) :-
    is_dict(PrologDict).

% === DECODING PREDICATES ===

% Decode MCP result from Python to Prolog
decode_mcp_result(PyResult, mcp_result(Content, IsError)) :-
    (   py_is_dict(PyResult)
    ->  py_call(PyResult:get(content), Content),
        py_call(PyResult:get(isError), IsError)
    ;   % If not a dict, treat as error
        Content = PyResult,
        IsError = true
    ).

% Decode tool list
decode_tool_list(PyTools, Tools) :-
    py_iter(PyTools, PyList),
    maplist(decode_tool, PyList, Tools).

decode_tool(PyTool, tool(Name, Schema)) :-
    py_call(PyTool:name, Name),
    py_call(PyTool:input_schema, PySchema),
    decode_schema(PySchema, Schema).

decode_schema(PySchema, Schema) :-
    (   py_is_dict(PySchema)
    ->  py_iter(PySchema, Items, items),
        maplist(decode_schema_pair, Items, PrologPairs),
        dict_create(Schema, _, PrologPairs)
    ;   Schema = PySchema
    ).

decode_schema_pair(Key-PyValue, Key-PrologValue) :-
    (   py_is_dict(PyValue)
    ->  decode_schema(PyValue, PrologValue)
    ;   py_is_list(PyValue)
    ->  py_iter(PyValue, PyList),
        maplist(decode_schema, PyList, PrologValue)
    ;   PrologValue = PyValue
    ).

% Decode server list
decode_server_list(PyServers, Servers) :-
    py_iter(PyServers, PyList),
    maplist(decode_server, PyList, Servers).

decode_server(PyServer, server(Name, Transport, Command)) :-
    py_call(PyServer:name, Name),
    py_call(PyServer:transport, Transport),
    py_call(PyServer:command, PyCommand),
    py_iter(PyCommand, Command).

% Ensure Python setup happens when this module is loaded
:- ensure_python_mcp_client.
