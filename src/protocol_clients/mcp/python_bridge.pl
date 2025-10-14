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

% Register MCP server with stdio transport
mcp_register_server(Server, stdio, Command) :-
    ensure_python_mcp_client,
    % Convert Prolog command list to Python list
    encode_command(Command, PyCommand),
    py_call(mcp_client:register_server(Server, stdio, command=PyCommand)).

% Register MCP server with http transport
mcp_register_server(Server, http, Url) :-
    ensure_python_mcp_client,
    atom_string(Url, UrlStr),
    py_call(mcp_client:register_server(Server, http, url=UrlStr)).

% Also support streamablehttp as alias for http
mcp_register_server(Server, streamablehttp, Url) :-
    mcp_register_server(Server, http, Url).

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
    % PyTools is now a Prolog list of py{...} dicts (auto-converted by Janus)
    maplist(extract_tool, PyTools, Tools).

% List all registered MCP servers
mcp_list_servers(Servers) :-
    ensure_python_mcp_client,
    py_call(mcp_client:list_servers(), PyServers),
    % PyServers is now a Prolog list of py{...} dicts (auto-converted by Janus)
    maplist(extract_server, PyServers, Servers).

% Ensure server is registered (for verification) - stdio transport
ensure_mcp_server_registered(Server, stdio, Command) :-
    ensure_python_mcp_client,
    encode_command(Command, PyCommand),
    py_call(mcp_client:ensure_registered(Server, stdio, command=PyCommand)).

% Ensure server is registered (for verification) - http transport
ensure_mcp_server_registered(Server, http, Url) :-
    ensure_python_mcp_client,
    atom_string(Url, UrlStr),
    py_call(mcp_client:ensure_registered(Server, http, url=UrlStr)).

% Also support streamablehttp as alias for http
ensure_mcp_server_registered(Server, streamablehttp, Url) :-
    ensure_mcp_server_registered(Server, http, Url).

% === ENCODING PREDICATES ===

% Encode command list
encode_command(Command, PyCommand) :-
    is_list(Command),
    PyCommand = Command.  % Lists pass through in janus

% Encode MCP args to Python dict (janus converts dicts automatically)
encode_mcp_args(Args, Args) :- is_dict(Args).

% === DECODING PREDICATES ===

% Decode MCP result from Python to Prolog (auto-converted by Janus)
% Content is now a list of content items [{'type': 'text', 'text': '...'}, ...]
decode_mcp_result(py{content: ContentList, isError: IsError}, mcp_result(DecodedContent, IsError)) :-
    !,
    decode_content_list(ContentList, DecodedContent).
decode_mcp_result(PyResult, mcp_result(PyResult, true)).  % Fallback: treat as error

% Decode content list - extract text from content items
decode_content_list([], []).
decode_content_list([ContentItem|Rest], [Text|RestDecoded]) :-
    (   py{type: text, text: Text} = ContentItem
    ->  true
    ;   py{type: image, data: Data, mimeType: MimeType} = ContentItem
    ->  Text = image(Data, MimeType)
    ;   Text = ContentItem  % Fallback
    ),
    decode_content_list(Rest, RestDecoded).

% Extract tool from Python dict (auto-converted by Janus)
extract_tool(py{name: Name, description: _Desc, input_schema: Schema},
             tool(Name, Schema)).
% Note: input_schema is already a Prolog term (dict or primitive)

% Extract server from Python dict (auto-converted by Janus)
% Handle stdio transport (has command)
extract_server(py{name: Name, transport: stdio, command: Command, tools: _Tools, url: @(null)},
               server(Name, stdio, Command)) :- !.
% Handle streamablehttp transport (has url)
extract_server(py{name: Name, transport: streamablehttp, command: @(null), tools: _Tools, url: Url},
               server(Name, streamablehttp, Url)) :- !.
% Fallback for other cases
extract_server(py{name: Name, transport: Transport},
               server(Name, Transport, unknown)).

% Ensure Python setup happens when this module is loaded
:- ensure_python_mcp_client.
