% MCP Client Logic - Pure Prolog (NO py_call)
% All Python interaction happens in python_bridge.pl

:- self_entity(protocol_client(mcp)).

:- use_module('python_bridge.pl', [
    mcp_register_server/3,
    mcp_call_tool/4,
    mcp_list_tools/2,
    mcp_list_servers/1
]).

% === DOCSTRINGS ===

docstring(protocol_client(mcp), "MCP (Model Context Protocol) client for consuming external MCP servers using FastMCP with stdio and HTTP transports").

% === SPELL IMPLEMENTATIONS ===

% MCP server registration spell
register_spell(
    conjure(protocol_client(mcp(register))),
    input(protocol_client(mcp(register(
        server('Server'),
        transport('Transport'),
        command('Command')
    )))),
    output(either(ok(server_registered('Server')), error(mcp_registration_error('Reason')))),
    docstring("Register an MCP server with stdio or HTTP transport and auto-discover its available tools")
).

cast(conjure(protocol_client(mcp(register(
    server(Server),
    transport(Transport),
    command(Command)
)))), Result) :-
    catch(
        (
            mcp_register_server(Server, Transport, Command),
            % Write to config file for persistence
            write_mcp_server_config(Server, Transport, Command),
            Result = ok(server_registered(Server))
        ),
        Error,
        Result = error(mcp_registration_error(Error))
    ).

% MCP tool call spell
register_spell(
    conjure(protocol_client(mcp(call))),
    input(protocol_client(mcp(call(
        server('Server'),
        tool('Tool'),
        args('Args')
    )))),
    output(either(ok(tool_result(content('Content'), server('Server'))), error(mcp_call_error('Reason')))),
    docstring("Call a tool on a registered MCP server with given arguments")
).

cast(conjure(protocol_client(mcp(call(
    server(Server),
    tool(Tool),
    args(Args)
)))), Result) :-
    catch(
        (
            mcp_call_tool(Server, Tool, Args, DecodedResult),
            Result = ok(tool_result(content(DecodedResult), server(Server)))
        ),
        Error,
        Result = error(mcp_call_error(Error))
    ).

% List MCP tools spell
register_spell(
    perceive(protocol_client(mcp(list_tools))),
    input(protocol_client(mcp(list_tools(server('Server'))))),
    output(either(ok(tools('Tools')), error(mcp_list_tools_error('Reason')))),
    docstring("List all tools available on a registered MCP server")
).

cast(perceive(protocol_client(mcp(list_tools(server(Server))))), Result) :-
    catch(
        (
            mcp_list_tools(Server, Tools),
            Result = ok(tools(Tools))
        ),
        Error,
        Result = error(mcp_list_tools_error(Error))
    ).

% List MCP servers spell
register_spell(
    perceive(protocol_client(mcp(list_servers))),
    input(protocol_client(mcp(list_servers))),
    output(either(ok(servers('Servers')), error(mcp_list_servers_error('Reason')))),
    docstring("List all registered MCP servers")
).

cast(perceive(protocol_client(mcp(list_servers))), Result) :-
    catch(
        (
            mcp_list_servers(Servers),
            Result = ok(servers(Servers))
        ),
        Error,
        Result = error(mcp_list_servers_error(Error))
    ).

% === HELPER PREDICATES ===

% Write MCP server config to file for persistence
write_mcp_server_config(Server, Transport, Command) :-
    getenv('GRIMOIRE_DATA', GrimoireData),
    format(atom(ConfigDir), '~w/protocol_clients/mcp', [GrimoireData]),
    % Ensure directory exists
    (   exists_directory(ConfigDir)
    ->  true
    ;   make_directory_path(ConfigDir)
    ),
    format(atom(ConfigPath), '~w/~w.json', [ConfigDir, Server]),
    % Create dict and write as JSON
    ConfigDict = #{
        server: Server,
        transport: Transport,
        command: Command
    },
    open(ConfigPath, write, Stream),
    json_write_dict(Stream, ConfigDict),
    close(Stream).
