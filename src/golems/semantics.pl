% Grimoire Golems - AI Agent Framework with Python-Prolog bridge
% Uses Pydantic AI for unified LLM interface with dict-based configuration

:- self_entity(golems).

% Import prolog-safe predicates from python bridge
:- use_module('python_bridge.pl', [
    get_golem_tools/2,
    execute_golem_task/3,
    execute_golem_task_parsed/3,
    get_golem_python_instance/2,
    log_thought_to_session/2
]).

% Load all individual golems from golems package structure
:- load_entity(semantic(folder("./code_assistant"))).
:- load_entity(semantic(folder("./project_manager"))).
:- load_entity(semantic(folder("./documentation"))).
:- load_entity(semantic(folder("./semantics_verifier"))).
:- load_entity(semantic(folder("./code_reviewer"))).
:- load_entity(semantic(folder("./test_planner"))).
:- load_entity(semantic(folder("./test_runner"))).
:- load_entity(semantic(folder("./architect"))).

% Golem task execution system - conjure spells
entity(golem_task).
entity(thought).

component(conjure, ctor, golem_task).
component(conjure, ctor, thought).

% Execute a golem task - returns structured golem_response with parsed output
cast(conjure(golem_task(golem(Id), InputDict)), Result) :-
    catch(
        (
            execute_golem_task_parsed(Id, InputDict, GolemResponse),
            Result = ok(GolemResponse)
        ),
        Error,
        (
            Result = error(golem_execution_failed(Id, Error)),
            (
                Error = error(python_error(ErrorType, ExceptionObj), Context) ->
                (
                    python_bridge:translate_python_error(ErrorType, ExceptionObj, ErrorMessage),
                    format('DEBUG: Python ~w: ~w~n', [ErrorType, ErrorMessage])
                ) ;
                true
            )
        )
    ).

% Log thoughts to session during execution
cast(conjure(thought(Content)), RetVal) :-
    log_thought_to_session(Content, RetVal).

% Dynamic docstring formatting for golems
format_golem_docstring(Config, Tools, DocString) :-
    % Extract model from config dict
    get_dict(model, Config, Model),
    format_model_section(Model, ModelSection),
    format_config_section(Config, ConfigSection),
    format_tools_section(Tools, ToolsSection),
    atomic_list_concat([
        ModelSection, '\n\n',
        ConfigSection, '\n\n',
        ToolsSection
    ], '', DocString).

format_model_section(Model, ModelSection) :-
    format(atom(ModelSection), 'Model: ~w', [Model]).

format_config_section(Config, ConfigSection) :-
    dict_pairs(Config, _, Pairs),
    findall(Line,
        (member(Key-Value, Pairs), format(atom(Line), '  ~w: ~w', [Key, Value])),
        Lines),
    atomic_list_concat(['Configuration:' | Lines], '\n', ConfigSection).

format_tools_section(Tools, ToolsSection) :-
    length(Tools, Count),
    format(atom(ToolsSection), 'Tools: ~w available tools from GrimoireInterface', [Count]).

% Generic docstring rule for all golems
docstring(golem(Id), DocString) :-
    % Get config dict
    component(golem(Id), config, Config),
    % Try to get tools, but fall back gracefully if Python bridge isn't working
    (catch(get_golem_tools(golem(Id), Tools), _, Tools = []) -> true; Tools = []),
    % Format the complete docstring
    format_golem_docstring(Config, Tools, DocString).

% Docstrings
docstring(golem_task,
   {|string(_)||
   Golem task execution constructor for conjure spells.

   Executes an AI agent task with input dict, returning output dict.
   Optionally applies structured output parsing if configured.

   Usage: conjure(golem_task(golem(agent_id), InputDict))
   |}).

docstring(thought,
   {|string(_)||
   Golem thought logging constructor for conjure spells.

   Logs AI agent reasoning and thought processes to session database
   for debugging and audit trails.

   Usage: conjure(thought(Content))
   |}).

docstring(golems,
   {|string(_)||
   Grimoire Golems AI Agent Framework with Pydantic AI

   Uses Pydantic AI for unified LLM interface across all providers.
   Configuration is dict-based, supporting model strings like:
   - "openai:gpt-5-mini"
   - "openai:gpt-4o"
   - Custom endpoints via base_url in config dict

   NOTE: "anthropic:claude-*" syntax is broken in Pydantic AI (issue #2780).
   Use OpenAI models until they fix their shit.

   Key Features:
   - Dict-based configuration and data exchange
   - Structured output with optional Prolog parsers
   - Auto-discovered tools from GrimoireInterface
   - Support for any Pydantic AI compatible model
   - Session-based execution with thought logging
   |}).