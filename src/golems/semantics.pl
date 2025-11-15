% Grimoire Golems - AI Agent Framework with Python-Prolog bridge
% Uses Pydantic AI for unified LLM interface with dict-based configuration

:- self_entity(golems).

% Import prolog-safe predicates from python bridge
:- use_module('python_bridge.pl', [
    list_golem_ids/1,
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

% === Registry-driven instance enumeration ===
% Expose available golem instances as components from the Python registry
component(golems, instance, golem(Id)) :-
    python_bridge:list_golem_ids(Ids),
    member(Id, Ids).

% === DSL Expansion for has(golems(instance)) ===
% Declarative fact schema:
%   component(Entity, has(golems(instance)), golems(instance(id(Id), options(Opts)))).
% Expands into queryable primitives for verification composition.

% Extract the instance id
component(E, golems_instance_id, Id) :-
    component(E, has(golems(instance)), golems(instance(id(Id), options(_)))).

% Availability derived from the registry
component(E, golems_instance_available, true) :-
    component(E, has(golems(instance)), golems(instance(id(Id), options(_)))),
    component(golems, instance, golem(Id)).

% Fallback: consider a golem available if its Prolog entity exists
% (useful in test environments where Python registry may be empty)
component(E, golems_instance_available, true) :-
    component(E, has(golems(instance)), golems(instance(id(Id), options(_)))),
    entity(golem(Id)).

% === SPELL REGISTRATIONS ===

register_spell(
    conjure(golem_task),
    input(conjure(golem_task(golem(GolemId:atom), input_dict(InputDict:term)))),
    output(either(
        ok(golem_response(parsed_output('ParsedOutput'), messages('Messages'), golem(GolemId:atom), session_id('SessionId'))),
        error(golem_execution_failed(GolemId:atom, 'Error'))
    )),
    docstring("Execute a golem AI agent task with input dict, returning structured golem response with parsed output and messages")
).

% Execute a golem task - returns structured golem_response with parsed output
cast(conjure(golem_task(golem(Id), InputDict)), Result) :-
    catch(
        (
            execute_golem_task_parsed(Id, InputDict, GolemResponse),
            Result = ok(GolemResponse)
        ),
        Error,
        Result = error(golem_execution_failed(Id, Error))
    ).

register_spell(
    conjure(thought),
    input(conjure(thought(content(Content:string)))),
    output(either(
        ok(thought_recorded(Content:string)),
        error(thought_logging_failed('Reason'))
    )),
    docstring("Log AI agent thought/reasoning to session database for debugging and audit trails")
).

% Log thoughts to session during execution
cast(conjure(thought(Content)), RetVal) :-
    log_thought_to_session(Content, RetVal).

% Dynamic docstring formatting for golems
format_golem_docstring(Config, Tools, DocString) :-
    % Extract model from config dict using dot notation
    Model = Config.model,
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
