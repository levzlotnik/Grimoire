% Grimoire Golems - AI Agent Framework with Python-Prolog bridge
% Uses Pydantic AI for unified LLM interface with dict-based configuration

:- self_entity(golems).

% Import prolog-safe predicates from python bridge
:- use_module('python_bridge.pl', [
    get_golem_tools/2,
    execute_golem_task/3,
    get_golem_python_instance/2,
    log_thought_to_session/2
]).

% Load all individual golems
:- load_entity(semantic(file("./code_assistant.pl"))).
:- load_entity(semantic(file("./project_manager.pl"))).
:- load_entity(semantic(file("./test_runner.pl"))).
:- load_entity(semantic(file("./documentation.pl"))).
:- load_entity(semantic(file("./semantics_verifier.pl"))).
:- load_entity(semantic(file("./code_reviewer.pl"))).
:- load_entity(semantic(file("./test_planner.pl"))).
:- load_entity(semantic(file("./architect.pl"))).

% Golem task execution system - conjure spells
entity(golem_task).
entity(thought).

component(conjure, ctor, golem_task).
component(conjure, ctor, thought).

% Execute a golem task - returns dict from Python
cast(conjure(golem_task(golem(Id), InputDict)), OutputDict) :-
    % Execute task with input dict, get output dict
    execute_golem_task(Id, InputDict, OutputDict).

% Execute with structured output parsing
cast(conjure(golem_task(golem(Id), InputDict, ParsedOutput)), OutputDict) :-
    % Execute task
    execute_golem_task(Id, InputDict, RawOutput),
    % Check for output parser
    (   component(golem(Id), output_parser, Parser)
    ->  % Parse the output dict into Prolog term
        call(Parser, RawOutput, ParsedOutput),
        OutputDict = _{raw: RawOutput, parsed: ParsedOutput}
    ;   % No parser, return raw output
        OutputDict = RawOutput,
        ParsedOutput = RawOutput
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
   
   Usage: 
   - conjure(golem_task(golem(agent_id), InputDict))
   - conjure(golem_task(golem(agent_id), InputDict, ParsedOutput))
   |}).

docstring(thought,
   {|string(_)||
   Golem thought logging constructor for conjure spells.
   
   Logs AI agent reasoning and thought processes to session database
   for debugging and audit trails.
   
   Usage: conjure(thought(content))
   |}).

docstring(golems,
   {|string(_)||
   Grimoire Golems AI Agent Framework with Pydantic AI
   
   Uses Pydantic AI for unified LLM interface across all providers.
   Configuration is dict-based, supporting model strings like:
   - "anthropic:claude-3-5-sonnet-20241022" (maps to sonnet-4)
   - "openai:gpt-4o"
   - Custom endpoints via base_url in config dict
   
   Key Features:
   - Dict-based configuration and data exchange
   - Structured output with optional Prolog parsers
   - Auto-discovered tools from GrimoireInterface
   - Support for any Pydantic AI compatible model
   - Session-based execution with thought logging
   |}).