% Grimoire Golems - AI Agent Framework with Python-Prolog bridge
% Implements ECS architecture for autonomous AI agents

:- self_entity(golems).

% Import prolog-safe predicates from python bridge
:- use_module('python_bridge.pl', [
    get_golem_tools/2,
    execute_golem_task/7,
    get_golem_python_instance/2,
    log_thought_to_session/2
]).

% Auto-register Python bridge functions on load
% TODO: Fix Python bridge integration
% :- py_call('grimoire_golems.janus_bridge':register_functions(), _).

% Load all individual golems
:- load_entity(semantic(file("./code_assistant.pl"))).
:- load_entity(semantic(file("./project_manager.pl"))).
:- load_entity(semantic(file("./test_runner.pl"))).

% Golem task execution system - conjure spells
entity(golem_task).
entity(thought).

component(conjure, ctor, golem_task).
component(conjure, ctor, thought).

% Execute a golem task - returns py_obj reference for successful starts
cast(conjure(golem_task(golem(Id), Inputs)), RetVal) :-
    % Gather all golem configuration from components
    component(golem(Id), llm_config, LLMConfigDict),
    component(golem(Id), role, Role),
    findall(I, component(golem(Id), input, I), InputSchema),
    findall(O, component(golem(Id), output, O), OutputSchema),
    % Delegate to python bridge
    execute_golem_task(Id, LLMConfigDict, Role, InputSchema, OutputSchema, Inputs, RetVal).

% Log thoughts to session during execution
cast(conjure(thought(Content)), RetVal) :-
    log_thought_to_session(Content, RetVal).

% Dynamic docstring formatting for golems
format_golem_docstring(Role, Config, Tools, Inputs, Outputs, DocString) :-
    % Generate structured docstring with all sections
    format_role_section(Role, RoleSection),
    format_llm_backend_section(Config, BackendSection),
    format_tools_section(Tools, ToolsSection),
    format_schema_section(Inputs, Outputs, SchemaSection),
    atomic_list_concat([
        RoleSection, '\n\n',
        BackendSection, '\n\n',
        ToolsSection, '\n\n',
        SchemaSection
    ], '', DocString).

format_role_section(Role, RoleSection) :-
    format(atom(RoleSection), 'Role: ~w', [Role]).

format_llm_backend_section(Config, BackendSection) :-
    _{provider: Provider, model: Model} :< Config,
    format(atom(BackendSection), 'LLM Backend: ~w (~w)', [Provider, Model]).

format_tools_section(Tools, ToolsSection) :-
    length(Tools, Count),
    format(atom(ToolsSection), 'Tools: ~w available tools from GrimoireInterface', [Count]).

format_schema_section(Inputs, Outputs, SchemaSection) :-
    length(Inputs, InputCount),
    length(Outputs, OutputCount),
    format(atom(SchemaSection), 'Format: ~w inputs, ~w outputs with structured validation', [InputCount, OutputCount]).


% Generic docstring rule for all golems
docstring(golem(Id), DocString) :-
    % Gather golem configuration
    component(golem(Id), role, Role),
    component(golem(Id), llm_config, Config),
    findall(input(I), component(golem(Id), input, I), Inputs),
    findall(output(O), component(golem(Id), output, O), Outputs),
    % Try to get tools, but fall back gracefully if Python bridge isn't working
    (catch(get_golem_tools(golem(Id), Tools), _, Tools = []) -> true; Tools = []),
    % Format the complete docstring
    format_golem_docstring(Role, Config, Tools, Inputs, Outputs, DocString).

% Docstrings
docstring(golem_task,
   {|string(_)||
   Golem task execution constructor for conjure spells.
   
   Executes an AI agent task with specified inputs, returning a Python object
   reference for task monitoring and result retrieval.
   
   Usage: conjure(golem_task(golem(agent_id), input_data))
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
   Grimoire Golems AI Agent Framework

   Provides autonomous AI agents built on Entity-Component-System architecture.
   Each golem is defined with role, LLM configuration, input/output schemas,
   and hierarchical delegation capabilities. Sessions provide git-backed
   transactionality with atomic commit/rollback operations.

   Key Features:
   - Per-task execution lifecycle with session isolation
   - Auto-discovered tools from GrimoireInterface
   - Multi-provider LLM support (OpenAI, Anthropic, Ollama, Groq)
   - Structured input/output validation
   - Thought logging for debugging and audit trails
   |}).