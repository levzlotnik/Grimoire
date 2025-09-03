# Grimoire Golems: AI Agent Architecture Design (Revised)

## Overview

Grimoire Golems is a comprehensive AI agent framework built on top of the Grimoire Knowledge-Based Operating System. Golems are intelligent software agents that leverage Grimoire's Entity-Component-System (ECS) architecture to provide structured, autonomous task execution with session management, git-backed transactionality, and per-task execution lifecycle.

## Core Philosophy

### Knowledge-First Agent Design
Golems follow Grimoire's core philosophy where all agent state, configuration, and capabilities are represented as semantic knowledge encoded in Prolog. This enables:
- **Declarative Agent Definition**: All agent properties defined through ECS components
- **File-Based Configuration**: Agent definitions in `src/golems/<golem_id>.pl` files
- **Reasoning About Agents**: The system can query and reason about agent capabilities
- **Version Control**: Git-backed agent definitions with full history

### Per-Task Execution with Session Isolation
Each golem is spawned per-task and operates within its own isolated session, providing:
- **Ephemeral Lifecycle**: Golems are created for specific tasks and die upon completion
- **Session Creation**: Each task spawns a new session with git branch isolation
- **Thought Logging**: `cast(conjure(thought(...)))` logs reasoning to session database
- **Resumable Work**: Sessions enable resuming golem work if needed
- **Atomic Operations**: All agent actions can be committed or rolled back as units
- **Parallel Execution**: Multiple golems can work simultaneously without conflicts
- **Audit Trail**: Complete command history through SQLite logging

## Architecture Overview

- **Grimoire Golems System**
  - **Python LLM Integration Layer** (via janus-swi)
    - LLM Providers:
      - OpenAI (GPT-4, etc)
      - Anthropic (Claude)
      - Ollama (Llama, Mistral - local)
      - Groq (fast inference)
      - Together AI
  - **Grimoire Interface Layer** 
    - GrimoireInterface (Python API)
      - Session Management
      - Git Operations
      - File System Operations
      - Nix Integration
      - Entity/Component Queries
    - Auto-discovered tools via reflection
  - **Prolog Knowledge Base** (janus-swi bridge)
    - Entity-Component-System Core
      - Entity Definitions (`golem(Id)` pattern)
      - Component Relations (role, llm_config, input/output)
      - Spell System (golem_task, thought)
      - Session/Transaction Management

## Entity-Component-System Design

### Golem Entity Structure

Each golem is defined as a compound entity using the `golem(Id)` pattern with components describing its capabilities and configuration:

```prolog
% Example: src/golems/code_assistant.pl
:- self_entity(golem(code_assistant)).

% Core golem components
component(golem(code_assistant), role, "Expert software engineer specialized in code generation, review, and refactoring").
component(golem(code_assistant), llm_config, {
    provider: anthropic,
    model: 'claude-3-5-sonnet-20241022',
    max_tokens: 4096,
    temperature: 0.1
}).

% Input/Output Schema for type safety
component(golem(code_assistant), input, task_description(string)).
component(golem(code_assistant), input, optional(file_context(list))).
component(golem(code_assistant), output, code_response(generated_code)).

% Hierarchical relationships
component(golem(code_assistant), can_delegate_to, golem(test_runner)).
component(golem(code_assistant), can_delegate_to, golem(documentation)).

% Auto-discovered from Golem(Id).tools() through python_bridge
component(golem(code_assistant), available_tools, Tools) :-
    get_golem_tools(golem(code_assistant), Tools).

% Dynamic docstring generation
docstring(golem(code_assistant), DocString) :-
    component(golem(code_assistant), role, Role),
    component(golem(code_assistant), llm_config, Config),
    component(golem(code_assistant), available_tools, Tools),
    findall(input(I), component(golem(code_assistant), input, I), Inputs),
    findall(output(O), component(golem(code_assistant), output, O), Outputs),
    format_golem_docstring(Role, Config, Tools, Inputs, Outputs, DocString).
```

### Component Types

| Component Type | Description | Examples |
|---|---|---|
| `role` | Short role description for the golem | `"Expert software engineer"`, `"QA specialist"` |
| `llm_config` | Dictionary with all LLM configuration | `{provider: anthropic, model: 'claude-3-5-sonnet', max_tokens: 4096, temperature: 0.2}` |
| `input` | Input schema types (set-like, multiple allowed) | `task_description(string)`, `optional(file_context(list))` |
| `output` | Output schema types (set-like, multiple allowed) | `code_response(generated_code)`, `test_results(list)` |
| `can_delegate_to` | Subordinate agents | `golem(test_runner)`, `golem(documentation)` |
| `available_tools` | Auto-discovered from GrimoireInterface | Dynamically generated list of available tools |

### Conversational Message Types

For conversational agents, input/output can include structured message types:

| Message Type | Description | Format |
|---|---|---|
| `thought(Content)` | Agent's internal reasoning | `thought("I need to analyze the code structure first")` |
| `tool_call(ToolId, Args)` | Agent's request to use a tool | `tool_call(grimoire_perceive, {query: "git(status)"})` |
| `tool_response(ToolId, Response)` | Tool's response to agent | `tool_response(grimoire_perceive, {result: "clean"})` |
| `user(Content)` | User's input to the agent | `user("Please implement authentication")` |

## Task Execution Architecture

### Golem Task Execution Flow

```prolog
% In src/golems/semantics.pl - delegates to python_bridge.pl
:- use_module('python_bridge.pl', [
    execute_golem_task/7,
    log_thought_to_session/2,
    get_golem_tools/2
]).

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
```

### Python Bridge Implementation

```prolog
% In src/golems/python_bridge.pl - handles all Python interaction
:- module(python_bridge, [
    execute_golem_task/7,
    log_thought_to_session/2,
    get_golem_tools/2
]).

:- table get_golem_python_instance/2.

% Get or create a Python Golem instance with full configuration from Prolog
get_golem_python_instance(golem(Id), GolemObj) :-
    % Gather all configuration from Prolog components
    component(golem(Id), llm_config, LLMConfigDict),
    component(golem(Id), role, Role),
    % Create Python Golem instance with real config
    py_call('grimoire_golems.core.golem':'Golem'(Id, LLMConfigDict, Role, 'bridge_session'), GolemObj).

% Get tools from instantiated Golem - prolog-safe interface
get_golem_tools(golem(Id), Tools) :-
    get_golem_python_instance(golem(Id), GolemObj),
    py_call(GolemObj:tools(), Tools).

% Execute a golem task via Python
execute_golem_task(Id, LLMConfigDict, Role, InputSchema, OutputSchema, Inputs, RetVal) :-
    % Send to Python execution layer with all config
    py_call('grimoire_golems.manager':execute_task(
        Id, LLMConfigDict, Role, InputSchema, OutputSchema, Inputs
    ), PyResult),
    % Parse Python result
    parse_py_result(PyResult, RetVal).

% Parse Python results into Prolog return values
parse_py_result(task_started(TaskObj), ok(py_obj(TaskObj))).
parse_py_result(error(Reason), error(python(Reason))).

% Log thoughts to session database
log_thought_to_session(Content, RetVal) :-
    get_current_session_id(SessionId),
    py_call('grimoire_golems.session':log_thought(SessionId, Content), Result),
    (Result = success -> 
        RetVal = ok(thought_logged(Content))
    ;
        RetVal = error(thought_log_failed)
    ).
```

### Return Value Semantics

- `ok(py_obj(TaskObj))`: Golem successfully started working on the task (even if it might fail later)
- `error(config_invalid(...))`: Golem couldn't start due to configuration issues
- `error(input_invalid(...))`: Golem couldn't start due to invalid input parameters
- `error(python(Error))`: Python execution layer returned an error

## LLM Integration Layer

### Provider Support Matrix

| Provider | Structured Output | Tool Calling | JSON Schema | Streaming | Local |
|----------|------------------|--------------|-------------|-----------|-------|
| **OpenAI** | ✅ Strict Schema | ✅ Native | ✅ JSON Mode | ✅ | ❌ |
| **Anthropic** | ✅ Tool-based | ✅ Native | ✅ Via Tools | ✅ | ❌ |
| **Ollama** | ✅ JSON Schema | ✅ Via Prompt | ✅ Native | ✅ | ✅ |
| **Groq** | ✅ OpenAI Compatible | ✅ Native | ✅ JSON Mode | ✅ | ❌ |
| **Together AI** | ✅ JSON Schema | ✅ Via Tools | ✅ Native | ✅ | ❌ |

### Grimoire Interface Integration

Golems access the existing `GrimoireInterface` (from `src/interface/api/grimoire_interface.py`) through a simple Golem class:

```python
# src/golems/python/grimoire_golems/core/golem.py
from grimoire_interface import GrimoireInterface
from typing import Dict, List, Any
import inspect

class Golem:
    """Core golem class with GrimoireInterface integration and LLM provider"""

    def __init__(self, golem_id: str, llm_config: Dict, role: str, session_id: str):
        self.golem_id = golem_id
        self.role = role
        self.session_id = session_id
        
        # Initialize Grimoire interface
        self.grimoire_interface = GrimoireInterface()
        
        # Initialize LLM provider from config
        from ..providers import llm_provider_factory
        self.llm_provider = llm_provider_factory(llm_config)
    
    def tools(self) -> List[Dict]:
        """Return tools in a format that can be converted to Prolog and processed by LLM provider"""
        tools = []
        
        # Auto-discover public methods from GrimoireInterface
        for name, method in inspect.getmembers(self.grimoire_interface, predicate=inspect.ismethod):
            if name.startswith('_'):
                continue
                
            # Extract method metadata
            sig = inspect.signature(method)
            doc = inspect.getdoc(method) or f"Execute {name}"
            
            # Create tool definition compatible with both Prolog and LLM providers
            tool = {
                'name': f'grimoire_{name}',
                'description': doc.split('\n')[0],
                'parameters': self._sig_to_schema(sig),
                'prolog_callable': name  # For Prolog bridge
            }
            tools.append(tool)
        
        return tools
    
    def _sig_to_schema(self, sig: inspect.Signature) -> Dict:
        """Convert Python signature to JSON schema"""
        properties = {}
        required = []
        
        for param_name, param in sig.parameters.items():
            if param_name == 'self':
                continue
            
            # Infer type from annotation
            param_type = 'string'  # default
            if param.annotation != inspect.Parameter.empty:
                if param.annotation == int:
                    param_type = 'integer'
                elif param.annotation == bool:
                    param_type = 'boolean'
                elif hasattr(param.annotation, '__origin__'):
                    if param.annotation.__origin__ == list:
                        param_type = 'array'
            
            properties[param_name] = {'type': param_type}
            
            if param.default == inspect.Parameter.empty:
                required.append(param_name)
        
        return {
            'type': 'object',
            'properties': properties,
            'required': required
        }

    def execute_tool(self, tool_name: str, parameters: Dict[str, Any]) -> Dict[str, Any]:
        """Execute a tool by calling the corresponding GrimoireInterface method"""
        # Remove 'grimoire_' prefix to get actual method name
        method_name = tool_name.replace('grimoire_', '', 1)
        
        if hasattr(self.grimoire_interface, method_name):
            try:
                method = getattr(self.grimoire_interface, method_name)
                result = method(**parameters)
                return {"success": True, "result": result}
            except Exception as e:
                return {"success": False, "error": str(e)}
        else:
            return {"success": False, "error": f"Unknown tool: {tool_name}"}

    def start_task(self, inputs: List, input_schema: List, output_schema: List):
        """Start executing a task with the LLM provider"""
        # Implementation depends on specific LLM provider interface
        return self.llm_provider.execute_task(
            role=self.role,
            inputs=inputs,
            tools=self.tools(),
            input_schema=input_schema,
            output_schema=output_schema
        )
```

## Testing Strategy

### Comprehensive Component Validation

The `src/golems/semantics.plt` test suite validates that all golems have proper ECS components:

```prolog
% Test that all golems have required components
test(golems_have_required_components) :-
    forall(golem_entity(GolemId), (
        % Every golem must have a role
        component(GolemId, role, _),
        % Every golem must have LLM configuration
        component(GolemId, llm_config, Config),
        is_valid_llm_config(Config),
        % Every golem must have at least one input and output
        component(GolemId, input, _),
        component(GolemId, output, _)
    )).

% Test LLM configuration structure
test(llm_configs_are_valid) :-
    forall(component(GolemId, llm_config, Config), (
        % Must be a dictionary with required keys
        Config = {provider: Provider, model: Model, max_tokens: Tokens, temperature: Temp},
        atom(Provider),
        atom(Model),
        integer(Tokens),
        number(Temp)
    )).

% Test input/output schemas are well-formed
test(schemas_are_wellformed) :-
    forall(component(GolemId, input, Input), (
        validate_schema_term(Input)
    )),
    forall(component(GolemId, output, Output), (
        validate_schema_term(Output)
    )).

% Test dynamic docstring generation
test(docstrings_generate_correctly) :-
    forall(golem_entity(GolemId), (
        docstring(GolemId, DocString),
        atom_string(DocString, DocStr),
        % Docstring should contain role, LLM backend, tools, and format sections
        sub_atom(DocStr, _, _, _, 'Role:'),
        sub_atom(DocStr, _, _, _, 'LLM Backend:'),
        sub_atom(DocStr, _, _, _, 'Tools:'),
        sub_atom(DocStr, _, _, _, 'Format:')
    )).

% Helper predicates
golem_entity(GolemId) :-
    entity(GolemId),
    GolemId =.. [golem, _].

validate_schema_term(optional(Term)) :- !, validate_schema_term(Term).
validate_schema_term(Term) :-
    Term =.. [Type, ArgName],
    atom(Type),
    atom(ArgName).

is_valid_llm_config(Config) :-
    Config = {provider: _, model: _, max_tokens: _, temperature: _}.
```

## Directory Structure & Package Architecture

### Directory Structure
```
src/golems/
├── semantics.pl          # Main golem system definitions and Python bridge
├── semantics.plt         # Comprehensive tests for all golem components
├── code_assistant.pl     # Individual golem: golem(code_assistant)
├── project_manager.pl    # Individual golem: golem(project_manager)
├── test_runner.pl        # Individual golem: golem(test_runner)
├── documentation.pl      # Individual golem: golem(documentation)
└── python/              # Python package: grimoire-golems
    ├── pyproject.toml
    ├── __init__.py
    ├── janus_bridge.py   # Auto-registers Python functions on import
    ├── core/
    │   ├── __init__.py
    │   ├── base_golem.py
    │   └── manager.py
    ├── providers/
    │   ├── __init__.py
    │   ├── base.py
    │   ├── anthropic.py
    │   ├── openai.py
    │   ├── ollama.py
    │   └── groq.py
    └── tools/
        ├── __init__.py
        └── grimoire_tools.py  # Auto-discovers GrimoireInterface methods
```

### Python Package Integration

Internal Python packages are built and added to the final `pythonEnv` in `flake.nix`:

```nix
# In flake.nix - internal packages added to pythonEnv
let
  # Build internal Python packages
  grimoire-interface = pkgs.python3Packages.buildPythonPackage {
    pname = "grimoire-interface";  # kebab-case package name
    version = "0.1.0";
    src = ./src/interface/api;
    format = "pyproject";
    propagatedBuildInputs = with pkgs.python3Packages; [
      janus-swi pydantic
    ];
  };

  grimoire-golems = pkgs.python3Packages.buildPythonPackage {
    pname = "grimoire-golems";     # kebab-case package name
    version = "0.1.0";
    src = ./src/golems/python;
    format = "pyproject";
    propagatedBuildInputs = with pkgs.python3Packages; [
      grimoire-interface openai anthropic groq janus-swi pydantic
    ];
  };

  # Final Python environment with all packages
  finalPythonEnv = grimoireEnv.python.withPackages (p:
    grimoireEnv.python.pkgs ++ [grimoire-interface grimoire-golems]
  );
```

### Prolog Bridge Integration

In `semantics.pl`, the Python bridge auto-registers on load:
```prolog
:- self_entity(golems).

% Auto-register Python bridge functions on load
:- py_call('grimoire_golems.janus_bridge':register_functions(), _).

% Load all individual golems
:- load_entity(semantic(file("code_assistant.pl"))).
:- load_entity(semantic(file("project_manager.pl"))).
:- load_entity(semantic(file("test_runner.pl"))).
:- load_entity(semantic(file("documentation.pl"))).

% Golem task execution system
component(conjure, ctor, golem_task).
component(conjure, ctor, thought).

% Dynamic docstring formatting
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
```

## Example Golem Definitions

### Code Assistant Golem
```prolog
% src/golems/code_assistant.pl
:- self_entity(golem(code_assistant)).

% Role and LLM configuration as dictionary
component(golem(code_assistant), role, "Expert software engineer specialized in code generation, review, and refactoring").
component(golem(code_assistant), llm_config, {
    provider: anthropic,
    model: 'claude-3-5-sonnet-20241022',
    max_tokens: 8192,
    temperature: 0.1
}).

% Input/Output Schema
component(golem(code_assistant), input, task_description(string)).
component(golem(code_assistant), input, optional(file_context(list))).
component(golem(code_assistant), input, optional(requirements(list))).
component(golem(code_assistant), output, code_response(generated_code)).
component(golem(code_assistant), output, optional(test_suggestions(list))).

% Delegation relationships
component(golem(code_assistant), can_delegate_to, golem(test_runner)).
component(golem(code_assistant), can_delegate_to, golem(documentation)).

% Dynamic docstring with all sections
docstring(golem(code_assistant), DocString) :-
    component(golem(code_assistant), role, Role),
    component(golem(code_assistant), llm_config, Config),
    component(golem(code_assistant), available_tools, Tools),
    findall(input(I), component(golem(code_assistant), input, I), Inputs),
    findall(output(O), component(golem(code_assistant), output, O), Outputs),
    format_golem_docstring(Role, Config, Tools, Inputs, Outputs, DocString).
```

### Project Manager Golem
```prolog
% src/golems/project_manager.pl
:- self_entity(golem(project_manager)).

% Role and LLM configuration
component(golem(project_manager), role, "Senior project manager responsible for coordinating development tasks and ensuring project quality").
component(golem(project_manager), llm_config, {
    provider: openai,
    model: 'gpt-4-turbo',
    max_tokens: 4096,
    temperature: 0.2
}).

% Input/Output Schema for project management
component(golem(project_manager), input, project_description(string)).
component(golem(project_manager), input, optional(timeline_constraints(dict))).
component(golem(project_manager), input, optional(team_members(list))).
component(golem(project_manager), output, project_plan(structured_plan)).
component(golem(project_manager), output, task_assignments(list)).

% Delegation hierarchy
component(golem(project_manager), can_delegate_to, golem(code_assistant)).
component(golem(project_manager), can_delegate_to, golem(test_runner)).
component(golem(project_manager), can_delegate_to, golem(documentation)).
component(golem(project_manager), can_delegate_to, golem(devops)).

% Dynamic docstring
docstring(golem(project_manager), DocString) :-
    component(golem(project_manager), role, Role),
    component(golem(project_manager), llm_config, Config),
    component(golem(project_manager), available_tools, Tools),
    findall(input(I), component(golem(project_manager), input, I), Inputs),
    findall(output(O), component(golem(project_manager), output, O), Outputs),
    format_golem_docstring(Role, Config, Tools, Inputs, Outputs, DocString).
```

### Test Runner Golem
```prolog
% src/golems/test_runner.pl
:- self_entity(golem(test_runner)).

% Role and LLM configuration
component(golem(test_runner), role, "QA engineer focused on testing, test automation, and quality assurance").
component(golem(test_runner), llm_config, {
    provider: ollama,
    model: 'llama-3.1-70b',
    max_tokens: 4096,
    temperature: 0.0
}).

% Input/Output Schema for testing
component(golem(test_runner), input, test_target(string)).
component(golem(test_runner), input, optional(test_types(list))).
component(golem(test_runner), input, optional(coverage_requirements(dict))).
component(golem(test_runner), output, test_results(test_report)).
component(golem(test_runner), output, coverage_report(coverage_data)).
component(golem(test_runner), output, optional(performance_metrics(dict))).

% Hierarchical relationship
component(golem(test_runner), supervisor, golem(project_manager)).

% Dynamic docstring
docstring(golem(test_runner), DocString) :-
    component(golem(test_runner), role, Role),
    component(golem(test_runner), llm_config, Config),
    component(golem(test_runner), available_tools, Tools),
    findall(input(I), component(golem(test_runner), input, I), Inputs),
    findall(output(O), component(golem(test_runner), output, O), Outputs),
    format_golem_docstring(Role, Config, Tools, Inputs, Outputs, DocString).
```

## Usage Examples

### Basic Golem Usage
```bash
# Execute a golem task - returns py_obj reference
./grimoire cast "golem_task(golem(code_assistant), ['implement user authentication system'])"

# Log a thought during golem execution (from within session)
./grimoire cast "thought('Analyzing existing authentication patterns')"

# Check active golem tasks using comp
./grimoire comp "golem(code_assistant)" task

# Query golem role using comp
./grimoire comp "golem(code_assistant)" role

# Query golem LLM configuration
./grimoire comp "golem(code_assistant)" llm_config

# View golem configuration
./grimoire doc "golem(code_assistant)"
```

### Python API Usage
```python
from grimoire_golems.manager import GolemManager
from grimoire_interface import GrimoireInterface

# Initialize golem system
grimoire = GrimoireInterface()
golem_manager = GolemManager(grimoire)

# Start a code assistant golem task
task_obj = golem_manager.execute_task(
    "code_assistant",
    ["Implement a REST API for user management"]
)

# Wait for completion and get results
result = golem_manager.wait_for_completion(task_obj)
if result['success']:
    print(f"Task completed successfully: {result['output']}")
    # Commit the session
    golem_manager.commit_session(task_obj.session_id, "Implemented user management API")
else:
    print(f"Task failed: {result['error']}")
    # Rollback the session
    golem_manager.rollback_session(task_obj.session_id)
```

## Implementation Roadmap

### Phase 0: Prerequisites  
- [ ] Create `python-bridge-pattern` template in `src/nix/templates/`
- [ ] Test template with `nix flake new --template github:your-org/grimoire#python-bridge-pattern`
- [ ] Validate template produces working Python-Prolog bridge

### Phase 1: Core Infrastructure
- [ ] Set up `src/golems/` directory structure using python-bridge-pattern template
- [ ] Implement base golem entity system in Prolog with `golem(Id)` pattern
- [ ] Create golem task execution (`golem_task`, `thought` conjure spells) 
- [ ] Implement `python_bridge.pl` with tabled Python instances
- [ ] Simple session management with per-task lifecycle

### Phase 2: LLM Integration
- [ ] Implement LLM provider classes (OpenAI, Anthropic, Ollama)
- [ ] Auto-discovery system for GrimoireInterface tools
- [ ] Structured input/output schema system
- [ ] Basic golem execution engine with py_obj references
- [ ] Simple code assistant golem implementation

### Phase 3: Testing & Validation
- [ ] Input/output schema validation
- [ ] Dynamic docstring generation
- [ ] Comprehensive test suite for component validation
- [ ] Conversational message type handling
- [ ] Error handling and recovery

### Phase 4: Additional Golems
- [ ] Project manager golem
- [ ] QA/testing golem
- [ ] Documentation golem

### Phase 5: Production Readiness
- [ ] Performance optimization
- [ ] Security hardening
- [ ] Documentation and examples
- [ ] Integration testing

## Configuration Management

### LLM Configuration Flow

All configuration except API keys is passed through Prolog components:

- **LLM Config**: `component(golem(Id), llm_config, {provider: anthropic, model: 'claude-3-5-sonnet', max_tokens: 4096, temperature: 0.1})`
- **Role/Prompt**: `component(golem(Id), role, "Expert software engineer...")`
- **API Keys**: Environment variables (user responsibility)
  - `OPENAI_API_KEY`
  - `ANTHROPIC_API_KEY`
  - `GROQ_API_KEY`
  - etc.

### Package Management

Internal packages follow Python conventions:
- **Package names**: kebab-case (`grimoire-interface`, `grimoire-golems`)
- **Module names**: underscore (`grimoire_interface`, `grimoire_golems`)
- **External deps**: Stay in `./deps/`
- **Internal deps**: Built in `flake.nix` and added to final `pythonEnv`
