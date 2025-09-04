% Tests for Grimoire Golems AI Agent Framework
% Validates ECS components, Python bridge functionality, and golem execution

:- use_module(library(plunit)).

:- begin_tests(golems).

% === GOLEM ENTITY TESTS ===

test(golem_entities_exist) :-
    entity(golem(code_assistant)),
    entity(golem(project_manager)),
    entity(golem(test_runner)).

test(golems_self_entity) :-
    entity(golems).

% === COMPONENT VALIDATION TESTS ===

% Test that all golems have required components
test(golems_have_required_components) :-
    forall(entity(golem(GolemId)), (
        % Every golem must have a role
        component(golem(GolemId), role, Role),
        atom_string(Role, _),
        % Every golem must have LLM configuration
        component(golem(GolemId), llm_config, Config),
        is_valid_llm_config(Config),
        % Every golem must have at least one input and output
        component(golem(GolemId), input, _),
        component(golem(GolemId), output, _)
    )).

% Test LLM configuration structure
test(llm_configs_are_valid) :-
    forall(component(GolemId, llm_config, Config), (
        % Must be a dictionary with required keys
        is_valid_llm_config(Config),
        get_dict(provider, Config, Provider),
        get_dict(model, Config, Model),
        get_dict(max_tokens, Config, Tokens),
        get_dict(temperature, Config, Temp),
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
    forall(entity(golem(GolemId)), (
        docstring(golem(GolemId), DocString),
        atom_string(DocString, DocStr),
        % Docstring should contain key sections
        sub_atom(DocStr, _, _, _, 'Role:'),
        sub_atom(DocStr, _, _, _, 'LLM Backend:')
    )).

% === CONJURE SPELL TESTS ===

test(golem_task_constructor_exists) :-
    component(conjure, ctor, golem_task).

test(thought_constructor_exists) :-
    component(conjure, ctor, thought).

% === PYTHON BRIDGE TESTS ===

test(python_bridge_exports) :-
    current_predicate(python_bridge:get_golem_tools/2),
    current_predicate(python_bridge:execute_golem_task/7),
    current_predicate(python_bridge:get_golem_python_instance/2),
    current_predicate(python_bridge:log_thought_to_session/2).

% Test Python bridge initialization (if Python available)
test(python_bridge_initialization, [condition(python_available)]) :-
    catch(
        (python_bridge:ensure_python_grimoire_golems),
        Error,
        (   Error = error(python_module_not_found(_), Msg)
        ->  format('Skipped: Python module not found - ~w~n', [Msg]),
            fail
        ;   throw(Error)
        )
    ).

% === GOLEM CONFIGURATION TESTS ===

test(code_assistant_configuration) :-
    component(golem(code_assistant), role, Role),
    atom_string(Role, RoleStr),
    sub_atom(RoleStr, _, _, _, 'software engineer'),
    component(golem(code_assistant), llm_config, Config),
    get_dict(provider, Config, anthropic).

test(project_manager_configuration) :-
    component(golem(project_manager), role, Role),
    atom_string(Role, RoleStr),
    sub_atom(RoleStr, _, _, _, 'project manager'),
    component(golem(project_manager), llm_config, Config),
    get_dict(provider, Config, openai).

test(test_runner_configuration) :-
    component(golem(test_runner), role, Role),
    atom_string(Role, RoleStr),
    sub_atom(RoleStr, _, _, _, 'QA engineer'),
    component(golem(test_runner), llm_config, Config),
    get_dict(provider, Config, ollama).

% === DELEGATION HIERARCHY TESTS ===

test(delegation_relationships) :-
    component(golem(code_assistant), can_delegate_to, golem(test_runner)),
    component(golem(project_manager), can_delegate_to, golem(code_assistant)),
    component(golem(test_runner), supervisor, golem(project_manager)).

% === DOCSTRING TESTS ===

test(main_docstring) :-
    docstring(golems, Doc),
    atom_string(Doc, DocStr),
    sub_atom(DocStr, _, _, _, 'AI Agent Framework'),
    sub_atom(DocStr, _, _, _, 'Entity-Component-System').

:- end_tests(golems).

% === HELPER PREDICATES ===

validate_schema_term(optional(Term)) :- !, validate_schema_term(Term).
validate_schema_term(Term) :-
    Term =.. [Type, ArgName],
    atom(Type),
    atom(ArgName).

is_valid_llm_config(Config) :-
    is_dict(Config),
    get_dict(provider, Config, _),
    get_dict(model, Config, _),
    get_dict(max_tokens, Config, _),
    get_dict(temperature, Config, _).

% Helper for conditional tests
python_available :-
    catch(
        py_call('sys':version, _),
        _,
        fail
    ).