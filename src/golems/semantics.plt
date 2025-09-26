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
        % Every golem must have configuration
        component(golem(GolemId), config, Config),
        is_valid_config(Config)
    )).

% Test configuration structure
test(llm_configs_are_valid) :-
    forall(component(_GolemId, config, Config), (
        % Must be a dictionary with required keys
        is_valid_config(Config),
        get_dict(model, Config, Model),
        get_dict(max_tokens, Config, Tokens),
        get_dict(temperature, Config, Temp),
        string(Model),
        integer(Tokens),
        number(Temp)
    )).

% Test output parsers exist (new format)
test(schemas_are_wellformed) :-
    forall(component(GolemId, output_parser, Parser), (
        atom(Parser)
    )).

% Test dynamic docstring generation
test(docstrings_generate_correctly) :-
    forall(entity(golem(GolemId)), (
        docstring(golem(GolemId), DocString),
        atom_string(DocString, DocStr),
        % Docstring should contain key sections
        sub_atom(DocStr, _, _, _, 'Model:')
    )).

% === CONJURE SPELL TESTS ===

test(golem_task_constructor_exists) :-
    component(conjure, ctor, golem_task), !.

test(thought_constructor_exists) :-
    component(conjure, ctor, thought), !.

% === PYTHON BRIDGE TESTS ===

test(python_bridge_exports) :-
    current_predicate(python_bridge:get_golem_tools/2),
    current_predicate(python_bridge:execute_golem_task/3),
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
    component(golem(code_assistant), config, Config), !,
    get_dict(system_prompt, Config, Prompt),
    atom_string(Prompt, PromptStr),
    sub_atom(PromptStr, _, _, _, 'software engineer'),
    get_dict(model, Config, Model),
    sub_atom(Model, _, _, _, 'anthropic').

test(project_manager_configuration) :-
    component(golem(project_manager), config, Config), !,
    get_dict(system_prompt, Config, Prompt),
    atom_string(Prompt, PromptStr),
    sub_atom(PromptStr, _, _, _, 'project manager'),
    get_dict(model, Config, Model),
    sub_atom(Model, _, _, _, 'openai').

test(test_runner_configuration) :-
    component(golem(test_runner), config, Config), !,
    get_dict(system_prompt, Config, Prompt),
    atom_string(Prompt, PromptStr),
    sub_atom(PromptStr, _, _, _, 'QA engineer'),
    get_dict(base_url, Config, BaseUrl),
    sub_atom(BaseUrl, _, _, _, 'localhost').

% === DELEGATION HIERARCHY TESTS ===

test(delegation_relationships) :-
    component(golem(code_assistant), can_delegate_to, golem(test_runner)), !,
    component(golem(project_manager), can_delegate_to, golem(code_assistant)), !,
    component(golem(test_runner), supervisor, golem(project_manager)), !.

% === DOCSTRING TESTS ===

test(main_docstring) :-
    docstring(golems, Doc), !,
    atom_string(Doc, DocStr),
    once(sub_atom(DocStr, _, _, _, 'AI Agent Framework')),
    once(sub_atom(DocStr, _, _, _, 'Pydantic AI')).

:- end_tests(golems).

% === HELPER PREDICATES ===

validate_schema_term(optional(Term)) :- !, validate_schema_term(Term).
validate_schema_term(Term) :-
    Term =.. [Type, ArgName],
    atom(Type),
    atom(ArgName).

is_valid_config(Config) :-
    is_dict(Config),
    get_dict(model, Config, _),
    get_dict(max_tokens, Config, _),
    get_dict(temperature, Config, _),
    get_dict(system_prompt, Config, _).

% Helper for conditional tests
python_available :-
    catch(
        py_call('sys':version, _),
        _,
        fail
    ).