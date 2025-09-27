% Tests for Grimoire Golems AI Agent Framework
% Validates ECS components, Python bridge functionality, and golem execution

:- use_module(library(plunit)).

:- begin_tests(golems).

% === GOLEM ENTITY TESTS ===

test(golem_entities_exist) :-
    entity(golem(code_assistant)),
    entity(golem(project_manager)),
    entity(golem(test_runner)),
    entity(golem(documentation)),
    entity(golem(architect)),
    entity(golem(code_reviewer)),
    entity(golem(test_planner)),
    entity(golem(semantics_verifier)).

test(golems_self_entity) :-
    entity(golems).

% === COMPONENT VALIDATION TESTS ===

% Test basic golem structure
test(golems_have_output_parsers) :-
    forall(entity(golem(GolemId)), (
        component(golem(GolemId), output_parser, _Parser)
    )).

% Test output parsers exist (new format)
test(schemas_are_wellformed) :-
    forall(component(GolemId, output_parser, Parser), (
        atom(Parser)
    )).

% Test basic docstring existence
test(golems_have_docstrings) :-
    forall(entity(golem(GolemId)), (
        docstring(golem(GolemId), _DocString)
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

% Configuration tests removed - configs now handled in Python
% Individual golem configurations are validated in their respective semantics.plt files

% === DELEGATION HIERARCHY TESTS ===

test(delegation_relationships) :-
    % Test some basic delegation relationships from the new structure
    component(golem(code_assistant), can_delegate_to, golem(test_runner)),
    component(golem(code_assistant), can_delegate_to, golem(documentation)),
    component(golem(architect), can_delegate_to, golem(code_reviewer)),
    component(golem(architect), can_delegate_to, golem(documentation)).

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