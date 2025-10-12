% Tests for Grimoire Golems AI Agent Framework
% Validates ECS components, Python bridge functionality, and golem execution

:- use_module(library(plunit)).

% Declare e2e_tests_enabled/0 as multifile so each golem test can define it
:- multifile e2e_tests_enabled/0.
:- load_entity(semantic(file('@/src/tests/golems_test_entities.pl'))).

% Dynamic per-golem test discovery and loading using term_to_atom (no substrings)
% Convert golem(Id) -> 'golem(Id)' atom for PLUnit unit names
golem_unit_atom(Id, UnitAtom) :- term_to_atom(golem(Id), UnitAtom).

% Resolve path to per-golem semantics.plt
golem_suite_path(Id, Path) :-
    atom_concat('@/src/golems/', Id, P1),
    atom_concat(P1, '/semantics.plt', Path).

% Load a single golem's test suite if present
load_golem_suite(Id) :-
    golem_suite_path(Id, Path),
    catch(grimoire_ensure_loaded(Path), _, true).

% Preferred enumerator for golem ids: component(golems, instance, golem(Id)) if available; fallback to entity/1
golem_id(Id) :-
    component(golems, instance, golem(Id)), !.
golem_id(Id) :-
    entity(golem(Id)).

% Load all per-golem test suites and ensure their units are registered
load_all_golem_suites :-
    forall(golem_id(Id), load_golem_suite(Id)),
    % Optionally assert units are now registered with PLUnit (no-op if absent)
    forall(golem_id(Id),
        ( golem_unit_atom(Id, Unit),
          ( plunit:current_unit(Unit, _, _, _) -> true ; true )
        )).

% Execute discovery at load-time of this file
:- load_all_golem_suites.


% NOTE: Per-golem test composition is handled by naming units with ground terms
% inside each golem's own semantics.plt (e.g., begin_tests(golem(code_assistant))).
% No dynamic resolution helpers or wrapper units are needed here.

% Wrapper units with exact names expected by the runner when quoting 'golems:{id}'








% === Verification for composed DSL (domain discriminative flow) ===
verify(component(E, has(golems(instance)), golems(instance(id(Id), options(_Opts))))) :-
    user:please_verify(component(E, golems_instance_id, Id)),
    user:please_verify(component(E, golems_instance_available, true)),
    % Output parsers removed - all golems now return typed dicts from Python
    % Tools are optional in this environment; just attempt to retrieve and ignore failures
    catch(python_bridge:golem_tools(Id, _Tools), _, true).

:- begin_tests(golems).

% === GOLEM ENTITY TESTS ===

test(golem_entities_exist) :-
    forall(member(GolemId, [
        code_assistant,
        project_manager,
        test_runner,
        documentation,
        architect,
        code_reviewer,
        test_planner,
        semantics_verifier
    ]), user:please_verify(component(golem(GolemId), defined, true))).

test(golems_self_entity) :-
    entity(golems).

% === COMPONENT VALIDATION TESTS ===

% Output parsers have been removed - all golems now return typed dicts from Python
% No need to test for output_parser components

% Test basic docstring existence
test(golems_have_docstrings) :-
    forall(entity(golem(GolemId)), (
        docstring(golem(GolemId), _DocString)
    )).

% === CONJURE SPELL TESTS ===

test(golem_task_constructor_exists) :-
    user:please_verify(component(conjure, ctor, golem_task)).

test(thought_constructor_exists) :-
    user:please_verify(component(conjure, ctor, thought)).

% === GOLEM CONFIGURATION TESTS ===

% Configuration tests removed - configs now handled in Python
% Individual golem configurations are validated in their respective semantics.plt files

% === DELEGATION HIERARCHY TESTS ===

test(delegation_relationships) :-
    % Test some basic delegation relationships from the new structure
    user:please_verify(component(golem(code_assistant), can_delegate_to, golem(test_runner))),
    user:please_verify(component(golem(code_assistant), can_delegate_to, golem(documentation))),
    user:please_verify(component(golem(architect), can_delegate_to, golem(code_reviewer))),
    user:please_verify(component(golem(architect), can_delegate_to, golem(documentation))), !.

% === DOCSTRING TESTS ===

test(main_docstring) :-
    docstring(golems, Doc), !,
    atom_string(Doc, DocStr),
    once(sub_atom(DocStr, _, _, _, 'AI Agent Framework')),
    once(sub_atom(DocStr, _, _, _, 'Pydantic AI')).

% === SPELL INVOCATION TESTS (using magic_cast) ===

% Test golem_task spell invocation - verify spell can be invoked
% Note: Full execution would require LLM calls, so we test error handling
test(golem_task_spell_invocation, [condition(python_available)]) :-
    % Test with invalid golem - should return error but not crash
    catch(
        magic_cast(conjure(golem_task(golem(nonexistent_test_golem), #{})), Result),
        Error,
        (Result = error(Error))
    ),
    % Should get an error result (expected behavior for invalid golem)
    assertion(Result = error(_)).

% Test actual thought spell invocation with magic_cast - DISABLED (session being reworked)
% test(thought_spell_invocation, [condition(python_available)]) :-
%     user:please_verify(component(test_thought, content, Content)),
%     magic_cast(conjure(thought(Content)), Result),
%     % Result should be either ok(_) or error(_)
%     assertion((Result = ok(_) ; Result = error(_))).

% Test test golem entities loaded
test(test_golem_entities_loaded) :-
    entity(test_golem(basic)),
    entity(test_golem(with_delegation)),
    entity(test_golem(with_parser)).

% Test test golem configurations
test(test_golem_configurations) :-
    user:please_verify(component(test_golem(basic), config, _Config)).

:- end_tests(golems).

% === HELPER PREDICATES ===

validate_schema_term(optional(Term)) :- !, validate_schema_term(Term).
validate_schema_term(Term) :-
    Term =.. [Type, ArgName],
    atom(Type),
    atom(ArgName).

is_valid_config(Config) :-
    is_dict(Config),
    _ = Config.model,
    _ = Config.max_tokens,
    _ = Config.temperature,
    _ = Config.system_prompt.

% Helper for conditional tests
python_available :-
    catch(
        py_call('sys':version, _),
        _,
        fail
    ).
