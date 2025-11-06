% Interface domain - clean Python-Prolog bridge for system introspection
% All operations exposed as spells receiving janus.Term from Python

:- self_entity(interface).

%% ============================================================================
%% OUR OWN TERM TO JSON CONVERSION
%% ============================================================================

% our_own_term_to_json(+Term, -Json)
% Convert Prolog term to JSON structure following documented format:
% - Atom: {"type":"atom", "value":<string>}
% - String: {"type":"string", "value":<string>}
% - Integer: {"type":"integer", "value":<integer>}
% - Float: {"type":"float", "value":<float>}
% - List: JSON array (recursive)
% - Dict: {"type":"dict", "value":<object>} (tag ignored, values recursive)
% - Compound: {"type":"compound", "functor":<string>, "args":<array>}

our_own_term_to_json(Term, Json) :-
    atom(Term), !,
    Json = json{type: "atom", value: Term}.

our_own_term_to_json(Term, Json) :-
    string(Term), !,
    Json = json{type: "string", value: Term}.

our_own_term_to_json(Term, Json) :-
    integer(Term), !,
    Json = json{type: "integer", value: Term}.

our_own_term_to_json(Term, Json) :-
    float(Term), !,
    Json = json{type: "float", value: Term}.

our_own_term_to_json(Term, JsonArray) :-
    is_list(Term), !,
    maplist(our_own_term_to_json, Term, JsonArray).

our_own_term_to_json(Term, Json) :-
    is_dict(Term), !,
    dict_pairs(Term, _Tag, Pairs),
    maplist(pair_to_json_value, Pairs, JsonPairs),
    dict_create(ValueDict, json, JsonPairs),
    Json = json{type: "dict", value: ValueDict}.

our_own_term_to_json(Term, Json) :-
    compound(Term), !,
    Term =.. [Functor|Args],
    maplist(our_own_term_to_json, Args, JsonArgs),
    Json = json{type: "compound", functor: Functor, args: JsonArgs}.

% Helper for dict pair conversion
pair_to_json_value(Key-Value, Key-JsonValue) :-
    our_own_term_to_json(Value, JsonValue).

%% ============================================================================
%% SPELL INVOCATION FROM PYTHON
%% ============================================================================

% python_magic_cast(+SpellTerm, -PyResult)
% Receives fully-constructed janus.Term from Python, casts it, returns JSON result
python_magic_cast(SpellTerm, PyResult) :-
    magic_cast(SpellTerm, Result),
    our_own_term_to_json(Result, PyResult).

%% ============================================================================
%% INTERFACE OPERATIONS - ECS INTROSPECTION
%% ============================================================================

% Helper: format component results with smart singleton/set detection
format_component_result([], set([])).
format_component_result([V], unique(V)) :- !.
format_component_result(Vs, set(Vs)).

% Component types - list all component types for entity
register_spell(
    perceive(interface(component_types)),
    input(interface(component_types(entity('Entity')))),
    output(ok(types('Types'))),
    "List all component types for an entity",
    [],
    implementation(perceive(interface(component_types(entity(EntityValue)))), Result, (
        findall(Type, component(EntityValue, Type, _), AllTypes),
        sort(AllTypes, Types),
        Result = ok(types(Types))
    ))
).

% Components - get verified components with singleton/set detection
register_spell(
    perceive(interface(components)),
    input(interface(components(entity('Entity'), type('Type')))),
    output(either(ok(unique('Value')), ok(set('Values')), error(component_not_found))),
    "Get verified components with smart singleton/set detection",
    [],
    implementation(perceive(interface(components(entity(EntityValue), type(TypeValue)))), Result, (
        get_all_components(component(EntityValue, TypeValue, _), Values),
        (   Values = []
        ->  Result = error(component_not_found(component(EntityValue, TypeValue)))
        ;   format_component_result(Values, FormattedResult),
            Result = ok(FormattedResult)
        )
    ))
).

% Docstring - get entity documentation
register_spell(
    perceive(interface(docstring)),
    input(interface(docstring(entity('Entity')))),
    output(ok(doc('Doc'))),
    "Get entity docstring",
    [],
    implementation(perceive(interface(docstring(entity(EntityValue)))), Result, (
        docstring(EntityValue, Doc),
        Result = ok(doc(Doc))
    ))
).

% Entities - list all entities in system
register_spell(
    perceive(interface(entities)),
    input(interface(entities)),
    output(ok(entities('Entities'))),
    "List all entities in the system",
    [],
    implementation(perceive(interface(entities)), Result, (
        findall(E, entity(E), Entities),
        Result = ok(entities(Entities))
    ))
).

%% ============================================================================
%% INTERFACE OPERATIONS - TESTING
%% ============================================================================

% Direct predicate for test command (NOT a spell - this is system infrastructure)
% This is called directly from Python to avoid setting in_magic_cast flag
% which would interfere with test execution that validates cast_impl guards
interface_test(Args, Result) :-
    catch(
        (   grimoire_ensure_loaded('@/src/run_tests.pl'),
            (   member('--list', Args)
            ->  (list_available_tests, Result = ok(tests_listed))
            ;   Args = []
            ->  (run_all_tests, Result = ok(tests_passed))
            ;   (run_specific_tests(Args), Result = ok(tests_passed))
            )
        ),
        Error,
        Result = error(tests_failed(Error))
    ).

% Python bridge for test command - converts result to JSON
python_interface_test(Args, PyResult) :-
    interface_test(Args, Result),
    our_own_term_to_json(Result, PyResult).

% Exec - execute arbitrary Prolog query and return string results
register_spell(
    conjure(interface(exec)),
    input(interface(exec(query('QueryString')))),
    output(ok(solutions('Solutions'))),
    "Execute arbitrary Prolog query and return string-formatted solutions",
    [],
    implementation(conjure(interface(exec(query(QueryString)))), Result, (
        term_string(Goal, QueryString, [variable_names(VarNames)]),
        findall(VarNames, call(Goal), AllSolutions),
        % Convert to Python-tagged dicts for serialization
        maplist(solution_to_py_dict, AllSolutions, PySolutions),
        Result = ok(solutions(PySolutions))
    ))
).

% Helper to convert variable bindings to Python dict tagged with py
% Creates py{VarName: Value, ...} from variable bindings
solution_to_py_dict(VarNames, PyDict) :-
    maplist(binding_to_pair, VarNames, Pairs),
    dict_create(PyDict, py, Pairs).

binding_to_pair(Var=Val, VarAtom-ValStr) :-
    % Var is already an atom like 'X', use it directly as dict key
    % Convert Val to string for value
    (atom(Var) -> VarAtom = Var ; atom_string(VarAtom, Var)),
    term_string(Val, ValStr).

%% ============================================================================
%% INTERFACE OPERATIONS - SESSION MANAGEMENT
%% ============================================================================

register_spell(
    conjure(interface(session_create)),
    input(interface(session_create(session_id('SessionId')))),
    output('SessionResult'),
    "Create a new session (delegates to session domain)",
    [],
    implementation(conjure(interface(session_create(session_id(SessionIdValue)))), Result, (
        magic_cast(conjure(session(create(id(SessionIdValue)))), Result)
    ))
).

register_spell(
    conjure(interface(session_switch)),
    input(interface(session_switch(session_id('SessionId')))),
    output('SessionResult'),
    "Switch to different session (delegates to session domain)",
    [],
    implementation(conjure(interface(session_switch(session_id(SessionIdValue)))), Result, (
        magic_cast(conjure(session(switch(id(SessionIdValue)))), Result)
    ))
).

register_spell(
    conjure(interface(session_delete)),
    input(interface(session_delete(session_id('SessionId')))),
    output('SessionResult'),
    "Delete a session (delegates to session domain)",
    [],
    implementation(conjure(interface(session_delete(session_id(SessionIdValue)))), Result, (
        magic_cast(conjure(session(delete(id(SessionIdValue)))), Result)
    ))
).

register_spell(
    conjure(interface(session_export)),
    input(interface(session_export(session_id('SessionId'), destination('Dest')))),
    output('SessionResult'),
    "Export session to archive (delegates to session domain)",
    [],
    implementation(conjure(interface(session_export(session_id(SessionIdValue), destination(DestValue)))), Result, (
        magic_cast(conjure(session(export(id(SessionIdValue), destination(DestValue)))), Result)
    ))
).

register_spell(
    conjure(interface(session_import)),
    input(interface(session_import(archive('Archive')))),
    output('SessionResult'),
    "Import session from archive (delegates to session domain)",
    [],
    implementation(conjure(interface(session_import(archive(ArchiveValue)))), Result, (
        magic_cast(conjure(session(import(archive(ArchiveValue)))), Result)
    ))
).

register_spell(
    conjure(interface(session_focus_entity)),
    input(interface(session_focus_entity(entity('Entity')))),
    output('FocusResult'),
    "Focus on entity by name (delegates to session domain)",
    [],
    implementation(conjure(interface(session_focus_entity(entity(EntityValue)))), Result, (
        magic_cast(conjure(session(focus_entity(entity(EntityValue)))), Result)
    ))
).

register_spell(
    conjure(interface(session_focus_path)),
    input(interface(session_focus_path(path('Path')))),
    output('FocusResult'),
    "Focus on entity by path (delegates to session domain)",
    [],
    implementation(conjure(interface(session_focus_path(path(PathValue)))), Result, (
        magic_cast(conjure(session(focus_path(path(PathValue)))), Result)
    ))
).

register_spell(
    perceive(interface(session_focused)),
    input(interface(session_focused)),
    output('FocusedResult'),
    "Get focused entity with structured information (delegates to session domain)",
    [],
    implementation(perceive(interface(session_focused)), Result, (
        magic_cast(perceive(session(focused)), Result)
    ))
).

register_spell(
    conjure(interface(session_unfocus)),
    input(interface(session_unfocus)),
    output('UnfocusResult'),
    "Clear focused entity (delegates to session domain)",
    [],
    implementation(conjure(interface(session_unfocus)), Result, (
        magic_cast(conjure(session(unfocus)), Result)
    ))
).

register_spell(
    perceive(interface(session_status)),
    input(interface(session_status)),
    output('StatusResult'),
    "Get session status including focused entity (delegates to session domain)",
    [],
    implementation(perceive(interface(session_status)), Result, (
        magic_cast(perceive(session(status)), Result)
    ))
).

register_spell(
    perceive(interface(session_context)),
    input(interface(session_context)),
    output(ok(session('SessionId'), focused('FocusInfo'), common_activity('Activities'))),
    "Get comprehensive session context for LLM state recovery (delegates to session domain)",
    [],
    implementation(perceive(interface(session_context)), Result, (
        magic_cast(perceive(session(context)), Result)
    ))
).

%% ============================================================================
%% INTERFACE OPERATIONS - SKILL SYSTEM
%% ============================================================================

register_spell(
    conjure(interface(invoke_skill)),
    input(interface(invoke_skill(entity('Entity'), skill('SkillTerm')))),
    output('SkillResult'),
    "Invoke a skill on an entity (delegates to invoke_skill spell)",
    [],
    implementation(conjure(interface(invoke_skill(entity(EntityValue), skill(SkillTermValue)))), Result, (
        magic_cast(conjure(invoke_skill(entity(EntityValue), skill(SkillTermValue))), Result)
    ))
).

register_spell(
    perceive(interface(skills)),
    input(interface(skills(entity('Entity')))),
    output('SkillsList'),
    "List all available skills for an entity (delegates to skills perception)",
    [],
    implementation(perceive(interface(skills(entity(EntityValue)))), Result, (
        magic_cast(perceive(skills(entity(EntityValue))), Result)
    ))
).

%% ============================================================================
%% INTERFACE OPERATIONS - PROJECT INITIALIZATION
%% ============================================================================

register_spell(
    conjure(interface(init)),
    input(interface(init(folder('Folder'), options('Options')))),
    output('InitResult'),
    "Initialize Grimoire for existing project (delegates to project domain)",
    [],
    implementation(conjure(interface(init(folder(Folder), options(Options)))), Result, (
        magic_cast(conjure(project(init(folder(Folder), options(Options)))), Result)
    ))
).

%% ============================================================================
%% INTERFACE OPERATIONS - META-INTROSPECTION
%% ============================================================================

register_spell(
    perceive(interface(prove_it)),
    input(interface(prove_it(entity('Entity'), type('Type'), value('Value')))),
    output('ProofResult'),
    "Component provenance - where generated and how verified (delegates to prove_it spell)",
    [],
    implementation(perceive(interface(prove_it(entity(EntityValue), type(TypeValue), value(ValueTerm)))), Result, (
        magic_cast(perceive(prove_it(component(EntityValue, TypeValue, ValueTerm))), Result)
    ))
).

register_spell(
    perceive(interface(sauce_me)),
    input(interface(sauce_me(spell_ctor('SpellCtor')))),
    output('SauceResult'),
    "Spell metadata - source location, implementation, formats (delegates to sauce_me spell)",
    [],
    implementation(perceive(interface(sauce_me(spell_ctor(SpellCtorValue)))), Result, (
        % Parse string to term if needed
        (atom(SpellCtorValue)
        -> term_string(SpellCtorTerm, SpellCtorValue)
        ; SpellCtorTerm = SpellCtorValue),
        magic_cast(perceive(sauce_me(spell(SpellCtorTerm))), Result)
    ))
).

register_spell(
    perceive(interface(system_instructions)),
    input(interface(system_instructions)),
    output(ok(instructions('Instructions'))),
    "Get system instructions/prompt for AI agents",
    [],
    implementation(perceive(interface(system_instructions)), Result, (
        docstring(system, Instructions),
        Result = ok(instructions(Instructions))
    ))
).

