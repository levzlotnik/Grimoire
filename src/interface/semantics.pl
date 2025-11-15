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

% Helper: format component results with smart singleton/set detection (deprecated - use format_component_result_with_status/3)
format_component_result([], set([])).
format_component_result([V], unique(V)) :- !.
format_component_result(Vs, set(Vs)).

% Helper: format component results with verification status
% unique(V) only when exactly one value total AND it's verified (no broken)
% set(Vs) in all other cases
format_component_result_with_status(Verified, Broken, components(VerifiedFormat, BrokenFormat)) :-
    length(Verified, VLen),
    length(Broken, BLen),
    (VLen =:= 1, BLen =:= 0
    -> (Verified = [V], VerifiedFormat = verified(unique(V)))
    ; VerifiedFormat = verified(set(Verified))),
    BrokenFormat = broken(Broken).

% Component types - list all component types for entity
register_spell(
    perceive(interface(component_types)),
    input(perceive(interface(component_types(entity(Entity:term))))),
    output(ok(types(Types:list(term)))),
    "List all component types for an entity",
    [],
    implementation(perceive(interface(component_types(entity(Entity)))), Result, (
        normalize_entity_arg(Entity, NormalizedEntity),
        findall(Type, component(NormalizedEntity, Type, _), AllTypes),
        sort(AllTypes, Types),
        Result = ok(types(Types))
    ))
).

% Components - get verified and broken components with singleton/set detection
register_spell(
    perceive(interface(components)),
    input(perceive(interface(components(entity(Entity:term), type(Type:term))))),
    output(either(
        ok(components(verified(either(unique(_Value:term), set(_Values:list(term)))), broken(_Broken:list(term)))),
        error(component_not_found, _Context:term)
    )),
    "Get verified and broken components with smart singleton/set detection and verification status",
    [],
    implementation(perceive(interface(components(entity(Entity), type(Type)))), Result, (
        normalize_entity_arg(Entity, NormalizedEntity),
        catch(
            (ask(component(NormalizedEntity, Type, _), Verified, Broken),
             (Verified = [], Broken = []
             -> throw(error(component_not_found(component(Entity, Type)), context(interface(components), 'Component not found')))
             ; (format_component_result_with_status(Verified, Broken, FormattedResult),
                Result = ok(FormattedResult)))),
            error(Reason, Context),
            Result = error(Reason, Context)
        )
    ))
).

% Docstring - get entity documentation
register_spell(
    perceive(interface(docstring)),
    input(perceive(interface(docstring(entity(Entity:term))))),
    output(ok(doc(Doc:string))),
    "Get entity docstring",
    [],
    implementation(perceive(interface(docstring(entity(Entity)))), Result, (
        normalize_entity_arg(Entity, NormalizedEntity),
        docstring(NormalizedEntity, Doc),
        Result = ok(doc(Doc))
    ))
).

% Entities - list all entities in system
register_spell(
    perceive(interface(entities)),
    input(perceive(interface(entities))),
    output(ok(entities(Entities:list(entity)))),
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
    input(conjure(interface(exec(query(QueryString:string))))),
    output(ok(solutions(Solutions:list(term)))),
    "Execute arbitrary Prolog query and return string-formatted solutions",
    [],
    implementation(conjure(interface(exec(query(QueryString)))), Result, (
        term_string(Goal, QueryString, [variable_names(VarNames)]),
        findall(VarNames, call(Goal), AllSolutions),
        % Convert to Python-tagged dicts for serialization
        maplist(solution_to_py_dict, AllSolutions, Solutions),
        Result = ok(solutions(Solutions))
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
    input(conjure(interface(session_create(session_id(SessionId:atom))))),
    output(Result:term),
    "Create a new session (delegates to session domain)",
    [],
    implementation(conjure(interface(session_create(session_id(SessionId)))), Result, (
        magic_cast(conjure(session(create(id(SessionId)))), Result)
    ))
).

register_spell(
    conjure(interface(session_switch)),
    input(conjure(interface(session_switch(session_id(SessionId:atom))))),
    output(Result:term),
    "Switch to different session (delegates to session domain)",
    [],
    implementation(conjure(interface(session_switch(session_id(SessionId)))), Result, (
        magic_cast(conjure(session(switch(id(SessionId)))), Result)
    ))
).

register_spell(
    conjure(interface(session_delete)),
    input(conjure(interface(session_delete(session_id(SessionId:atom))))),
    output(Result:term),
    "Delete a session (delegates to session domain)",
    [],
    implementation(conjure(interface(session_delete(session_id(SessionId)))), Result, (
        magic_cast(conjure(session(delete(id(SessionId)))), Result)
    ))
).

register_spell(
    conjure(interface(session_export)),
    input(conjure(interface(session_export(session_id(SessionId:atom), destination(Dest:atom))))),
    output(Result:term),
    "Export session to archive (delegates to session domain)",
    [],
    implementation(conjure(interface(session_export(session_id(SessionId), destination(Dest)))), Result, (
        magic_cast(conjure(session(export(id(SessionId), destination(Dest)))), Result)
    ))
).

register_spell(
    conjure(interface(session_import)),
    input(conjure(interface(session_import(archive(Archive:atom))))),
    output(Result:term),
    "Import session from archive (delegates to session domain)",
    [],
    implementation(conjure(interface(session_import(archive(Archive)))), Result, (
        magic_cast(conjure(session(import(archive(Archive)))), Result)
    ))
).

register_spell(
    conjure(interface(session_focus_entity)),
    input(conjure(interface(session_focus_entity(entity(Entity:term))))),
    output(Result:term),
    "Focus on entity by name (delegates to session domain)",
    [],
    implementation(conjure(interface(session_focus_entity(entity(Entity)))), Result, (
        normalize_entity_arg(Entity, NormalizedEntity),
        magic_cast(conjure(session(focus_entity(entity(NormalizedEntity)))), Result)
    ))
).

register_spell(
    conjure(interface(session_focus_path)),
    input(conjure(interface(session_focus_path(path(Path:atom))))),
    output(Result:term),
    "Focus on entity by path (delegates to session domain)",
    [],
    implementation(conjure(interface(session_focus_path(path(Path)))), Result, (
        magic_cast(conjure(session(focus_path(path(Path)))), Result)
    ))
).

register_spell(
    perceive(interface(session_focused)),
    input(perceive(interface(session_focused))),
    output(Result:term),
    "Get focused entity with structured information (delegates to session domain)",
    [],
    implementation(perceive(interface(session_focused)), Result, (
        magic_cast(perceive(session(focused)), Result)
    ))
).

register_spell(
    conjure(interface(session_unfocus)),
    input(conjure(interface(session_unfocus))),
    output(Result:term),
    "Clear focused entity (delegates to session domain)",
    [],
    implementation(conjure(interface(session_unfocus)), Result, (
        magic_cast(conjure(session(unfocus)), Result)
    ))
).

register_spell(
    perceive(interface(session_status)),
    input(perceive(interface(session_status))),
    output(Result:term),
    "Get session status including focused entity (delegates to session domain)",
    [],
    implementation(perceive(interface(session_status)), Result, (
        magic_cast(perceive(session(status)), Result)
    ))
).

register_spell(
    perceive(interface(session_context)),
    input(perceive(interface(session_context))),
    output(Result:term),
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
    input(conjure(interface(invoke_skill(entity(Entity:term), skill(SkillTerm:term))))),
    output(Result:term),
    "Invoke a skill on an entity (delegates to invoke_skill spell)",
    [],
    implementation(conjure(interface(invoke_skill(entity(EntityInput), skill(SkillTerm)))), Result, (
        normalize_entity_arg(EntityInput, Entity),
        magic_cast(conjure(invoke_skill(entity(Entity), skill(SkillTerm))), Result)
    ))
).

register_spell(
    perceive(interface(skills)),
    input(perceive(interface(skills(entity(Entity:term))))),
    output(Result:term),
    "List all available skills for an entity (delegates to skills perception)",
    [],
    implementation(perceive(interface(skills(entity(EntityInput)))), Result, (
        normalize_entity_arg(EntityInput, Entity),
        magic_cast(perceive(skills(entity(Entity))), Result)
    ))
).

%% ============================================================================
%% INTERFACE OPERATIONS - PROJECT INITIALIZATION
%% ============================================================================

register_spell(
    conjure(interface(init)),
    input(conjure(interface(init(folder(Folder:atom), options(Options:term))))),
    output(Result:term),
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
    input(perceive(interface(prove_it(entity(Entity:term), type(Type:term), value(Value:term))))),
    output(Result:term),
    "Component provenance - where generated and how verified (delegates to prove_it spell)",
    [],
    implementation(perceive(interface(prove_it(entity(EntityInput), type(Type), value(Value)))), Result, (
        normalize_entity_arg(EntityInput, Entity),
        magic_cast(perceive(prove_it(component(Entity, Type, Value))), Result)
    ))
).

register_spell(
    perceive(interface(sauce_me)),
    input(perceive(interface(sauce_me(spell_ctor(SpellCtor:term))))),
    output(Result:term),
    "Spell metadata - source location, implementation, formats (delegates to sauce_me spell)",
    [],
    implementation(perceive(interface(sauce_me(spell_ctor(SpellCtor)))), Result, (
        % Parse string to term if needed
        (atom(SpellCtor)
        -> term_string(SpellCtorTerm, SpellCtor)
        ; SpellCtorTerm = SpellCtor),
        magic_cast(perceive(sauce_me(spell(SpellCtorTerm))), Result)
    ))
).

register_spell(
    perceive(interface(system_instructions)),
    input(perceive(interface(system_instructions))),
    output(ok(instructions(Instructions:string))),
    "Get system instructions/prompt for AI agents",
    [],
    implementation(perceive(interface(system_instructions)), Result, (
        docstring(system, Instructions),
        Result = ok(instructions(Instructions))
    ))
).

%% ============================================================================
%% INTERFACE OPERATIONS - CRUD COMPONENT OPERATIONS
%% ============================================================================

% Helper: Resolve entity name, handling focused_entity special case
normalize_entity_arg(EntityInput, Entity) :-
    (string(EntityInput) ->
        (catch(term_string(Entity, EntityInput), _, atom_string(Entity, EntityInput)))
    ; Entity = EntityInput).

resolve_entity_name(focused_entity, ActualEntity) :- !,
    catch(
        (current_session_id(SessionId),
         please_verify(component(session(SessionId), focused_entity, ActualEntity))),
        _,
        fail
    ).
resolve_entity_name(EntityInput, Entity) :-
    normalize_entity_arg(EntityInput, Entity).

% Add component - smart wrapper that resolves focused_entity
register_spell(
    conjure(interface(add_component)),
    input(conjure(interface(add_component(entity(Entity:term), component_type(Type:term), value(Value:term))))),
    output(either(
        ok(component_added(component_type(Type:term), value(Value:term))),
        error(add_error(Reason:term), Context:term)
    )),
    "Add component to entity (resolves focused_entity from session)",
    [],
    implementation(conjure(interface(add_component(entity(Entity), component_type(Type), value(Value)))), Result, (
        catch(
            (resolve_entity_name(Entity, ActualEntity)
            -> magic_cast(conjure(add_component(entity(ActualEntity), component_type(Type), value(Value))), Result)
            ; throw(error(no_focused_entity, context(interface(add_component), 'No focused entity')))),
            error(Reason, Context),
            Result = error(add_error(Reason), Context)
        )
    ))
).

% Remove component - smart wrapper that resolves focused_entity
register_spell(
    conjure(interface(remove_component)),
    input(conjure(interface(remove_component(entity(Entity:term), component_type(Type:term), value(Value:term))))),
    output(either(
        ok(component_removed(component_type(Type:term), value(Value:term))),
        error(remove_error(Reason:term), Context:term)
    )),
    "Remove component from entity (resolves focused_entity from session)",
    [],
    implementation(conjure(interface(remove_component(entity(Entity), component_type(Type), value(Value)))), Result, (
        catch(
            (resolve_entity_name(Entity, ActualEntity)
            -> magic_cast(conjure(remove_component(entity(ActualEntity), component_type(Type), value(Value))), Result)
            ; throw(error(no_focused_entity, context(interface(remove_component), 'No focused entity')))),
            error(Reason, Context),
            Result = error(remove_error(Reason), Context)
        )
    ))
).
