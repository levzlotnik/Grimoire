% Interface domain - clean Python-Prolog bridge for system introspection
% All operations exposed as spells with template-based parameter filling

:- self_entity(interface).

%% ============================================================================
%% TEMPLATE FILLING SYSTEM
%% ============================================================================

% fill_input_template(+Template, +UserDict, -FilledTerm)
% Recursively fill template placeholders from user dictionary
% Template variables are atoms starting with uppercase (Entity, Type, Value, etc.)
% Dict keys MUST be quoted atoms like 'Entity', 'Type', etc.

fill_input_template(Atom, UserDict, Value) :-
    atom(Atom),
    is_placeholder_var(Atom),
    !,
    % Look up placeholder in dict - keys are quoted atoms
    (   get_dict(Atom, UserDict, Value)
    ->  true
    ;   throw(error(missing_template_arg(Atom),
                    context(fill_input_template/3, 'Template variable not provided in args')))
    ).

fill_input_template(Atom, _UserDict, Atom) :-
    atom(Atom), !.

fill_input_template(String, _UserDict, String) :-
    string(String), !.

fill_input_template(Number, _UserDict, Number) :-
    number(Number), !.

fill_input_template(List, UserDict, FilledList) :-
    is_list(List), !,
    maplist({UserDict}/[Item, Filled]>>fill_input_template(Item, UserDict, Filled), List, FilledList).

fill_input_template(Compound, UserDict, Filled) :-
    compound(Compound), !,
    Compound =.. [Functor|Args],
    maplist({UserDict}/[Arg, FilledArg]>>fill_input_template(Arg, UserDict, FilledArg), Args, FilledArgs),
    Filled =.. [Functor|FilledArgs].

% Detect placeholder variables - atoms starting with uppercase letter
is_placeholder_var(Atom) :-
    atom(Atom),
    atom_chars(Atom, [FirstChar|_]),
    char_type(FirstChar, upper).

%% ============================================================================
%% UNIVERSAL SPELL CASTING WITH TEMPLATE FILLING
%% ============================================================================

% python_magic_cast(+SpellSig, +UserDict, -PyResult)
% Universal spell casting for Python clients:
% 1. Get input template from spell metadata
% 2. Fill template with user dictionary
% 3. Cast filled spell
% 4. Convert result to Python-friendly dict

python_magic_cast(SpellSigStr, UserDict, PyResult) :-
    catch(
        (   % Step 0: Parse spell signature string to term
            (   atom(SpellSigStr)
            ->  atom_to_term(SpellSigStr, SpellSig, [])
            ;   SpellSig = SpellSigStr
            ),
            % Step 1: Extract verb (perceive/conjure) from signature
            SpellSig =.. [Verb|_],
            % Step 2: Get input format template
            please_verify(component(SpellSig, format_input, input(InputFormat))),
            % Step 3: Fill template with user dictionary
            fill_input_template(InputFormat, UserDict, FilledArgs),
            % Step 4: Reconstruct full spell term with verb
            FullSpellTerm =.. [Verb, FilledArgs],
            % Step 5: Cast the spell
            magic_cast(FullSpellTerm, Result),
            % Step 6: Convert to Python dict
            term_struct_to_python_dict(Result, PyResult), !
        ),
        Error,
        (   % Extract just the error type, ignore context
            (   Error = error(ErrorType, _)
            ->  SimplifiedError = error(ErrorType)
            ;   SimplifiedError = Error
            ),
            % Convert simplified error
            term_struct_to_python_dict(SimplifiedError, PyResult)
        )
    ).

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
        ->  throw(error(existence_error(component, component(EntityValue, TypeValue, _)),
                       context(perceive(interface(components)), 'Component does not exist')))
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

register_spell(
    conjure(interface(test)),
    input(interface(test(args('Args')))),
    output(either(ok(tests_passed), error(tests_failed('Reason')))),
    "Run test suite with optional args",
    [],
    implementation(conjure(interface(test(args(ArgsValue)))), Result, (
        catch(
            (   grimoire_ensure_loaded('@/src/run_tests.pl'),
                (   member('--list', ArgsValue)
                ->  (list_available_tests, Result = ok(tests_listed))
                ;   ArgsValue = []
                ->  (run_all_tests, Result = ok(tests_passed))
                ;   (run_specific_tests(ArgsValue), Result = ok(tests_passed))
                )
            ),
            Error,
            Result = error(tests_failed(Error))
        )
    ))
).

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
        magic_cast(perceive(sauce_me(spell(SpellCtorValue))), Result)
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

%% ============================================================================
%% PYTHON INTERFACE SUPPORT - TERM CONVERSION
%% ============================================================================

% Convert Prolog term structures to Python dictionaries recursively
% Used by python_magic_cast/3 to make results Python-friendly
term_struct_to_python_dict(Term, Dict) :-
    % Handle empty list first
    (   Term == [] ->
        Dict = _{type: "list", elements: []}
    % Handle primitive types
    ;   atomic(Term) ->
        (   atom(Term) ->
            Dict = _{type: "atom", value: Term}
        ;   string(Term) ->
            Dict = _{type: "string", value: Term}
        ;   number(Term) ->
            (   integer(Term) ->
                Dict = _{type: "int", value: Term}
            ;   Dict = _{type: "float", value: Term}
            )
        )
    % Handle non-empty lists
    ;   is_list(Term) ->
        maplist(term_struct_to_python_dict, Term, Elements),
        Dict = _{type: "list", elements: Elements}
    % Handle py-tagged dicts - pass through directly to Python via janus
    ;   is_dict(Term, py) ->
        Dict = Term
    % Handle compound terms
    ;   compound(Term) ->
        compound_name_arity(Term, Functor, Arity),
        Term =.. [Functor|Args],
        maplist(term_struct_to_python_dict, Args, ConvertedArgs),
        Dict = _{type: "term_struct", functor: Functor, arity: Arity, args: ConvertedArgs}
    % Fallback for any other types
    ;   Dict = _{type: "unknown", value: Term}
    ).
