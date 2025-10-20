% Utils entity - General utilities for the Grimoire system
:- self_entity(utils).

% === ENTITY DECLARATIONS ===
entity(utils(tree_builder)).
entity(utils(validator)).
entity(utils(collection)).
entity(utils(core_dump)).
entity(utils(core_dump_db)).
entity(utils(core_dump_csv)).

% === DOCUMENTATION ===
docstring(utils, "
# Subsystem: Utils

## Entity Declaration
`:- self_entity(utils).`

## DSL Patterns
This entity exposes user-friendly DSL fact schemas:

```prolog
% Tree building utilities (pure ECS traversal, no I/O)
component(user_entity, has(utils(tree_builder)), utils(tree_builder(
    root(user_entity), relationship(child), options([max_depth(10)])
))).
% Generatively populates: component(user_entity, utils_tree_root, user_entity).

% Validation utilities (pure logic, no I/O)
component(user_entity, has(utils(validator)), utils(validator(
    rules([check_existence, check_format]), on_error(throw)
))).
% Generatively populates: component(user_entity, utils_validation_rules, [check_existence, check_format]).

% Collection utilities (pure data transformation, no I/O)
component(user_entity, has(utils(collection)), utils(collection(
    type(entities), operations([filter, map, reduce]), predicate(custom_filter/2)
))).
% Generatively populates: component(user_entity, utils_collection_type, entities).
```

## Verification Patterns
Users can use `please_verify/1` on fact schemas in their own `semantics.plt` files:

```prolog
please_verify(component(user_entity, has(utils(tree_builder)),
    utils(tree_builder(root(user_entity), relationship(child), options([max_depth(10)]))))).
please_verify(component(user_entity, has(utils(validator)),
    utils(validator(rules([check_existence, check_format]), on_error(throw))))).
```

## Purpose
Utils domain provides pure logic utility functions for other Grimoire domains:
- Entity hierarchy traversal and tree building (pure ECS, no I/O)
- Validation framework for component verification (pure logic)
- Collection operations (map, filter, reduce) on entity data (pure transformation)

Note: File processing utilities have been moved to the fs domain.

## Spell Constructors
- **perceive**: `entity_hierarchy(RootEntity)` - Build hierarchical tree from relationships
- **perceive**: `utils(entity_hierarchy(RootEntity))` - Build hierarchical tree from relationships
- **conjure**: `utils(validate(Entity, Rules))` - Validate entity against rules
- **conjure**: `utils(transform(Data, Operation))` - Transform data using map/filter/reduce

## Dependencies
- **ecs_kernel** (L0): Core ECS predicates and verification primitive
- **grimoire** (L1): Spell system (for magic_cast in spell implementations)
").

% === DSL SCHEMA REGISTRATIONS ===

% Tree builder schema - pure ECS traversal
register_dsl_schema(
    utils,
    has(utils(tree_builder)),
    signature(utils(tree_builder(root('Root'), relationship('Rel'), options('Options')))),
    "Build hierarchical tree from entity relationships (pure ECS traversal, no I/O)",
    (
        component(E, has(utils(tree_builder)), utils(tree_builder(root(Root), relationship(Rel), options(Options))))
            ==> component(E, utils_tree_root, Root),
                component(E, utils_tree_relationship, Rel),
                (component(E, utils_tree_max_depth, MD) :- member(max_depth(MD), Options))
            ::  ground(Root),
                atom(Rel),
                is_list(Options)
    )
).

% Validator schema - pure logic validation
register_dsl_schema(
    utils,
    has(utils(validator)),
    signature(utils(validator(rules('Rules'), on_error('ErrorMode')))),
    "Validate entity against rules with configurable error handling (pure logic, no I/O)",
    (
        component(E, has(utils(validator)), utils(validator(rules(Rules), on_error(ErrorMode))))
            ==> component(E, utils_validation_rules, Rules),
                component(E, utils_error_handling, ErrorMode)
            ::  is_list(Rules),
                atom(ErrorMode)
    )
).

% Collection schema - pure data transformation
register_dsl_schema(
    utils,
    has(utils(collection)),
    signature(utils(collection(type('Type'), operations('Operations'), predicate('Predicate')))),
    "Transform collections using map/filter/reduce operations (pure data transformation, no I/O)",
    (
        component(E, has(utils(collection)), utils(collection(type(Type), operations(Operations), predicate(Predicate))))
            ==> component(E, utils_collection_type, Type),
                component(E, utils_collection_operations, Operations),
                component(E, utils_collection_predicate, Predicate)
            ::  atom(Type),
                is_list(Operations),
                callable(Predicate)
    )
).

% === LEAF VERIFICATIONS ===

component(_, utils_tree_root, Root)
    :: ground(Root).

component(_, utils_tree_relationship, Rel)
    :: atom(Rel).

component(_, utils_tree_max_depth, MaxDepth)
    :: integer(MaxDepth),
       MaxDepth > 0.

component(_, utils_validation_rules, Rules)
    :: is_list(Rules),
       subset(Rules, [check_existence, check_format, check_permissions]).

component(_, utils_error_handling, ErrorMode)
    :: member(ErrorMode, [throw, return, log]).

component(_, utils_collection_type, Type)
    :: atom(Type).

component(_, utils_collection_operations, Operations)
    :: is_list(Operations),
       subset(Operations, [filter, map, reduce]).

component(_, utils_collection_predicate, Predicate)
    :: callable(Predicate).

% === SPELL IMPLEMENTATIONS ===

% Entity hierarchy spell (perceive) - utils namespace only
% Note: Short form perceive(entity_hierarchy) removed due to grimoire.pl term_expansion
% limitations with atomic spell domains. Use perceive(utils(entity_hierarchy(...))) instead.
register_spell(
    perceive(utils(entity_hierarchy)),
    input(utils(entity_hierarchy(entity('RootEntity')))),
    output(either(ok(hierarchy(tree('Tree'))), error(hierarchy_error('Error')))),
    "Build hierarchical tree structure from entity child relationships (pure ECS, no I/O)",
    [],
    implementation(perceive(utils(entity_hierarchy(entity(RootEntity)))), Result, (
        catch(
            (build_entity_tree(RootEntity, Tree),
             Result = ok(hierarchy(Tree))),
            Error,
            Result = error(hierarchy_error(Error))
        )
    ))
).

% Validation spell (conjure)
register_spell(
    conjure(utils(validate)),
    input(utils(validate(entity('Entity'), rules('Rules')))),
    output(either(ok(validation_passed), error(validation_error('Reason')))),
    "Validate entity against rules (pure logic, no I/O)",
    [],
    implementation(conjure(utils(validate(entity(Entity), rules(Rules)))), Result, (
        catch(
            (validate_entity_with_rules(Entity, Rules),
             Result = ok(validation_passed)),
            validation_error(_, Reason),
            Result = error(validation_error(Reason))
        )
    ))
).

% Transform data spell (conjure)
register_spell(
    conjure(utils(transform)),
    input(utils(transform(data('Data'), operation('Operation')))),
    output(either(ok(transformed('Result')), error(transform_error('Reason')))),
    "Transform data using map/filter/reduce (pure logic, no I/O)",
    [],
    implementation(conjure(utils(transform(data(Data), operation(Operation)))), Result, (
        catch(
            (apply_transformation(Data, Operation, TransformedData),
             Result = ok(transformed(TransformedData))),
            Error,
            Result = error(transform_error(Error))
        )
    ))
).

% === CORE UTILITY PREDICATES ===

% Build tree recursively from child components only
% Tree structure: tree(Entity, Children) - no docstrings to keep it lean
build_entity_tree(Entity, tree(Entity, Children)) :-
    findall(ChildTree, (
        component(Entity, child, Child),
        build_entity_tree(Child, ChildTree)
    ), Children).

% Validation helper implementations
validate_entity_with_rules(Entity, Rules) :-
    forall(member(Rule, Rules), apply_validation_rule(Entity, Rule)).

apply_validation_rule(Entity, check_existence) :-
    (entity(Entity) -> true ; throw(validation_error(utils, entity_does_not_exist(Entity)))).

apply_validation_rule(Entity, check_format) :-
    (atom(Entity), atom_length(Entity, Length), Length > 0 -> true ;
     throw(validation_error(utils, invalid_entity_format(Entity)))).

apply_validation_rule(_Entity, check_permissions) :-
    % Placeholder - domains can extend this for their specific permission checks
    true.

% Transformation operations
apply_transformation(Data, map(Predicate), Result) :-
    maplist(Predicate, Data, Result).

apply_transformation(Data, filter(Predicate), Result) :-
    include(Predicate, Data, Result).

apply_transformation(Data, reduce(Predicate, Initial), Result) :-
    foldl(Predicate, Data, Initial, Result).

% === CORE DUMP SPELLS ===

:- use_module(library(csv)).

% Perceive core dump - capture complete system state
register_spell(
    perceive(core_dump),
    input(core_dump),
    output(ok(core_dump(verified('VerifiedOntology'), broken('BrokenOntology')))),
    "Capture complete system state as verified and broken ontology",
    [],
    implementation(perceive(core_dump), Result, (
        core_dump(Dump),
        Result = ok(Dump)
    ))
).

% Dump core dump to SQLite database
register_spell(
    conjure(core_dump_db),
    input(core_dump_db(db_path('DbPath'))),
    output(either(ok(dumped), error('SomeDbError'))),
    "Write current core dump to SQLite database (single table with status column)",
    [],
    implementation(conjure(core_dump_db(db_path(DbPath))), Result, (
        catch(
            (core_dump(Dump),
             write_core_dump_to_db(Dump, DbPath),
             Result = ok(dumped)),
            Error,
            Result = error(Error)
        )
    ))
).

% Read core dump from SQLite database
register_spell(
    perceive(read_core_dump_db),
    input(read_core_dump_db(db_path('DbPath'))),
    output(either(ok(core_dump(verified('VerifiedOntology'), broken('BrokenOntology'))), error('SomeDbError'))),
    "Read core dump from SQLite database",
    [],
    implementation(perceive(read_core_dump_db(db_path(DbPath))), Result, (
        catch(
            (read_core_dump_from_db(DbPath, Dump),
             Result = ok(Dump)),
            Error,
            Result = error(Error)
        )
    ))
).

% Dump core dump to CSV file
register_spell(
    conjure(core_dump_csv),
    input(core_dump_csv(csv_path('CsvPath'))),
    output(either(ok(dumped), error('SomeCsvError'))),
    "Write current core dump to CSV file (single file with status column)",
    [],
    implementation(conjure(core_dump_csv(csv_path(CsvPath))), Result, (
        catch(
            (core_dump(Dump),
             write_core_dump_to_csv(Dump, CsvPath),
             Result = ok(dumped)),
            Error,
            Result = error(Error)
        )
    ))
).

% Read core dump from CSV file
register_spell(
    perceive(read_core_dump_csv),
    input(read_core_dump_csv(csv_path('CsvPath'))),
    output(either(ok(core_dump(verified('VerifiedOntology'), broken('BrokenOntology'))), error('SomeCsvError'))),
    "Read core dump from CSV file",
    [],
    implementation(perceive(read_core_dump_csv(csv_path(CsvPath))), Result, (
        catch(
            (read_core_dump_from_csv(CsvPath, Dump),
             Result = ok(Dump)),
            Error,
            Result = error(Error)
        )
    ))
).

% === CORE DUMP HELPER IMPLEMENTATIONS ===

% Write core dump to database using db spells
write_core_dump_to_db(core_dump(verified(Verified), broken(Broken)), DbPath) :-
    % Create database
    magic_cast(conjure(db(create(file(DbPath), schema(sql("CREATE TABLE dummy (id INTEGER);"))))), CreateResult),
    (CreateResult = ok(_) -> true ; throw(CreateResult)),

    % Convert to rows: components(E, P, V, Status)
    % Verified: status = ""
    % Broken: status = error text
    findall(
        components(E, P, V, ""),
        member(component(E, P, V), Verified),
        VerifiedRows
    ),
    findall(
        components(E, P, V, ErrorStr),
        (member(component(E, P, V)-Error, Broken),
         term_string(Error, ErrorStr)),
        BrokenRows
    ),
    append(VerifiedRows, BrokenRows, AllRows),

    % Write all rows to single table
    magic_cast(conjure(db(write_table(database(DbPath), table(components), rows(AllRows)))), WriteResult),
    (WriteResult = ok(_) -> true ; throw(WriteResult)).

% Read core dump from database using db spells
read_core_dump_from_db(DbPath, core_dump(verified(Verified), broken(Broken))) :-
    % Read all rows
    magic_cast(perceive(db(read_table(database(DbPath), table(components)))), ReadResult),
    (ReadResult = ok(rows(Rows)) -> true ; throw(ReadResult)),

    % Partition by status column
    findall(
        component(E, P, V),
        member(components(E, P, V, ""), Rows),
        Verified
    ),
    findall(
        component(E, P, V)-Error,
        (member(components(E, P, V, ErrorStr), Rows),
         ErrorStr \= "",
         term_string(Error, ErrorStr)),
        Broken
    ).

% Write core dump to CSV using library(csv)
write_core_dump_to_csv(core_dump(verified(Verified), broken(Broken)), CsvPath) :-
    % Convert to rows with status column - all fields as strings
    findall(
        row(EStr, PStr, VStr, StatusStr),
        (member(component(E, P, V), Verified),
         term_string(E, EStr),
         term_string(P, PStr),
         term_string(V, VStr),
         StatusStr = ""),
        VerifiedRows
    ),
    findall(
        row(EStr, PStr, VStr, StatusStr),
        (member(component(E, P, V)-Error, Broken),
         term_string(E, EStr),
         term_string(P, PStr),
         term_string(V, VStr),
         term_string(Error, StatusStr)),
        BrokenRows
    ),
    append(VerifiedRows, BrokenRows, AllRows),

    % Write CSV with explicit options - no conversion
    csv_write_file(CsvPath, AllRows, [functor(row), convert(false)]).

% Read core dump from CSV using library(csv)
read_core_dump_from_csv(CsvPath, core_dump(verified(Verified), broken(Broken))) :-
    % Read CSV with explicit options - no conversion, preserve strings
    csv_read_file(CsvPath, Rows, [functor(row), convert(false)]),

    % Partition by status column and parse strings back to terms
    % Note: CSV reads empty fields as empty atom '', not empty string ""
    findall(
        component(E, P, V),
        (member(row(EStr, PStr, VStr, StatusField), Rows),
         StatusField = '',  % Empty atom
         term_string(E, EStr),
         term_string(P, PStr),
         term_string(V, VStr)),
        Verified
    ),
    findall(
        component(E, P, V)-Error,
        (member(row(EStr, PStr, VStr, StatusField), Rows),
         StatusField \= '',  % Non-empty
         term_string(E, EStr),
         term_string(P, PStr),
         term_string(V, VStr),
         term_string(Error, StatusField)),
        Broken
    ).

% === ENTITY-SPECIFIC DOCSTRINGS ===
docstring(entity_hierarchy, "Build a hierarchical tree structure from entity child relationships.
    Format: perceive(entity_hierarchy(RootEntity))

    Returns a tree structure where each node is:
    tree(Entity, [ChildTree1, ChildTree2, ...])

    Only follows 'child' component relationships. Other domains can subscribe
    their components to the child pattern using rules like:
    component(Entity, child, Child) :- component(Entity, ctor, Child).

    Example:
    ?- magic_cast(perceive(entity_hierarchy(project)), Result).
    Result = ok(hierarchy(tree(project, [
        tree(project(frontend), [
            tree(project(frontend(src)), [...])
        ]),
        tree(project(backend), [])
    ]))).").

% === META-INTROSPECTION SPELLS ===

:- use_module(library(prolog_stack)).

% Prove component provenance - show where it comes from and how it's verified
register_spell(
    perceive(prove_it),
    input(prove_it(component('Entity', 'ComponentType', 'Value'))),
    output(either(
        ok(proof(
            component('Entity', 'ComponentType', 'Value'),
            generated_by('Generation'),
            discriminated_by('Verification')
        )),
        error(proof_error('Reason'))
    )),
    "Trace component provenance: where it was generated (fact or derivation) and how it's verified",
    [],
    implementation(perceive(prove_it(component(Entity, ComponentType, Value))), Result, (
        catch(
            (prove_component_provenance(component(Entity, ComponentType, Value), Proof),
             Result = ok(Proof)),
            Error,
            Result = error(proof_error(Error))
        )
    ))
).

% Spell sauce - show complete spell metadata including source location
register_spell(
    perceive(sauce),
    input(sauce(spell('SpellConstructor'))),
    output(either(
        ok(sauce(
            spell('SpellConstructor'),
            registered_at('Location'),
            implementation('ImplText'),
            input_format('Input'),
            output_format('Output'),
            docstring('Doc'),
            options('Options')
        )),
        error(sauce_error('Reason'))
    )),
    "Show complete spell metadata: where registered, implementation source, formats, docs, options",
    [],
    implementation(perceive(sauce(spell(SpellCtor))), Result, (
        catch(
            (extract_spell_sauce(SpellCtor, Sauce),
             Result = ok(Sauce)),
            Error,
            Result = error(sauce_error(Error))
        )
    ))
).

% === PROVE_IT IMPLEMENTATION ===

prove_component_provenance(component(E, C, V), Proof) :-
    % Set up trace collection
    setup_call_cleanup(
        start_component_trace(TraceId),
        catch(
            please_verify(component(E, C, V)),
            error(ErrorTerm, _Context),
            (stop_component_trace(TraceId, Events),
             extract_verification_from_trace(component(E, C, V), Events, Verification),
             Proof = error(verification_failed(component(E, C, V), ErrorTerm, Verification)))
        ),
        (var(Proof) -> stop_component_trace(TraceId, Events) ; true)
    ),
    (var(Proof) ->
        analyze_trace(Events, component(E, C, V), Proof)
    ;
        true
    ).

start_component_trace(TraceId) :-
    gensym(trace, TraceId),
    nb_setval(current_trace_id, TraceId),
    assertz((
        user:prolog_trace_interception(Port, Frame, _PC, continue) :-
            nb_getval(current_trace_id, TID),
            catch(
                (
                    prolog_frame_attribute(Frame, goal, Goal),
                    (   (Goal = component(_, _, _) ; Goal = user:component(_, _, _))
                    ->  prolog_frame_attribute(Frame, level, Level),
                        (   catch(prolog_frame_attribute(Frame, clause, Clause), _, fail)
                        ->  ClauseInfo = clause(Clause)
                        ;   ClauseInfo = no_clause
                        ),
                        recordz(TID, trace_event(Port, Level, Goal, ClauseInfo))
                    ;   (Goal = verify(_) ; Goal = user:verify(_))
                    ->  prolog_frame_attribute(Frame, level, Level),
                        (   catch(prolog_frame_attribute(Frame, clause, Clause), _, fail)
                        ->  ClauseInfo = clause(Clause)
                        ;   ClauseInfo = no_clause
                        ),
                        recordz(TID, trace_event(Port, Level, Goal, ClauseInfo))
                    ;   true
                    )
                ),
                _,
                true
            )
    )),
    trace.

stop_component_trace(TraceId, Events) :-
    notrace,
    retractall(user:prolog_trace_interception(_, _, _, _)),
    findall(Event, recorded(TraceId, Event, _Ref), Events),
    forall(recorded(TraceId, _, Ref), erase(Ref)),
    nb_delete(current_trace_id).

analyze_trace(Events, component(E, C, V), Proof) :-
    findall(
        Level-trace_event(exit, Level, Goal, ClauseInfo),
        (member(trace_event(exit, Level, Goal, ClauseInfo), Events),
         unify_goals(Goal, component(E, C, V))),
        ExitEvents
    ),
    (   ExitEvents = []
    ->  Proof = no_proof(component(E, C, V))
    ;   sort(ExitEvents, SortedExits),
        reverse(SortedExits, [TopLevel-trace_event(exit, _, SuccessGoal, ClauseInfo)|_]),
        extract_provenance(SuccessGoal, ClauseInfo, Events, TopLevel, Generation),
        extract_verification_from_trace(component(E, C, V), Events, Verification),
        Proof = proof(
            component(E, C, V),
            generated_by(Generation),
            Verification
        )
    ).

unify_goals(user:component(E1, C1, V1), component(E2, C2, V2)) :- !,
    E1 = E2, C1 = C2, V1 = V2.
unify_goals(component(E1, C1, V1), user:component(E2, C2, V2)) :- !,
    E1 = E2, C1 = C2, V1 = V2.
unify_goals(component(E1, C1, V1), component(E2, C2, V2)) :-
    E1 = E2, C1 = C2, V1 = V2.

extract_provenance(_Goal, no_clause, _Events, _TopLevel, unknown_source) :- !.

extract_provenance(Goal, clause(ClauseRef), Events, TopLevel, Generation) :-
    catch(clause_property(ClauseRef, file(File)), _, File = '<no file>'),
    catch(clause_property(ClauseRef, line_count(Line)), _, Line = 0),
    catch(clause(Goal, Body, ClauseRef), _, Body = unknown),
    (   Body = true
    ->  Generation = fact(source_location(File, Line))
    ;   Body = unknown
    ->  Generation = unknown_body(source_location(File, Line))
    ;   find_dependency(Body, Events, TopLevel, Dependency),
        Generation = derived_from(Dependency, source_location(File, Line))
    ).

find_dependency(_Body, Events, TopLevel, Dependency) :-
    findall(
        Goal,
        (member(trace_event(exit, Level, Goal, _ClauseInfo), Events),
         Level > TopLevel,
         (Goal = component(_, _, _) ; Goal = user:component(_, _, _)),
         strip_user_prefix(Goal, CleanGoal),
         CleanGoal = component(E, _, _),
         E \= component),
        ComponentCalls
    ),
    (   ComponentCalls = []
    ->  Dependency = body_goal(unknown)
    ;   ComponentCalls = [SingleDep|_]
    ->  strip_user_prefix(SingleDep, Dependency)
    ).

strip_user_prefix(user:Goal, Goal) :- !.
strip_user_prefix(Goal, Goal).

extract_verification_from_trace(Component, Events, Verification) :-
    findall(
        verify_event(Level, Goal, ClauseInfo, Port),
        (member(trace_event(Port, Level, Goal, ClauseInfo), Events),
         (Port = exit ; Port = redo(_) ; Port = exception(_)),
         ClauseInfo = clause(_),
         strip_user_prefix(Goal, CleanGoal),
         CleanGoal = verify(VerifyArg),
         VerifyArg = Component),
        VerifyEvents
    ),
    (VerifyEvents = [] ->
        Verification = no_verifier
    ;   sort(VerifyEvents, SortedVerifyEvents),
        reverse(SortedVerifyEvents, [verify_event(_, _, ClauseInfo, _Port)|_]),
        (ClauseInfo = clause(ClauseRef) ->
            catch(clause_property(ClauseRef, file(File)), _, File = '<no file>'),
            catch(clause_property(ClauseRef, line_count(Line)), _, Line = 0),
            catch(clause(verify(Component), Body, ClauseRef), _, Body = unknown),
            (Body = ((\+ in_please_verify), _, _) ->
                Verification = no_verifier
            ;   Verification = discriminated_by(
                    verifier(verify(Component) :- Body, source_location(File, Line))
                )
            )
        ;   Verification = no_verifier
        )
    ).

% === SAUCE IMPLEMENTATION ===

extract_spell_sauce(SpellCtor, Sauce) :-
    % Get all metadata from component facts (generated by register_spell)
    (   component(SpellCtor, docstring, Doc)
    ->  true
    ;   throw(error(spell_not_found(SpellCtor)))
    ),

    component(SpellCtor, format_input, Input),
    component(SpellCtor, format_output, Output),
    component(SpellCtor, spell_options, Options),
    component(SpellCtor, source_location, Location),
    component(SpellCtor, implementation, Implementation),

    Sauce = sauce(
        spell(SpellCtor),
        registered_at(Location),
        implementation(Implementation),
        input_format(Input),
        output_format(Output),
        docstring(Doc),
        options(Options)
    ).
