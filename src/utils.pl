% Utils entity - General utilities for the Grimoire system
:- self_entity(utils).

% === ENTITY DECLARATIONS ===
entity(utils(tree_builder)).
entity(utils(validator)).
entity(utils(collection)).
entity(utils(core_dump)).
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
    signature(utils(tree_builder(root(Root:entity), relationship(Rel:atom), options(Options:list(term))))),
    "Build hierarchical tree from entity relationships (pure ECS traversal, no I/O)",
    (
        component(E, has(utils(tree_builder)), utils(tree_builder(root(Root), relationship(Rel), options(Options))))
            ==> component(E, utils_tree_root, Root),
                component(E, utils_tree_relationship, Rel),
                (component(E, utils_tree_max_depth, MD) :- member(max_depth(MD), Options))
            ::  ground(Root)
    )
).

% Validator schema - pure logic validation
register_dsl_schema(
    utils,
    has(utils(validator)),
    signature(utils(validator(rules(Rules:list(term)), on_error(ErrorMode:atom)))),
    "Validate entity against rules with configurable error handling (pure logic, no I/O)",
    (
        component(E, has(utils(validator)), utils(validator(rules(Rules), on_error(ErrorMode))))
            ==> component(E, utils_validation_rules, Rules),
                component(E, utils_error_handling, ErrorMode)
    )
).

% Collection schema - pure data transformation
register_dsl_schema(
    utils,
    has(utils(collection)),
    signature(utils(collection(type(Type:atom), operations(Operations:list(atom)), predicate(Predicate:term)))),
    "Transform collections using map/filter/reduce operations (pure data transformation, no I/O)",
    (
        component(E, has(utils(collection)), utils(collection(type(Type), operations(Operations), predicate(Predicate))))
            ==> component(E, utils_collection_type, Type),
                component(E, utils_collection_operations, Operations),
                component(E, utils_collection_predicate, Predicate)
            ::  callable(Predicate)
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
    input(perceive(utils(entity_hierarchy(entity(RootEntity:entity))))),
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
    input(conjure(utils(validate(entity(Entity:term), rules(Rules:term))))),
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
    input(conjure(utils(transform(data(Data:term), operation(Operation:term))))),
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
    input(perceive(core_dump)),
    output(ok(core_dump(verified('VerifiedOntology'), broken('BrokenOntology'), ignored('IgnoredOntology')))),
    "Capture complete system state as verified, broken, and ignored ontology",
    [],
    implementation(perceive(core_dump), Result, (
        core_dump(Dump),
        Result = ok(Dump)
    ))
).

% Dump core dump to TSV file
register_spell(
    conjure(core_dump_tsv),
    input(conjure(core_dump_tsv(tsv_path(TsvPath:atom)))),
    output(either(ok(dumped), error('SomeTsvError'))),
    "Write current core dump to TSV file (single file with status column)",
    [],
    implementation(conjure(core_dump_tsv(tsv_path(TsvPath))), Result, (
        catch(
            (core_dump(Dump),
             write_core_dump_to_tsv(Dump, TsvPath),
             Result = ok(dumped)),
            Error,
            Result = error(Error)
        )
    ))
).

% Read core dump from TSV file
register_spell(
    perceive(read_core_dump_tsv),
    input(perceive(read_core_dump_tsv(tsv_path(TsvPath:atom)))),
    output(either(ok(core_dump(verified('VerifiedOntology'), broken('BrokenOntology'), ignored('IgnoredOntology'))), error('SomeTsvError'))),
    "Read core dump from TSV file",
    [],
    implementation(perceive(read_core_dump_tsv(tsv_path(TsvPath))), Result, (
        catch(
            (read_core_dump_from_tsv(TsvPath, Dump),
             Result = ok(Dump)),
            Error,
            Result = error(Error)
        )
    ))
).

% === CORE DUMP HELPER IMPLEMENTATIONS ===

% Write core dump to TSV using library(csv) with tab separator
write_core_dump_to_tsv(core_dump(verified(Verified), broken(Broken), ignored(Ignored)), TsvPath) :-
    % Convert to rows: row(E, P, V, ErrorStr, Status) - all fields as atoms/strings
    findall(
        row(EStr, PStr, VStr, '', not_ignored),
        (member(component(E, P, V), Verified),
         term_string(E, EStr),
         term_string(P, PStr),
         term_string(V, VStr)),
        VerifiedRows
    ),
    findall(
        row(EStr, PStr, VStr, ErrorStr, not_ignored),
        (member(component(E, P, V)-Error, Broken),
         term_string(E, EStr),
         term_string(P, PStr),
         term_string(V, VStr),
         term_string(Error, ErrorStr)),
        BrokenRows
    ),
    findall(
        row(EStr, PStr, VStr, ErrorStr, ignored),
        (member(component(E, P, V)-Error, Ignored),
         term_string(E, EStr),
         term_string(P, PStr),
         term_string(V, VStr),
         term_string(Error, ErrorStr)),
        IgnoredRows
    ),
    append([VerifiedRows, BrokenRows, IgnoredRows], DataRows),

    % Write TSV with tab separator - header is first row
    AllRows = [row(entity, type, value, error, status) | DataRows],
    csv_write_file(TsvPath, AllRows, [
        functor(row),
        separator(0'\t),
        convert(false)
    ]).

% Read core dump from TSV using library(csv) with tab separator
read_core_dump_from_tsv(TsvPath, core_dump(verified(Verified), broken(Broken), ignored(Ignored))) :-
    % Read TSV with tab separator - no conversion, preserve as atoms
    csv_read_file(TsvPath, Rows, [functor(row), separator(0'\t), convert(false)]),

    % Partition by ErrorStr and Status columns, parse strings back to terms
    % Note: CSV reads all fields as atoms - empty fields as '', strings as atoms
    findall(
        component(E, P, V),
        (member(row(EStr, PStr, VStr, '', not_ignored), Rows),
         term_string(E, EStr),
         term_string(P, PStr),
         term_string(V, VStr)),
        Verified
    ),
    findall(
        component(E, P, V)-Error,
        (member(row(EStr, PStr, VStr, ErrorStr, not_ignored), Rows),
         ErrorStr \= '',
         term_string(E, EStr),
         term_string(P, PStr),
         term_string(V, VStr),
         term_string(Error, ErrorStr)),
        Broken
    ),
    findall(
        component(E, P, V)-Error,
        (member(row(EStr, PStr, VStr, ErrorStr, ignored), Rows),
         term_string(E, EStr),
         term_string(P, PStr),
         term_string(V, VStr),
        term_string(Error, ErrorStr)),
        Ignored
    ).

%% ============================================================================
%% COMPONENT CRUD OPERATIONS
%% ============================================================================

% Helper: Get semantics file path for an entity
entity_semantics_file(Entity, FilePath) :-
    component(Entity, self, semantic(file(FilePath))), !.
entity_semantics_file(Entity, FilePath) :-
    component(Entity, self, semantic(folder(FolderPath))),
    atomic_list_concat([FolderPath, '/semantics.pl'], FilePath).

% Add component to entity
register_spell(
    conjure(add_component),
    input(conjure(add_component(entity(Entity:entity), component_type(Type:term), value(Value:term)))),
    output(either(
        ok(component_added(component_type(Type:term), value(Value:term))),
        error(add_error('Reason'))
    )),
    "Add verified component to entity",
    [],
    implementation(conjure(add_component(entity(Entity), component_type(Type), value(Value))), Result, (
        catch(
            (% Get entity's semantics file
             entity_semantics_file(Entity, FilePath),

             % Append component to file
             format(string(ComponentStr), '~n% Added by grimoire add~ncomponent(~q, ~q, ~q).~n',
                    [Entity, Type, Value]),
             magic_cast(conjure(fs(edit_file(file(FilePath), edits([append(ComponentStr)])))), EditResult),
             (EditResult = ok(_) -> true ; throw(EditResult)),

             % Reload entity file
             reload_entity(semantic(file(FilePath))),

             % Verify the new component
             please_verify(component(Entity, Type, Value)),

             Result = ok(component_added(component_type(Type), value(Value)))),
            Exception,
            (   Exception = error(sus(verification_failed(_)), _)
            ->  Result = error(add_error(verification_failed))
            ;   term_string(Exception, ExceptionStr),
                Result = error(add_error(ExceptionStr))
            )
        )
    ))
).

% Remove component from entity (only fact components)
register_spell(
    conjure(remove_component),
    input(conjure(remove_component(entity(Entity:entity), component_type(Type:term), value(Value:term)))),
    output(either(
        ok(component_removed(component_type(Type:term), value(Value:term))),
        error(remove_error('Reason'))
    )),
    "Remove component from entity (only fact components, not derived)",
    [],
    implementation(conjure(remove_component(entity(Entity), component_type(Type), value(Value))), Result, (
        catch(
            (% Check if component exists and is deletable
             % prove_it will throw if component doesn't exist
             prove_it(component(Entity, Type, Value),
                      qed(_, generated_by(Generation), _)),

             (   Generation = fact(file(File), line(_Line))
             ->  % It's a fact, read file and remove it
                 read_file_to_terms(File, AllTerms, []),
                 ComponentTerm = component(Entity, Type, Value),
                 exclude(=(ComponentTerm), AllTerms, FilteredTerms),

                 % Check if anything was actually removed
                 length(AllTerms, OrigLen),
                 length(FilteredTerms, NewLen),
                 (OrigLen =:= NewLen
                 -> throw(error(component_not_found_in_file, _))
                 ; true),

                 % Rebuild file content
                 findall(TermStr,
                         (member(Term, FilteredTerms),
                          format(string(TermStr), '~q.~n', [Term])),
                         TermStrs),
                 atomic_list_concat(TermStrs, '', NewContent),

                 % Count lines in original file
                 read_file_to_string(File, FileContent, []),
                 split_string(FileContent, "\n", "", AllLines),
                 % Remove trailing empty lines
                 reverse(AllLines, ReversedLines),
                 (ReversedLines = [""|Rest] -> reverse(Rest, Lines) ; Lines = AllLines),
                 length(Lines, LineCount),

                 % Replace entire file content
                 (LineCount > 0
                 -> Edits = [replace(1, LineCount, NewContent)]
                 ; Edits = [append(NewContent)]),
                 magic_cast(conjure(fs(edit_file(file(File), edits(Edits)))), EditResult),
                 (EditResult = ok(_) -> true ; throw(EditResult)),

                 % Reload entity file
                 reload_entity(semantic(file(File)))

             ;   Generation = runtime_assertion
             ->  % Runtime assertion, can retract
                 retract(component(Entity, Type, Value))

             ;   % It's derived - cannot remove
                 throw(error(cannot_remove_derived_component(component(Entity, Type, Value)), _))
             ),

             Result = ok(component_removed(component_type(Type), value(Value)))),
            Exception,
            (   Exception = error(sus(component_not_found(_)), _)
            ->  Result = error(remove_error(component_not_found))
            ;   Exception = error(cannot_remove_derived_component(_), _)
            ->  Result = error(remove_error(cannot_remove_derived))
            ;   term_string(Exception, ExceptionStr),
                Result = error(remove_error(ExceptionStr))
            )
        )
    ))
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
