% Load ecs_kernel.pl to get operators, please_verify, verify, etc.
:- load_files('ecs_kernel.pl', [if(not_loaded)]).

%% ============================================================================
%% Test Component Definitions - MUST BE BEFORE begin_tests
%% ============================================================================
% These are defined in the user module scope, not inside the plunit module

% Simple leaf verification test component
% The :: syntax automatically generates verify/1 clauses with error handling
component(_, test_leaf_atom, Value)
    :: atom(Value),
       Value \= ''.

% Composite expansion test component
component(E, has(test_composite), test_composite(Name, Type))
    ==> component(E, test_name, Name),
        component(E, test_type, Type)
    ::  atom(Name),
        atom(Type).

% Verify clauses for expanded leaf components
component(_, test_name, Name)
    :: atom(Name).

component(_, test_type, Type)
    :: atom(Type).

% Conditional expansion test component
component(E, has(test_optional), test_optional(Options))
    ==> component(E, test_required, req),
        (component(E, test_optional_field, F) :- member(field(F), Options))
    ::  is_list(Options).

% Verify clauses for conditional expansion leaf components
component(_, test_required, req)
    :: true.

component(_, test_optional_field, Field)
    :: atom(Field).

% DSL schema registration test
register_dsl_schema(
    test_domain,
    has(test_domain(schema)),
    signature(test_domain(schema(field('Field')))),
    "Test DSL schema for ecs_kernel.plt verification",
    (
        component(E, has(test_domain(schema)), test_domain(schema(field(Field))))
            ==> component(E, test_domain_field, Field)
            ::  atom(Field)
    )
).

% Verify clause for DSL schema expanded component
component(_, test_domain_field, Field)
    :: atom(Field).

%% ============================================================================
%% Test Entity Facts - STATIC, before begin_tests
%% ============================================================================

entity(test_entity_1).
entity(test_entity_2).
entity(test_entity_3).
entity(test_entity_4).
entity(test_entity_5).
entity(test_entity_6).
entity(test_entity_7).
entity(test_entity_8).
entity(test_entity_9).
entity(test_entity_dsl).
entity(test_entity_dsl_2).

% Test entities for leaf verification
component(test_entity_1, test_leaf_atom, hello).
component(test_entity_2, test_leaf_atom, '').
component(test_entity_2, intentionally_broken_please_ignore, [test_leaf_atom]).

% Test entities for composite expansion
component(test_entity_3, has(test_composite), test_composite(foo, bar)).
component(test_entity_4, has(test_composite), test_composite(baz, qux)).

% Test entities for conditional expansion
component(test_entity_5, has(test_optional), test_optional([field(value)])).
component(test_entity_6, has(test_optional), test_optional([])).
component(test_entity_7, has(test_optional), test_optional([])).

% Test entity for please_verify fetching
component(test_entity_8, test_leaf_atom, fetched_value).

% Test entity for verification failure (compound is not atom)
component(test_entity_9, test_domain_field, compound(term)).
component(test_entity_9, intentionally_broken_please_ignore, [test_domain_field]).

% Test entities for DSL schema
component(test_entity_dsl, has(test_domain(schema)), test_domain(schema(field(test_value)))).
component(test_entity_dsl_2, has(test_domain(schema)), test_domain(schema(field(another_value)))).

%% ============================================================================
%% BEGIN TESTS
%% ============================================================================

:- begin_tests(ecs_kernel).

%% ============================================================================
%% Test Case 1: Operators are defined
%% ============================================================================

test(operators_defined) :-
    % Check operators exist with correct precedence
    current_op(1150, xfx, ==>),
    current_op(1160, xfx, '::').

%% ============================================================================
%% Test Case 2: Simple leaf verification
%% ============================================================================

test(leaf_verification_passes) :-
    user:please_verify(component(test_entity_1, test_leaf_atom, hello)).

test(leaf_verification_fails_on_empty, [throws(_)]) :-
    user:please_verify(component(test_entity_2, test_leaf_atom, '')).

%% ============================================================================
%% Test Case 3: Composite expansion with ==>
%% ============================================================================

test(composite_expansion_generates_components) :-
    user:please_verify(component(test_entity_3, test_name, N)),
    assertion(N = foo),
    user:please_verify(component(test_entity_3, test_type, T)),
    assertion(T = bar).

test(composite_verification_composes) :-
    user:please_verify(component(test_entity_4, has(test_composite), test_composite(baz, qux))).

%% ============================================================================
%% Test Case 4: Conditional expansion
%% ============================================================================

test(conditional_expansion_when_present) :-
    user:please_verify(component(test_entity_5, test_required, R)),
    assertion(R = req),
    user:please_verify(component(test_entity_5, test_optional_field, F)),
    assertion(F = value).

test(conditional_expansion_when_absent) :-
    user:please_verify(component(test_entity_6, test_required, R)),
    assertion(R = req),
    \+ user:component(test_entity_6, test_optional_field, _).

test(conditional_verification_skips_absent) :-
    user:please_verify(component(test_entity_7, has(test_optional), test_optional([]))).

%% ============================================================================
%% Test Case 5: please_verify grounding checks
%% ============================================================================

test(please_verify_requires_grounded_entity, [throws(error(instantiation_error(_), _))]) :-
    user:please_verify(component(_UnboundEntity, test_leaf_atom, foo)).

test(please_verify_requires_grounded_type, [throws(error(instantiation_error(_), _))]) :-
    user:please_verify(component(some_entity, _UnboundType, foo)).

test(please_verify_can_fetch_value) :-
    user:please_verify(component(test_entity_8, test_leaf_atom, V)),
    assertion(V = fetched_value).

test(please_verify_throws_on_verification_failure, [throws(error(verification_failed(_), _))]) :-
    % test_domain_field has :: atom(Field), so compound(term) will fail
    user:please_verify(component(test_entity_9, test_domain_field, compound(term))).

%% ============================================================================
%% Test Case 6: verify/1 guard prevents direct calls
%% ============================================================================

test(verify_guard_forbids_direct_call, [throws(error(direct_verify_forbidden(_), _))]) :-
    user:verify(component(anything, anything, anything)).

%% ============================================================================
%% Test Case 7: register_dsl_schema generates metadata
%% ============================================================================

test(dsl_schema_generates_ctor_metadata) :-
    user:component(dsl_schema, ctor, has(test_domain(schema))).

test(dsl_schema_generates_provided_by_metadata) :-
    user:component(has(test_domain(schema)), provided_by, Domain),
    assertion(Domain = test_domain).

test(dsl_schema_generates_signature_metadata) :-
    user:please_verify(component(has(test_domain(schema)), signature, Sig)),
    assertion(Sig = signature(test_domain(schema(field('Field'))))).

test(dsl_schema_generates_docstring_metadata) :-
    user:docstring(has(test_domain(schema)), Doc),
    assertion(Doc = "Test DSL schema for ecs_kernel.plt verification").

%% ============================================================================
%% Test Case 8: DSL schema expansion and verification
%% ============================================================================

test(dsl_schema_expands_components) :-
    user:please_verify(component(test_entity_dsl, test_domain_field, Field)),
    assertion(Field = test_value).

test(dsl_schema_verification_works) :-
    user:please_verify(component(test_entity_dsl_2, has(test_domain(schema)), test_domain(schema(field(another_value))))).

%% ============================================================================
%% Test Case 9: Core dump - System health verification
%% ============================================================================

test(system_healthy) :-
    core_dump(core_dump(verified(_), broken(BrokenOntology))),
    % Filter out intentionally broken test components
    exclude(is_intentionally_broken, BrokenOntology, RealBroken),
    RealBroken = [].

% Check if a broken component is marked as intentionally broken
is_intentionally_broken(component(Entity, ComponentType, _)-_) :-
    component(Entity, intentionally_broken_please_ignore, BrokenTypes),
    member(ComponentType, BrokenTypes).

:- end_tests(ecs_kernel).
