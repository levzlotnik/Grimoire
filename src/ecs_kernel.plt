:- use_module(library(plunit)).

% === DISCRIMINATIVE FLOW: VERIFICATION INFRASTRUCTURE ===
% This is the .plt (test/verification) side of the dual ECS kernel.
% Here we define please_verify/1 and the verify/1 extension point.

% Verification predicates and hook infrastructure
:- dynamic([
    verify/1,
    please_verify/1,
    verify_pre_hook/2,
    verify_post_hook/2
], [
    discontiguous(true),
    multifile(true)
]).

% === PLEASE_VERIFY/1: COMPOSABLE VERIFICATION PRIMITIVE ===

% Case 1: Component exists, is grounded, verify/1 exists and succeeds with hooks
please_verify(component(A, B, C)) :-
    component(A, B, C),                    % Component exists (may unify vars)
    ground(component(A, B, C)),            % NOW ground after unification
    verify_pre_hooks(component(A, B, C)),
    verify(component(A, B, C)), !,         % Try domain verification
    verify_post_hooks(component(A, B, C)).

% Case 2: Component exists and is grounded - trust generative flow
please_verify(component(A, B, C)) :-
    component(A, B, C),                    % Component exists
    ground(component(A, B, C)).            % Ground after unification

% Case 3: Component does not exist - verification error
please_verify(component(A, B, C)) :-
    \+ component(A, B, C),  % Only throw if component truly doesn't exist
    format(string(E), "Component not found: ~w", [component(A, B, C)]),
    throw(verification_error(missing_component, E)).

% Hook execution helpers
verify_pre_hooks(Term) :-
    forall(verify_pre_hook(Term, Goal), call(Goal)).

verify_post_hooks(Term) :-
    forall(verify_post_hook(Term, Goal), call(Goal)).

% === BASELINE VERIFICATIONS ===
% Domain-specific verify/1 implementations for ECS kernel components

% Verify docstring component
verify(component(Entity, docstring, Doc)) :-
    % Already proven to exist via please_verify
    % Verify docstring is non-empty
    (atom(Doc), Doc \= '' ->
        true
    ; string(Doc), Doc \= "" ->
        true
    ;
        throw(verification_error(empty_docstring, Entity))
    ).

% Verify self component
verify(component(_Entity, self, semantic(Source))) :-
    % Already proven to exist via please_verify
    % Verify the semantic source actually exists
    (Source = file(Path) ->
        (exists_file(Path) ->
            true
        ;
            throw(verification_error(missing_semantic_file, Path))
        )
    ; Source = folder(Path) ->
        (exists_directory(Path) ->
            true
        ;
            throw(verification_error(missing_semantic_folder, Path))
        )
    ;
        throw(verification_error(invalid_semantic_source, Source))
    ).

% === PLUNIT TESTS ===

:- begin_tests(ecs_kernel).

% Test please_verify/1 Case 1: Component exists + verify succeeds
test(please_verify_with_verify_succeeds, [setup(setup_test_component), cleanup(cleanup_test_component)]) :-
    % Component exists and has verify/1 rule
    please_verify(component(test_entity(ecs_kernel), test_prop, test_value)).

% Test please_verify/1 Case 2: Component exists but no verify/1
test(please_verify_without_verify_succeeds, [setup(setup_simple_component), cleanup(cleanup_simple_component)]) :-
    % Component exists, no verify/1, should succeed deterministically
    please_verify(component(simple_entity, simple_prop, simple_value)), !.

% Test please_verify/1 Case 3: Component does not exist
test(please_verify_missing_component_throws, [throws(verification_error(missing_component, _))]) :-
    please_verify(component(nonexistent, foo, bar)).

% Test grounding requirement - ungrounded variables throw error
test(please_verify_ungrounded_throws, [throws(verification_error(missing_component, _))]) :-
    please_verify(component(_, ungrounded, _)).

% Test hook infrastructure
test(verify_hooks_execute, [setup(setup_hook_test), cleanup(cleanup_hook_test)]) :-
    % Hook should have executed and set the flag
    please_verify(component(hook_entity, hook_prop, hook_value)),
    hook_executed(pre),
    hook_executed(post).

% Test baseline verification: defined component
test(verify_defined_component) :-
    % entity(component) exists in ecs_kernel.pl
    please_verify(component(component, defined, true)).

% Test baseline verification: docstring component
test(verify_docstring_component) :-
    % docstring(entity, ...) exists in ecs_kernel.pl
    please_verify(component(entity, docstring, _)).

% Test baseline verification: self component
test(verify_self_component, [setup(setup_self_entity), cleanup(cleanup_self_entity)]) :-
    % Create entity with self component pointing to existing file
    user:grimoire_root(Root),
    atomic_list_concat([Root, '/src/ecs_kernel.pl'], ExistingFile),
    please_verify(component(self_test_entity, self, semantic(file(ExistingFile)))).

% Test verify/1 extension by domain - empty docstring should fail
test(verify_empty_docstring_fails, [
    setup(setup_empty_docstring),
    cleanup(cleanup_empty_docstring),
    throws(verification_error(empty_docstring, _))
]) :-
    please_verify(component(empty_doc_entity, docstring, '')).

% Test verify/1 for missing semantic file
test(verify_missing_semantic_file, [
    setup(setup_missing_file),
    cleanup(cleanup_missing_file),
    throws(verification_error(missing_semantic_file, _))
]) :-
    please_verify(component(missing_file_entity, self, semantic(file('/nonexistent/file.pl')))).

:- end_tests(ecs_kernel).

% === TEST SETUP/CLEANUP HELPERS ===

% Setup for component with verify/1
setup_test_component :-
    user:assertz(entity(test_entity(ecs_kernel))),
    user:assertz(component(test_entity(ecs_kernel), test_prop, test_value)),
    user:assertz(verify(component(test_entity(ecs_kernel), test_prop, test_value))).

cleanup_test_component :-
    user:retractall(entity(test_entity(ecs_kernel))),
    user:retractall(component(test_entity(ecs_kernel), test_prop, test_value)),
    user:retractall(verify(component(test_entity(ecs_kernel), test_prop, test_value))).

% Setup for simple component without verify/1
setup_simple_component :-
    user:assertz(entity(simple_entity)),
    user:assertz(component(simple_entity, simple_prop, simple_value)).

cleanup_simple_component :-
    user:retractall(entity(simple_entity)),
    user:retractall(component(simple_entity, simple_prop, simple_value)).

% Setup for hook test
:- dynamic hook_executed/1.

setup_hook_test :-
    retractall(hook_executed(_)),
    user:assertz(entity(hook_entity)),
    user:assertz(component(hook_entity, hook_prop, hook_value)),
    user:assertz(verify_pre_hook(component(hook_entity, hook_prop, hook_value), assertz(hook_executed(pre)))),
    user:assertz(verify_post_hook(component(hook_entity, hook_prop, hook_value), assertz(hook_executed(post)))),
    user:assertz(verify(component(hook_entity, hook_prop, hook_value))).

cleanup_hook_test :-
    user:retractall(entity(hook_entity)),
    user:retractall(component(hook_entity, hook_prop, hook_value)),
    user:retractall(verify_pre_hook(component(hook_entity, hook_prop, hook_value), _)),
    user:retractall(verify_post_hook(component(hook_entity, hook_prop, hook_value), _)),
    user:retractall(verify(component(hook_entity, hook_prop, hook_value))),
    retractall(hook_executed(_)).

% Setup for self entity test
setup_self_entity :-
    user:grimoire_root(Root),
    atomic_list_concat([Root, '/src/ecs_kernel.pl'], ExistingFile),
    user:assertz(entity(self_test_entity)),
    user:assertz(component(self_test_entity, self, semantic(file(ExistingFile)))).

cleanup_self_entity :-
    user:retractall(entity(self_test_entity)),
    user:retractall(component(self_test_entity, self, semantic(file(_)))).

% Setup for empty docstring test
setup_empty_docstring :-
    user:assertz(entity(empty_doc_entity)),
    user:assertz(docstring(empty_doc_entity, '')).

cleanup_empty_docstring :-
    user:retractall(entity(empty_doc_entity)),
    user:retractall(docstring(empty_doc_entity, _)).

% Setup for missing file test
setup_missing_file :-
    user:assertz(entity(missing_file_entity)),
    user:assertz(component(missing_file_entity, self, semantic(file('/nonexistent/file.pl')))).

cleanup_missing_file :-
    user:retractall(entity(missing_file_entity)),
    user:retractall(component(missing_file_entity, self, semantic(file(_)))).
