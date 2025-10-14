:- use_module(library(plunit)).

% Load test entities from file
:- load_entity(semantic(file('@/src/tests/ecs_kernel_test_entities.pl'))).

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
test(please_verify_with_verify_succeeds) :-
    % Component exists and has verify/1 rule
    user:please_verify(component(test_entity(ecs_kernel), test_prop, test_value)).

% Test please_verify/1 Case 2: Component exists but no verify/1
test(please_verify_without_verify_succeeds) :-
    % Component exists, no verify/1, should succeed deterministically
    user:please_verify(component(simple_entity, simple_prop, simple_value)), !.

% Test please_verify/1 Case 3: Component does not exist
test(please_verify_missing_component_throws, [throws(verification_error(missing_component, _))]) :-
    user:please_verify(component(nonexistent, foo, bar)).

% Test grounding requirement - ungrounded variables throw error
test(please_verify_ungrounded_throws, [throws(verification_error(missing_component, _))]) :-
    user:please_verify(component(_, ungrounded, _)).

% Test hook infrastructure
test(verify_hooks_execute, [setup(clear_hook_files), cleanup(clear_hook_files)]) :-
    % Hook should have executed and written to files
    user:please_verify(component(hook_entity, hook_prop, hook_value)),
    exists_file('/tmp/hook_pre_executed'),
    exists_file('/tmp/hook_post_executed').

% Helper for hook tests - clean up temp files before/after
clear_hook_files :-
    (exists_file('/tmp/hook_pre_executed') -> delete_file('/tmp/hook_pre_executed') ; true),
    (exists_file('/tmp/hook_post_executed') -> delete_file('/tmp/hook_post_executed') ; true).

% Test baseline verification: defined component
test(verify_defined_component) :-
    % entity(component) exists in ecs_kernel.pl
    user:please_verify(component(component, defined, true)).

% Test baseline verification: docstring component
test(verify_docstring_component) :-
    % docstring(entity, ...) exists in ecs_kernel.pl
    user:please_verify(component(entity, docstring, _)).

% Test baseline verification: self component
test(verify_self_component) :-
    % test_test_entity loaded from file
    user:please_verify(component(self_test_entity, self, semantic(file(_)))).

% Test verify/1 extension by domain - empty docstring should fail
test(verify_empty_docstring_fails, [
    throws(verification_error(empty_docstring, _))
]) :-
    user:please_verify(component(empty_doc_entity, docstring, '')).

% Test verify/1 for missing semantic file
test(verify_missing_semantic_file, [
    throws(verification_error(missing_semantic_file, _))
]) :-
    user:please_verify(component(missing_file_entity, self, semantic(file('/nonexistent/file.pl')))).

:- end_tests(ecs_kernel).
