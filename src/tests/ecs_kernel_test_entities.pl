% Test entities for ecs_kernel tests
:- self_entity(test_entities(ecs_kernel), "Test entities for ECS kernel verification tests").

:- discontiguous verify/1.
:- multifile verify_pre_hook/2.
:- multifile verify_post_hook/2.

% Test entity with verify/1 rule
entity(test_entity(ecs_kernel)).
component(test_entity(ecs_kernel), test_prop, test_value).
verify(component(test_entity(ecs_kernel), test_prop, test_value)).

% Simple entity without verify/1
entity(simple_entity).
component(simple_entity, simple_prop, simple_value).

% Hook test entity
entity(hook_entity).
component(hook_entity, hook_prop, hook_value).
user:verify_pre_hook(component(hook_entity, hook_prop, hook_value),
    (open('/tmp/hook_pre_executed', write, S), close(S))).
user:verify_post_hook(component(hook_entity, hook_prop, hook_value),
    (open('/tmp/hook_post_executed', write, S), close(S))).
verify(component(hook_entity, hook_prop, hook_value)).

% Self entity test - points to actual ecs_kernel.pl file
entity(self_test_entity).
component(self_test_entity, self, semantic(file(FilePath))) :-
   grimoire_resolve_path('@/src/ecs_kernel.pl', FilePath).

% Empty docstring test entity
entity(empty_doc_entity).
docstring(empty_doc_entity, '').

% Missing file test entity
entity(missing_file_entity).
component(missing_file_entity, self, semantic(file('/nonexistent/file.pl'))).
