:- use_module(library(plunit)).

% Load tests from subdirectory
:- load_entity(semantic(file('@/src/project/tests/semantics.plt'))).

% Note: project/semantics.pl already loaded by grimoire.pl
% ECS predicates (entity/1, component/3, etc.) are multifile and globally available

:- begin_tests(project).

% Test basic project entity existence
test(project_entity_exists, [true]) :-
    entity(project), !.

test(mkproject_entity_exists, [true]) :-
    entity(mkproject), !.

test(conjure_mkproject_entity_exists, [true]) :-
    entity(conjure(mkproject)), !.

% Test project command constructors
test(project_command_constructors, [true]) :-
    user:please_verify(component(conjure, ctor, mkproject)), !.

% Test project context entities
test(project_contexts, [true]) :-
    entity(project(context(build))), !,
    entity(project(context(runtime))), !,
    entity(project(context(test))), !.

% Test project docstrings exist
test(project_docstrings_exist, [true]) :-
    docstring(project, _), !.

% Test mkproject implementation exists
test(mkproject_implementation, [true]) :-
    % Test that mkproject conjure exists
    user:please_verify(component(conjure, ctor, mkproject)), !.

:- end_tests(project).
