:- use_module(library(plunit)).

% Note: project/semantics.pl already loaded by grimoire.pl
% ECS predicates (entity/1, component/3, etc.) are multifile and globally available

:- begin_tests(project_semantics).

% Test basic project entity existence
test(project_entity_exists, [true]) :-
    entity(project), !.

test(mkproject_entity_exists, [true]) :-
    entity(mkproject), !.

test(conjure_mkproject_entity_exists, [true]) :-
    entity(conjure(mkproject)), !.

% Test project command constructors
test(project_command_constructors, [true]) :-
    component(conjure, ctor, mkproject), !.

% Test project context entities
test(project_contexts, [true]) :-
    entity(context(build)), !,
    entity(context(runtime)), !,
    entity(context(test)), !.

% Test project docstrings exist
test(project_docstrings_exist, [true]) :-
    docstring(project, _), !,
    docstring(mkproject, _), !.

% Test application types
test(application_types, [true]) :-
    component(application, ctor, program), !,
    component(application, ctor, library), !,
    component(application, ctor, service), !.

% Test config and deps relationships
test(config_deps_relationships, [true]) :-
    entity(config), !,
    entity(deps), !,
    % Test that contexts require config and deps
    component(context(build), requires, config(build)), !,
    component(context(build), requires, deps(build)), !.

% Test mkproject implementation exists
test(mkproject_implementation, [true]) :-
    % Test that mkproject conjure exists
    component(conjure, ctor, mkproject), !.

% Test project discovery utilities exist
test(project_discovery_utilities, [true]) :-
    component(project, utility, discover_project_artifacts), !,
    component(project, utility, discover_core_artifacts), !,
    component(project, utility, discover_nix_targets), !.

:- end_tests(project_semantics).
