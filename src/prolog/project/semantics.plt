:- use_module(library(plunit)).
:- ensure_loaded('semantics.pl').

:- begin_tests(project_semantics).

% Test basic project entity existence
test(project_entity_exists, [true]) :-
    entity(project).

% Test project command constructors
test(project_command_constructors, [true]) :-
    component(command, ctor, mkproject).

% Test project context entities
test(project_contexts, [true]) :-
    entity(context(build)),
    entity(context(runtime)),
    entity(context(test)).

% Test project docstrings exist
test(project_docstrings_exist, [true]) :-
    docstring(project, _).
    % Note: mkproject docstring might depend on other modules

% Test application types
test(application_types, [true]) :-
    component(application, ctor, program),
    component(application, ctor, library),
    component(application, ctor, service).

% Test config and deps relationships
test(config_deps_relationships, [true]) :-
    entity(config),
    entity(deps),
    % Test that contexts require config and deps
    component(context(build), requires, config(build)),
    component(context(build), requires, deps(build)).

:- end_tests(project_semantics).
