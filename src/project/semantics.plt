:- use_module(library(plunit)).
:- load_entity(semantic(file('@/src/tests/project_test_entities.pl'))).

% Note: project/semantics.pl already loaded by grimoire.pl
% ECS predicates (entity/1, component/3, etc.) are multifile and globally available

:- begin_tests(project).

% === VERIFY PREDICATES ===

% Verify project app component and validate type
verify(component(Entity, has(project(app)), project(app(Options)))) :-
    please_verify(component(Entity, project_type, Type)),
    member(Type, [web_service, cli_tool, library, package]).

% Verify project type is valid
verify(component(Entity, project_type, Type)) :-
    member(Type, [web_service, cli_tool, library, package]).

% Verify git repository component
verify(component(Entity, has(git(repository)), git(repository(origin(Origin))))) :-
    atom(Origin).

% Verify nix flake component
verify(component(Entity, has(nix(flake)), nix(flake(ref(Ref))))) :-
    atom(Ref).

% === TESTS ===

% Test basic project entity existence
test(project_entity_exists, [true]) :-
    entity(project), !.

test(mkproject_entity_exists, [true]) :-
    entity(mkproject), !.

test(conjure_mkproject_entity_exists, [true]) :-
    entity(conjure(mkproject)), !.

% Test project command constructors
test(project_command_constructors, [true]) :-
    please_verify(component(conjure, ctor, mkproject)), !.

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
    please_verify(component(application, ctor, program)), !,
    please_verify(component(application, ctor, library)), !,
    please_verify(component(application, ctor, service)), !.

% Test config and deps relationships
test(config_deps_relationships, [true]) :-
    entity(config), !,
    entity(deps), !,
    % Test that contexts require config and deps
    please_verify(component(context(build), requires, config(build))), !,
    please_verify(component(context(build), requires, deps(build))), !.

% Test mkproject implementation exists
test(mkproject_implementation, [true]) :-
    % Test that mkproject conjure exists
    please_verify(component(conjure, ctor, mkproject)), !.

% Test DSL expansion rules work
test(project_dsl_expansion, [true]) :-
    please_verify(component(test_web_app, project_type, web_service)), !,
    please_verify(component(test_web_app, project_git_origin, 'https://github.com/test/web-app.git')), !,
    please_verify(component(test_web_app, has(git(repository)), git(repository(origin('https://github.com/test/web-app.git'))))), !.

:- end_tests(project).
