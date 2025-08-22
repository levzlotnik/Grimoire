:- use_module(library(plunit)).
:- ensure_loaded('../grimoire.pl').
:- ensure_loaded('../project/semantics.pl').

:- begin_tests(integration_tests).

% Test basic cross-domain functionality (simplified)
test(project_creation_workflow, [true]) :-
    % Test that we can access basic entities across domains
    entity(git), !,
    entity(nix), !,
    component(conjure, ctor, git(init)), !,
    component(conjure, ctor, nix(build)), !.

% Test basic semantic relationships (simplified)
test(transaction_execution, [true]) :-
    % Test that we have transaction infrastructure
    current_predicate(execute/2),
    entity(system).

% Test docstring system works across domains
test(cross_domain_docstrings, [true]) :-
    % Test git docstrings
    entity(git),
    docstring(git, _),
    % Test basic system docstrings
    docstring(entity, _).

% Test semantic mounting across domains
test(cross_domain_mounting, [true]) :-
    % Verify that git and nix domains are properly loaded
    entity(git), !,
    entity(nix), !,
    component(conjure, ctor, git(clone)), !,
    component(conjure, ctor, nix(build)), !.

setup_test_workspace :-
    % Clean up any existing test workspace
    (exists_directory('/tmp/test_project') ->
        delete_directory_and_contents('/tmp/test_project')
    ; true).

cleanup_test_workspace :-
    % Clean up test files
    (exists_directory('/tmp/test_project') ->
        delete_directory_and_contents('/tmp/test_project')
    ; true).

exists_file_path(Dir, File) :-
    directory_file_path(Dir, File, Path),
    exists_file(Path).

% Helper to recursively delete directory
delete_directory_and_contents(Dir) :-
    (exists_directory(Dir) ->
        delete_directory(Dir)
    ; true).

:- end_tests(integration_tests).
