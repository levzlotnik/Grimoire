%! Python template test suite
%
% Tests for Python project template semantics, including project detection,
% command integration, and Nix build system support.

:- use_module(library(plunit)).
:- use_module(library(filesex)).

:- begin_tests(python_project_semantics).

% Test that a loaded Python project has essential components
test(python_project_has_essentials) :-
    % Load the python template directly
    once(load_entity(semantic(file('@/src/nix/templates/python/semantics.pl')))),

    % Verify project type identification
    once(component(python_template, project_type, python)),
    once(component(python_template, build_system, nix)),
    once(component(python_template, language, python)),

    % Verify command integration
    once(component(conjure, ctor, python_template)).

test(python_project_subcommands) :-
    once(load_entity(semantic(file('@/src/nix/templates/python/semantics.pl')))),

    % Verify all expected subcommands
    once(component(python_template, subcommand, run)),
    once(component(python_template, subcommand, build)),
    once(component(python_template, subcommand, develop)).

test(python_project_docstrings) :-
    once(load_entity(semantic(file('@/src/nix/templates/python/semantics.pl')))),

    % Verify main docstring exists and mentions Python
    once((docstring(python_template, MainDoc), sub_string(MainDoc, _, _, _, "Python"))),

    % Verify subcommand docstrings use Nix
    once((docstring(python_template(run), RunDoc), sub_string(RunDoc, _, _, _, "nix run"))),

    once((docstring(python_template(build), BuildDoc), sub_string(BuildDoc, _, _, _, "nix build"))).

:- end_tests(python_project_semantics).
