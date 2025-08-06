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
    load_entity(semantic(file('src/prolog/nix/templates/python/semantics.pl'))),

    % Verify project type identification
    component(python_template, project_type, python),
    component(python_template, build_system, nix),
    component(python_template, language, python),

    % Verify command integration
    component(command, ctor, python_template).

test(python_project_subcommands) :-
    load_entity(semantic(file('src/prolog/nix/templates/python/semantics.pl'))),

    % Verify all expected subcommands
    component(python_template, subcommand, run),
    component(python_template, subcommand, build),
    component(python_template, subcommand, develop).

test(python_project_docstrings) :-
    load_entity(semantic(file('src/prolog/nix/templates/python/semantics.pl'))),

    % Verify main docstring exists and mentions Python
    docstring(python_template, MainDoc),
    sub_string(MainDoc, _, _, _, "Python"),

    % Verify subcommand docstrings use Nix
    docstring(python_template(run), RunDoc),
    sub_string(RunDoc, _, _, _, "nix run"),

    docstring(python_template(build), BuildDoc),
    sub_string(BuildDoc, _, _, _, "nix build").

:- end_tests(python_project_semantics).
