%! C++ template test suite
%
% Tests for C++ project template semantics, including project detection,
% command integration, and Nix build system support.

:- use_module(library(plunit)).
:- use_module(library(filesex)).

:- begin_tests(cpp_project_semantics).

% Test that a loaded C++ project has essential components
test(cpp_project_has_essentials) :-
    % Load the cpp template directly
    once(load_entity(semantic(file('src/nix/templates/cpp/semantics.pl')))),

    % Verify project type identification
    once(component(cpp_template, project_type, cpp)),
    once(component(cpp_template, build_system, nix)),
    once(component(cpp_template, language, cpp)),

    % Verify command integration
    once(component(conjure, ctor, cpp_template)).

test(cpp_project_subcommands) :-
    once(load_entity(semantic(file('src/nix/templates/cpp/semantics.pl')))),

    % Verify all expected subcommands
    once(component(cpp_template, subcommand, run)),
    once(component(cpp_template, subcommand, build)),
    once(component(cpp_template, subcommand, develop)).

test(cpp_project_docstrings) :-
    once(load_entity(semantic(file('src/nix/templates/cpp/semantics.pl')))),

    % Verify main docstring exists and mentions C++
    once((docstring(cpp_template, MainDoc), sub_string(MainDoc, _, _, _, "C++"))),

    % Verify subcommand docstrings use Nix
    once((docstring(cpp_template(run), RunDoc), sub_string(RunDoc, _, _, _, "nix run"))),

    once((docstring(cpp_template(build), BuildDoc), sub_string(BuildDoc, _, _, _, "nix build"))).

:- end_tests(cpp_project_semantics).
