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
    load_entity(semantic(file('src/prolog/nix/templates/cpp/semantics.pl'))),

    % Verify project type identification
    component(cpp_template, project_type, cpp),
    component(cpp_template, build_system, nix),
    component(cpp_template, language, cpp),

    % Verify command integration
    component(command, ctor, cpp_template).

test(cpp_project_subcommands) :-
    load_entity(semantic(file('src/prolog/nix/templates/cpp/semantics.pl'))),

    % Verify all expected subcommands
    component(cpp_template, subcommand, run), !,
    component(cpp_template, subcommand, build), !,
    component(cpp_template, subcommand, develop), !.

test(cpp_project_docstrings) :-
    load_entity(semantic(file('src/prolog/nix/templates/cpp/semantics.pl'))),

    % Verify main docstring exists and mentions C++
    docstring(cpp_template, MainDoc),
    sub_string(MainDoc, _, _, _, "C++"), !,

    % Verify subcommand docstrings use Nix
    docstring(cpp_template(run), RunDoc),
    sub_string(RunDoc, _, _, _, "nix run"), !,

    docstring(cpp_template(build), BuildDoc),
    sub_string(BuildDoc, _, _, _, "nix build"), !.

:- end_tests(cpp_project_semantics).
