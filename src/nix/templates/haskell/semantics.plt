%! Haskell template test suite
%
% Tests for Haskell project template semantics, including project detection,
% command integration, and Nix build system support.

:- use_module(library(plunit)).
:- use_module(library(filesex)).

:- begin_tests(haskell_project_semantics).

% Test that a loaded Haskell project has essential components
test(haskell_project_has_essentials) :-
    % Load the haskell template directly
    once(load_entity(semantic(file('@/src/nix/templates/haskell/semantics.pl')))),

    % Verify project type identification
    once(component(haskell_template, project_type, haskell)),
    once(component(haskell_template, build_system, nix)),
    once(component(haskell_template, language, haskell)),

    % Verify command integration
    once(component(conjure, ctor, haskell_template)).

test(haskell_project_subcommands) :-
    once(load_entity(semantic(file('@/src/nix/templates/haskell/semantics.pl')))),

    % Verify all expected subcommands
    once(component(haskell_template, subcommand, build)),
    once(component(haskell_template, subcommand, test)),
    once(component(haskell_template, subcommand, run)),
    once(component(haskell_template, subcommand, ghci)),
    once(component(haskell_template, subcommand, hlint)).

test(haskell_project_docstrings) :-
    once(load_entity(semantic(file('@/src/nix/templates/haskell/semantics.pl')))),

    % Verify docstrings exist for main entity and subcommands
    once((docstring(haskell_template, _))),
    once((docstring(haskell_template(build), _))),
    once((docstring(haskell_template(test), _))),
    once((docstring(haskell_template(run), _))),
    once((docstring(haskell_template(ghci), _))),
    once((docstring(haskell_template(hlint), _))).

test(haskell_project_patterns) :-
    once(load_entity(semantic(file('@/src/nix/templates/haskell/semantics.pl')))),

    % Verify filesystem patterns for Haskell detection
    once(component(haskell_template, file_pattern, glob("*.hs"))),
    once(component(haskell_template, file_pattern, glob("*.cabal"))),
    once(component(haskell_template, discovery_pattern, include(_))),
    once(component(haskell_template, discovery_pattern, exclude(_))).

test(haskell_nix_command_delegation) :-
    once(load_entity(semantic(file('@/src/nix/templates/haskell/semantics.pl')))),

    % Verify that Haskell commands delegate to Nix
    % This tests the Nix-centric approach
    once(component(haskell_template, build_system, nix)),
    once(component(haskell_template, subcommand, build)).

:- end_tests(haskell_project_semantics).
