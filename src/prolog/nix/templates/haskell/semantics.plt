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
    load_entity(semantic(file('src/prolog/nix/templates/haskell/semantics.pl'))),

    % Verify project type identification
    component(haskell_template, project_type, haskell),
    component(haskell_template, build_system, nix),
    component(haskell_template, language, haskell),

    % Verify command integration
    component(command, ctor, haskell_template).

test(haskell_project_subcommands) :-
    load_entity(semantic(file('src/prolog/nix/templates/haskell/semantics.pl'))),

    % Verify all expected subcommands
    component(haskell_template, subcommand, build), !,
    component(haskell_template, subcommand, test), !,
    component(haskell_template, subcommand, run), !,
    component(haskell_template, subcommand, ghci), !,
    component(haskell_template, subcommand, hlint), !.

test(haskell_project_docstrings) :-
    load_entity(semantic(file('src/prolog/nix/templates/haskell/semantics.pl'))),

    % Verify docstrings exist for main entity and subcommands
    docstring(haskell_template, _),
    docstring(haskell_template(build), _),
    docstring(haskell_template(test), _),
    docstring(haskell_template(run), _),
    docstring(haskell_template(ghci), _),
    docstring(haskell_template(hlint), _).

test(haskell_project_patterns) :-
    load_entity(semantic(file('src/prolog/nix/templates/haskell/semantics.pl'))),

    % Verify filesystem patterns for Haskell detection
    component(haskell_template, file_pattern, glob("*.hs")), !,
    component(haskell_template, file_pattern, glob("*.cabal")), !,
    component(haskell_template, discovery_pattern, include(_)), !,
    component(haskell_template, discovery_pattern, exclude(_)), !.

test(haskell_nix_command_delegation) :-
    load_entity(semantic(file('src/prolog/nix/templates/haskell/semantics.pl'))),

    % Verify that Haskell commands delegate to Nix
    % This tests the Nix-centric approach
    component(haskell_template, build_system, nix), !,
    component(haskell_template, subcommand, build), !.

:- end_tests(haskell_project_semantics).
