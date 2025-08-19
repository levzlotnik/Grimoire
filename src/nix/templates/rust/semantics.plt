%! Rust template test suite
%
% Tests for Rust project template semantics, including project detection,
% command integration, and Nix build system support.

:- use_module(library(plunit)).
:- use_module(library(filesex)).

:- begin_tests(rust_project_semantics).

% Test that a loaded Rust project has essential components
test(rust_project_has_essentials) :-
    % Load the rust template directly
    load_entity(semantic(file('src/nix/templates/rust/semantics.pl'))),

    % Verify project type identification
    component(rust_template, project_type, rust),
    component(rust_template, build_system, nix),  % Changed from build_tool to build_system
    component(rust_template, language, rust),

    % Verify command integration
    component(command, ctor, rust_template).

test(rust_project_subcommands) :-
    load_entity(semantic(file('src/nix/templates/rust/semantics.pl'))),

    % Verify all expected subcommands
    component(rust_template, subcommand, build), !,
    component(rust_template, subcommand, test), !,
    component(rust_template, subcommand, run), !,
    component(rust_template, subcommand, check), !,
    component(rust_template, subcommand, clippy), !,
    component(rust_template, subcommand, fmt), !.

test(rust_project_docstrings) :-
    load_entity(semantic(file('src/nix/templates/rust/semantics.pl'))),

    % Verify main docstring exists and mentions Rust
    docstring(rust_template, MainDoc),
    sub_string(MainDoc, _, _, _, "Rust"), !,

    % Verify subcommand docstrings use proper Nix CLI semantics
    docstring(rust_template(build), BuildDoc),
    sub_string(BuildDoc, _, _, _, "nix build"), !,

    docstring(rust_template(test), TestDoc),
    sub_string(TestDoc, _, _, _, "nix flake check"), !.

:- end_tests(rust_project_semantics).
