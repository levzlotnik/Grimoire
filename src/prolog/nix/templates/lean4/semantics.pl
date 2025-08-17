% Lean4 project semantics - Nix-centric approach
% Uses Nix flake apps rather than direct lake commands
% Note: Lean4 focuses on formal verification, not traditional testing

entity(lean4_template).

% Main docstring for the project
docstring(lean4_template, "A Lean4 formal verification template using Nix flake apps for canonical build operations. Focuses on proof verification rather than traditional testing workflows.").

% Project type identification
component(lean4_template, project_type, lean4).
component(lean4_template, build_system, nix).  % Changed from lake to nix
component(lean4_template, language, lean4).
component(lean4_template, paradigm, formal_verification).

% Make this entity available as a command
component(command, ctor, lean4_template).

% Nix-provided subcommands (from flake apps)
component(lean4_template, subcommand, build).
component(lean4_template, subcommand, check).
component(lean4_template, subcommand, clean).
component(lean4_template, subcommand, doc).

% Docstrings for Nix-based subcommands
docstring(lean4_template(build), "Build the Lean4 project using 'nix build'").
docstring(lean4_template(check), "Check proofs in the Lean4 project using 'nix flake check'").
docstring(lean4_template(clean), "Clean build artifacts using 'nix run .#clean'").
docstring(lean4_template(doc), "Generate documentation using 'nix run .#doc'").

% Make subcommands available as ctors too
component(lean4_template, ctor, C) :- component(lean4_template, subcommand, C).

% Command implementations using proper Nix CLI semantics
run(command(lean4_template(build)), RetVal) :-
    run(command(nix(build(['.']))), RetVal).

run(command(lean4_template(check)), RetVal) :-
    run(command(nix(flake(['check']))), RetVal).

run(command(lean4_template(clean)), RetVal) :-
    run(command(nix(run(['.#clean']))), RetVal).

run(command(lean4_template(doc)), RetVal) :-
    run(command(nix(run(['.#doc']))), RetVal).

% Filesystem pattern matching for Lean4 projects
component(lean4_template, file_pattern, glob("*.lean")).
component(lean4_template, file_pattern, glob("**/*.lean")).
component(lean4_template, file_pattern, glob("lakefile.toml")).
component(lean4_template, file_pattern, glob("lean-toolchain")).

% Discovery integration - can be used to identify Lean4 projects
component(lean4_template, discovery_pattern,
    include([glob("*.lean"), glob("lakefile.toml"), glob("lean-toolchain")])).
component(lean4_template, discovery_pattern,
    exclude([glob("build/**"), glob(".lake/**")])).

% Source file location
component(lean4_template, source, source(semantic(file("semantics.pl")))).
