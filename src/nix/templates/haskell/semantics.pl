% Haskell project semantics - Nix-centric approach
% Uses Nix flake apps rather than direct stack/cabal commands

:- self_entity(haskell_template).

% Main docstring for the project
docstring(haskell_template, "A Haskell project template using Nix flake apps for canonical build operations. All commands go through 'nix run' for reproducibility.").

% Project type identification
component(haskell_template, project_type, haskell).
component(haskell_template, build_system, nix).  % Changed from stack/cabal to nix
component(haskell_template, language, haskell).

% Make this entity available as a command
component(conjure, ctor, haskell_template).

% Nix-provided subcommands (from flake apps)
component(haskell_template, subcommand, build).
component(haskell_template, subcommand, test).
component(haskell_template, subcommand, run).
component(haskell_template, subcommand, ghci).
component(haskell_template, subcommand, hlint).

% Docstrings for Nix-based subcommands
docstring(haskell_template(build), "Build the Haskell project using 'nix build'").
docstring(haskell_template(test), "Run tests for the Haskell project using 'nix flake check'").
docstring(haskell_template(run), "Run the Haskell project using 'nix run .#run'").
docstring(haskell_template(ghci), "Start GHCi REPL using 'nix run .#ghci'").
docstring(haskell_template(hlint), "Run HLint linter using 'nix run .#hlint'").

% Make subcommands available as ctors too
component(haskell_template, ctor, C) :- component(haskell_template, subcommand, C).

% Command implementations using proper Nix CLI semantics
run(command(haskell_template(build)), RetVal) :-
    run(command(nix(build(['.']))), RetVal).

run(command(haskell_template(test)), RetVal) :-
    run(command(nix(flake(['check']))), RetVal).

run(command(haskell_template(run)), RetVal) :-
    run(command(nix(run(['.#run']))), RetVal).

run(command(haskell_template(ghci)), RetVal) :-
    run(command(nix(run(['.#ghci']))), RetVal).

run(command(haskell_template(hlint)), RetVal) :-
    run(command(nix(run(['.#hlint']))), RetVal).

% Filesystem pattern matching for Haskell projects
component(haskell_template, file_pattern, glob("*.hs")).
component(haskell_template, file_pattern, glob("src/**/*.hs")).
component(haskell_template, file_pattern, glob("test/**/*.hs")).
component(haskell_template, file_pattern, glob("*.cabal")).
component(haskell_template, file_pattern, glob("stack.yaml")).

% Discovery integration - can be used to identify Haskell projects
component(haskell_template, discovery_pattern,
    include([glob("*.hs"), glob("*.cabal"), glob("stack.yaml")])).
component(haskell_template, discovery_pattern,
    exclude([glob("dist/**"), glob(".stack-work/**")])).

% Source file location
component(haskell_template, source, source(semantic(file("semantics.pl")))).
