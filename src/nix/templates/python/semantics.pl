% Python project semantics - Nix-centric approach
% Uses Nix flake apps rather than direct pip/python commands

:- self_entity(python_template).

% Main docstring for the project
docstring(python_template, "A Python project template using Nix flake apps for canonical build operations. All commands go through 'nix run' for reproducibility.").

% Project type identification
component(python_template, project_type, python).
component(python_template, build_system, nix).
component(python_template, language, python).

% Make this entity available as a command
component(conjure, ctor, python_template).

% Nix-provided subcommands (from flake apps)
component(python_template, subcommand, run).
component(python_template, subcommand, build).
component(python_template, subcommand, develop).

% Docstrings for Nix-based subcommands
docstring(python_template(run), "Run the Python project using 'nix run .#run'").
docstring(python_template(build), "Build the Python package using 'nix build'").
docstring(python_template(develop), "Enter development shell using 'nix develop'").

% Make subcommands available as ctors too
component(python_template, ctor, C) :- component(python_template, subcommand, C).

% Command implementations using Nix
cast(conjure(python_template(run)), RetVal) :-
    cast(conjure(nix(run(['.#run']))), RetVal).

cast(conjure(python_template(build)), RetVal) :-
    cast(conjure(nix(build(['.']))), RetVal).

cast(conjure(python_template(develop)), RetVal) :-
    cast(conjure(nix(develop(['.']))), RetVal).

% Python project structure expectations
component(python_template, expected_file, file("pyproject.toml")).
component(python_template, expected_file, file("main.py")).
component(python_template, expected_subdir, folder("src")).
component(python_template, main_source, file("main.py")).

% Common Python build artifacts patterns
component(python_template, build_artifact_pattern, glob("dist/**/*")).
component(python_template, build_artifact_pattern, glob("__pycache__/**/*")).
component(python_template, build_artifact_pattern, glob("*.egg-info/**/*")).
