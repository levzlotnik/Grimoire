% C++ project semantics - Nix-centric approach
% Uses Nix flake apps rather than direct cmake/make commands

:- self_entity(cpp_template).

% Main docstring for the project
docstring(cpp_template, "A C++ project template using Nix flake apps for canonical build operations. All commands go through 'nix run' for reproducibility.").

% Project type identification
component(cpp_template, project_type, cpp).
component(cpp_template, build_system, nix).
component(cpp_template, language, cpp).

% Make this entity available as a command
component(conjure, ctor, cpp_template).

% Nix-provided subcommands (from flake apps)
component(cpp_template, subcommand, run).
component(cpp_template, subcommand, build).
component(cpp_template, subcommand, develop).

% Docstrings for Nix-based subcommands
docstring(cpp_template(run), "Run the C++ project using 'nix run .#run'").
docstring(cpp_template(build), "Build the C++ project using 'nix build'").
docstring(cpp_template(develop), "Enter development shell using 'nix develop'").

% Make subcommands available as ctors too
component(cpp_template, ctor, C) :- component(cpp_template, subcommand, C).

% Command implementations using Nix
run(command(cpp_template(run)), RetVal) :-
    run(command(nix(run(['.#run']))), RetVal).

run(command(cpp_template(build)), RetVal) :-
    run(command(nix(build(['.']))), RetVal).

run(command(cpp_template(develop)), RetVal) :-
    run(command(nix(develop(['.']))), RetVal).

% C++ project structure expectations
component(cpp_template, expected_file, file("CMakeLists.txt")).
component(cpp_template, expected_subdir, folder("src")).
component(cpp_template, expected_subdir, folder("include")).
component(cpp_template, main_source, file("src/main.cpp")).

% Common C++ build artifacts patterns
component(cpp_template, build_artifact_pattern, glob("build/**/*")).
component(cpp_template, build_artifact_pattern, glob("*.o")).
component(cpp_template, build_artifact_pattern, glob("*.so")).
