% Rust project semantics - Nix-centric approach
% Uses Nix flake apps rather than direct cargo commands

entity(rust_template).

% Main docstring for the project
docstring(rust_template, "A Rust project template using Nix flake apps for canonical build operations. All commands go through 'nix run' for reproducibility.").

% Project type identification
component(rust_template, project_type, rust).
component(rust_template, build_system, nix).  % Changed from cargo to nix
component(rust_template, language, rust).

% Make this entity available as a command
component(command, ctor, rust_template).

% Nix-provided subcommands (from flake apps)
component(rust_template, subcommand, build).
component(rust_template, subcommand, test).
component(rust_template, subcommand, run).
component(rust_template, subcommand, check).
component(rust_template, subcommand, clippy).
component(rust_template, subcommand, fmt).

% Docstrings for Nix-based subcommands
docstring(rust_template(build), "Build the Rust project using 'nix run .#build'").
docstring(rust_template(test), "Run tests for the Rust project using 'nix run .#test'").
docstring(rust_template(run), "Run the Rust project using 'nix run .#run'").
docstring(rust_template(check), "Check the Rust project using 'nix run .#check'").
docstring(rust_template(clippy), "Run clippy linter using 'nix run .#clippy'").
docstring(rust_template(fmt), "Format the Rust code using 'nix run .#fmt'").

% Make subcommands available as ctors too
component(rust_template, ctor, C) :- component(rust_template, subcommand, C).

% Command implementations using Nix
run(command(rust_template(build)), RetVal) :-
    run(command(nix(run(['.#build']))), RetVal).

run(command(rust_template(test)), RetVal) :-
    run(command(nix(run(['.#test']))), RetVal).

run(command(rust_template(run)), RetVal) :-
    run(command(nix(run(['.#run']))), RetVal).

run(command(rust_template(check)), RetVal) :-
    run(command(nix(run(['.#check']))), RetVal).

run(command(rust_template(clippy)), RetVal) :-
    run(command(nix(run(['.#clippy']))), RetVal).

run(command(rust_template(fmt)), RetVal) :-
    run(command(nix(run(['.#fmt']))), RetVal).

% Discover project artifacts with Rust-specific patterns
% TODO: Fix discover_project_artifacts integration
% :- discover_project_artifacts(rust_template, [
%     fs_patterns(
%         include([
%             glob("src/**/*.rs"),
%             glob("Cargo.toml"),
%             glob("Cargo.lock"),
%             glob("tests/**/*.rs"),
%             glob("benches/**/*.rs"),
%             glob("examples/**/*.rs"),
%             glob("build.rs")
%         ]),
%         exclude([
%             glob("target/**/*"),
%             glob("**/target/**/*")
%         ])
%     )
% ]).

% Rust project structure expectations
component(rust_template, expected_file, file("Cargo.toml")).
component(rust_template, expected_subdir, folder("src")).
component(rust_template, main_source, file("src/main.rs")).
component(rust_template, lib_source, file("src/lib.rs")).

% Common Rust dependencies patterns
component(rust_template, dep_pattern, glob("Cargo.toml")).
component(rust_template, build_artifact_pattern, glob("target/**/*")).
