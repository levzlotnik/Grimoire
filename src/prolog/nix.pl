:- module(nix_commands, []).
:- use_module(core_rules).

% Nix-specific entity types
docstring(nix_package,
    {|string(_)||
    Represents a Nix package.
    Format: nix_package(Name, Version)
    Example: entity(nix_package("hello", "2.12.1"))
    |}
).

docstring(nix_derivation,
    {|string(_)||
    Represents a Nix derivation.
    Format: nix_derivation(Path)
    Example: entity(nix_derivation("/nix/store/..."))
    |}
).

% Nix commands
docstring(nix_build,
    {|string(_)||
    Builds a Nix expression.
    Format: command(nix_build(Path)).
    Creates:
    - Derivation entity
    - Result component linking to built outputs
    |}
).

docstring(nix_develop,
    {|string(_)||
    Enters a development environment.
    Format: command(nix_develop(Path)).
    Creates development shell with specified dependencies.
    |}
).

docstring(nix_shell,
    {|string(_)||
    Runs a command in a Nix shell.
    Format: command(nix_shell(Packages, Command)).
    Executes Command with specified package dependencies.
    |}
).

% Command implementations
run(command(nix_build(Path)), RetVal) :-
    run(command(shell({|string(Path)||nix build '{Path}'|})), RetVal),
    % Track built derivation
    (RetVal = ok(Output) ->
        assert(entity(nix_derivation(Output))),
        directory_file_path(Dir, _, Path),
        assert(component(folder(Dir), build_output, nix_derivation(Output)))
    ; true).

run(command(nix_develop(Path)), RetVal) :-
    run(command(shell({|string(Path)||nix develop '{Path}'|})), RetVal).

run(command(nix_shell(Packages, Command)), RetVal) :-
    atomic_list_concat(Packages, ' ', PackageStr),
    run(command(shell({|string(PackageStr, Command)||
        nix shell {PackageStr} -c {Command}
    |})), RetVal).
