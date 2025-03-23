% Core Nix entity
entity(nix).

% Nix concepts as namespaces
entity(nix(store)).
entity(nix(derivation)).
entity(nix(package)).

% Type constructors for packages
component(nix(package), ctor, versioned).
component(nix(package), ctor, flake).

% Command constructors
component(command, ctor, nix(build)).
component(command, ctor, nix(develop)).
component(command, ctor, nix(shell)).

% Store relationships
component(nix(store), contains, nix(derivation)).
component(nix(derivation), outputs, nix(package)).

% Package types
component(nix(package), ctor, versioned_package).
component(nix(package), ctor, flake_package).

% Docstrings
docstring(nix(package),
    {|string(_)||
    Represents a Nix package.
    Format: nix_package(Name, Version)
    Example: entity(nix(package("hello", "2.12.1")))
    |}
).

docstring(nix(derivation),
    {|string(_)||
    Represents a Nix derivation.
    Format: nix_derivation(Path)
    Example: entity(nix(derivation("/nix/store/...")))
    |}
).

% Command docstrings
docstring(nix(build),
    {|string(_)||
    Builds a Nix expression.
    Format: command(nix(build(Path))).
    Creates:
    - Derivation entity
    - Result component linking to built outputs
    |}
).

% Command implementations
run(command(nix(build(Path))), RetVal) :-
    run(command(shell(["nix", "build", Path])), RetVal),
    % Track built derivation
    (RetVal = ok(Output) ->
        assert(entity(nix(derivation(Output)))),
        directory_file_path(Dir, _, Path),
        assert(component(folder(Dir), build_output, nix(derivation(Output))))
    ; true).

component(nix(develop), option(unique), shell_cmd).
component(nix(develop), option(unique), phase).

entity(nix(develop(phase))).
docstring(nix(develop(phase)),
    {|string(_)||
    Nix flake build phase: unpack / configure / build / check / install / installcheck
    |}
).

component(nix(develop(phase)), ctor, unpack      ).
component(nix(develop(phase)), ctor, configure   ).
component(nix(develop(phase)), ctor, build       ).
component(nix(develop(phase)), ctor, check       ).
component(nix(develop(phase)), ctor, install     ).
component(nix(develop(phase)), ctor, installcheck).

docstring(nix(develop), S) :-
    docstring(nix(develop(phase)), PhaseDoc),
    S = {|string(PhaseDoc)||
    Enters a development environment.
    Format: command(nix(develop(Options))).
    Creates development shell with specified dependencies.

    Options:
        shell_cmd(Args): execute a shell command instead of interactive session.
        phase(Phase): {PhaseDoc}
    |}.

run(command(nix(develop(Options))), RetVal) :-
    % Convert options to args
    findall(
        OptionArgs,
        (
            member(Option, Options),
            (Option = shell_cmd(Args) -> OptionArgs = ["--command" | Args]
            ;Option = phase(Phase) -> OptionArgs = ["--phase", Phase]
            )
        ),
        AllOptionArgs
    ),
    flatten([["nix", "develop"] | AllOptionArgs], Args),
    run(command(shell(Args, interactive)), RetVal).

docstring(nix(shell),
    {|string(_)||
    Runs a command in a Nix shell.
    Format: command(nix(shell(Packages, Command))).
    Executes Command with specified package dependencies.
    |}
).

run(command(nix(shell(Packages, Command))), RetVal) :-
    flatten([
        "nix-shell",
        "--run", Command,
        "--packages", Packages
    ], Args),
    run(command(shell(Args)), RetVal).
