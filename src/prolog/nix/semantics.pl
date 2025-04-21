:- use_module(library(http/json)).

% Core Nix entity
entity(nix).

% Concepts in nix
component(nix, concept, nix(store)).
component(nix, concept, nix(derivation)).
component(nix, concept, nix(package)).
component(nix, concept, nix(build)).
component(nix, concept, nix(flake)).
component(nix, concept, nix(develop)).
component(nix, concept, nix(shell)).

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
component(nix(package), ctor, versioned).
component(nix(package), ctor, flake).

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

% Flake commands
entity(nix(flake)).
component(command, ctor, nix(flake)).
component(nix(flake), ctor, new).
docstring(nix(flake), S) :-
    S = {|string(_)||
    Represents a Nix flake.
    Flakes are a concept in Nix for reproducible, composable, and shareable development environments and packages.
    Refer to `nix(flake(template))` for more information on existing templates.
    |}.

component(nix(flake), templates_source, source(folder(Path))) :-
    component(nix, semantic_root, folder(SemanticDir)),
    directory_file_path(SemanticDir, "templates", Path).

nix_templates_path(Path) :-
    component(nix(flake), templates_source, source(folder(Path))).

entity(nix(flake(template))).
docstring(nix(flake(template)),
   {|string(_)||
   Nix flake templates that can be used to initialize projects.
   Format: nix(flake(template(TemplateId)))
   You can find the existing templates through the `instance` component
   of `nix(flake(template))`: `component(nix(flake), instance, Templates)`.
   |}).

nix_templates_expr_base(Expr) :-
    nix_templates_path(TemplatePath),
    format(string(Expr), "path:~w", [TemplatePath]).

nix_templates_expr_id(TemplateId, Expr) :-
    ( TemplateId = default -> TId = "defaultTemplate" ; TId = TemplateId ),
    nix_templates_path(TPath),
    format(string(Expr), "path:~w#~w", [TPath, TId]).


component(nix(flake(template)), instance, Templates) :-
    % We read it from `nix flake show --json path:path/to/templates`
    nix_templates_expr_base(PathExpr),
    NixCmdArgs = ["flake", "show", "--json", PathExpr],
    setup_call_cleanup(
        % Run `nix flake show --json path:.../templates`
        process_create(path("nix"), NixCmdArgs, [stdout(pipe(Out))]),
        % Read output json
        (
            json_read_dict(Out, JsonDict, [tag(template)]),
            dict_pairs(JsonDict.templates, Tag, Pairs),
            findall(
                template(K, D),
                (member(K-V, Pairs), D = V.description),
                Templates
            )
        ),
        % Cleanup
        (close(Out))
    ).

docstring(nix(flake(new)),
    {|string(_)||
    Initialize a new flake from template.
    Creates a new flake.nix from specified template.
    Format: command(nix(flake(new(TemplateId, DestPath)))).
        TemplateId - the ID of the template as it appears in `templates.*` attribute within the flake. default: none
        DestPath - where to create the new flake
    |}
).

run(command(nix(flake(new(TemplateId, DestPath)))), RetVal) :-
    ( TemplateId = none -> TId = default ; TId = TemplateId ),
    nix_templates_expr_id(TId, Template),
    Args = ["nix", "flake", "new", DestPath, "-t", Template],
    run(command(shell(Args)), RetVal).