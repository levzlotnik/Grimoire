:- use_module(library(http/json)).

% Declare run/2 as discontiguous since it's spread throughout the file
:- discontiguous run/2.

% Core Nix entity
entity(nix).

% Main Nix command categories from nix --help
component(nix, concept, nix(store)).
component(nix, concept, nix(derivation)).
component(nix, concept, nix(package)).
component(nix, concept, nix(target)).
component(nix, concept, nix(build)).
component(nix, concept, nix(flake)).
com:- use_module(library(http/json)).ent(nix, concept, nix(develop)).
component(nix, concept, nix(search)).
component(nix, concept, nix(run)).

% Nix concepts as namespaces
entity(nix(store)).
entity(nix(derivation)).
entity(nix(package)).
entity(nix(target)).

% Main command constructors (the essential ones)
component(command, ctor, nix(build)).
component(command, ctor, nix(develop)).
component(command, ctor, nix(flake)).
component(command, ctor, nix(run)).
component(command, ctor, nix(search)).

% Essential utility commands
component(command, ctor, nix(store)).
component(command, ctor, nix(log)).
component(command, ctor, nix(why_depends)).

% Store relationships
component(nix(store), contains, nix(derivation)).
component(nix(derivation), outputs, nix(package)).
component(nix(flake), exposes, nix(target)).
component(nix(target), builds_to, nix(derivation)).

% Package types
component(nix(package), ctor, versioned).
component(nix(package), ctor, flake).

% Target types (flake outputs)
component(nix(target), ctor, package).
component(nix(target), ctor, app).
component(nix(target), ctor, devShell).
component(nix(target), ctor, check).
component(nix(target), ctor, formatter).

% Target constructor entities
entity(nix(target(package))).
entity(nix(target(app))).
entity(nix(target(devShell))).
entity(nix(target(check))).
entity(nix(target(formatter))).

% Dynamic target discovery - flakes define themselves, we discover their targets
% Usage: entity(nix(flake(FlakeName, FlakeRef))) in individual semantics.pl files

% Memoized flake introspection
:- table get_nix_flake_targets/2.
get_nix_flake_targets(FlakeRef, Targets) :-
    process_create(path(nix),
                   ["flake", "show", "--json", FlakeRef],
                   [stdout(pipe(Out))]),
    json_read_dict(Out, JsonDict),
    close(Out),
    findall(Target, extract_flake_target(JsonDict, FlakeRef, Target), Targets).

extract_flake_target(JsonDict, FlakeRef, Target) :-
    % Extract packages
    (get_dict(packages, JsonDict, Packages) ->
        dict_pairs(Packages, _, SystemPairs),
        member(System-SystemPackages, SystemPairs),
        dict_pairs(SystemPackages, _, PackagePairs),
        member(PackageName-_, PackagePairs),
        format(atom(AttrPath), 'packages.~w.~w', [System, PackageName]),
        Target = nix(target(package(FlakeRef, AttrPath)))
    ; fail),
    !.

extract_flake_target(JsonDict, FlakeRef, Target) :-
    % Extract apps
    (get_dict(apps, JsonDict, Apps) ->
        dict_pairs(Apps, _, SystemPairs),
        member(System-SystemApps, SystemPairs),
        dict_pairs(SystemApps, _, AppPairs),
        member(AppName-_, AppPairs),
        format(atom(AttrPath), 'apps.~w.~w', [System, AppName]),
        Target = nix(target(app(FlakeRef, AttrPath)))
    ; fail),
    !.

extract_flake_target(JsonDict, FlakeRef, Target) :-
    % Extract devShells
    (get_dict(devShells, JsonDict, DevShells) ->
        dict_pairs(DevShells, _, SystemPairs),
        member(System-SystemShells, SystemPairs),
        dict_pairs(SystemShells, _, ShellPairs),
        member(ShellName-_, ShellPairs),
        format(atom(AttrPath), 'devShells.~w.~w', [System, ShellName]),
        Target = nix(target(devShell(FlakeRef, AttrPath)))
    ; fail),
    !.

extract_flake_target(JsonDict, FlakeRef, Target) :-
    % Extract checks
    (get_dict(checks, JsonDict, Checks) ->
        dict_pairs(Checks, _, SystemPairs),
        member(System-SystemChecks, SystemPairs),
        dict_pairs(SystemChecks, _, CheckPairs),
        member(CheckName-_, CheckPairs),
        format(atom(AttrPath), 'checks.~w.~w', [System, CheckName]),
        Target = nix(target(check(FlakeRef, AttrPath)))
    ; fail),
    !.

extract_flake_target(JsonDict, FlakeRef, Target) :-
    % Extract formatters
    (get_dict(formatter, JsonDict, Formatters) ->
        dict_pairs(Formatters, _, SystemPairs),
        member(System-_, SystemPairs),
        format(atom(AttrPath), 'formatter.~w', [System]),
        Target = nix(target(formatter(FlakeRef, AttrPath)))
    ; fail).

% Dynamic entity generation for flake targets
entity(nix(target(package(FlakeRef, AttrPath)))) :-
    entity(nix(flake(_, FlakeRef))),
    get_nix_flake_targets(FlakeRef, Targets),
    member(nix(target(package(FlakeRef, AttrPath))), Targets).

entity(nix(target(app(FlakeRef, AttrPath)))) :-
    entity(nix(flake(_, FlakeRef))),
    get_nix_flake_targets(FlakeRef, Targets),
    member(nix(target(app(FlakeRef, AttrPath))), Targets).

entity(nix(target(devShell(FlakeRef, AttrPath)))) :-
    entity(nix(flake(_, FlakeRef))),
    get_nix_flake_targets(FlakeRef, Targets),
    member(nix(target(devShell(FlakeRef, AttrPath))), Targets).

entity(nix(target(check(FlakeRef, AttrPath)))) :-
    entity(nix(flake(_, FlakeRef))),
    get_nix_flake_targets(FlakeRef, Targets),
    member(nix(target(check(FlakeRef, AttrPath))), Targets).

entity(nix(target(formatter(FlakeRef, AttrPath)))) :-
    entity(nix(flake(_, FlakeRef))),
    get_nix_flake_targets(FlakeRef, Targets),
    member(nix(target(formatter(FlakeRef, AttrPath))), Targets).

% Dynamic component generation - flakes expose their targets
component(nix(flake(FlakeName, FlakeRef)), target(TargetType), Target) :-
    entity(nix(flake(FlakeName, FlakeRef))),
    get_nix_flake_targets(FlakeRef, Targets),
    member(Target, Targets),
    Target = nix(target(TargetTypeCall)),
    functor(TargetTypeCall, TargetType, _).

% === DOCSTRINGS ===

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

docstring(nix(target),
    {|string(_)||
    Represents a flake output target (sum type).
    Targets are discovered dynamically from flakes via 'nix flake show --json'.

    Formats:
      nix(target(package(FlakeRef, AttrPath)))
      nix(target(app(FlakeRef, AttrPath)))
      nix(target(devShell(FlakeRef, AttrPath)))
      nix(target(check(FlakeRef, AttrPath)))
      nix(target(formatter(FlakeRef, AttrPath)))

    Usage:
      1. Define flake: entity(nix(flake(MyFlake, "path/to/flake")))
      2. Targets auto-discovered: entity(nix(target(app("path/to/flake", "apps.x86_64-linux.hello"))))
      3. Components auto-generated: component(nix(flake(MyFlake, "path/to/flake")), target(app), Target)
    |}
).

% === COMMAND IMPLEMENTATIONS ===

% Search command
docstring(nix(search),
    {|string(_)||
    Search for packages in nixpkgs.
    Format: command(nix(search(Query))) or command(nix(search(Flake, Query)))
      Query: Search term(s)
      Flake: Flake to search in (defaults to nixpkgs)
    |}
).

run(command(nix(search(Query))), RetVal) :-
    run(command(nix(search(nixpkgs, Query))), RetVal).

run(command(nix(search(Flake, Query))), RetVal) :-
    format(atom(FlakeQuery), '~w#~w', [Flake, Query]),
    Args = ["nix", "search", FlakeQuery],
    run(command(shell(Args)), RetVal).

% Run command
docstring(nix(run),
    {|string(_)||
    Run a Nix application.
    Format: command(nix(run(Installable))) or command(nix(run(Installable, Args)))
      Installable: Package/flake to run
      Args: Arguments to pass to the application
    |}
).

run(command(nix(run(Installable))), RetVal) :-
    Args = ["nix", "run", Installable],
    run(command(shell(Args, interactive)), RetVal).

run(command(nix(run(Installable, AppArgs))), RetVal) :-
    flatten([["nix", "run", Installable, "--"], AppArgs], Args),
    run(command(shell(Args, interactive)), RetVal).

% Command docstrings
docstring(nix(build),
    {|string(_)||
    Builds a Nix expression or flake.
    Format: command(nix(build(Installable))) or command(nix(build(Installable, Options)))
      Installable: What to build (flake reference, derivation, etc.)
      Options: List of build options
    Creates:
    - Derivation entity
    - Result component linking to built outputs
    |}
).

% Command implementations
run(command(nix(build(Installable))), RetVal) :-
    Args = ["nix", "build", Installable],
    run(command(shell(Args)), RetVal),
    % Track built derivation
    (RetVal = ok(Output) ->
        assert(entity(nix(derivation(Output)))),
        assert(component(build_result, output_path, Output))
    ; true).

run(command(nix(build(Installable, Options))), RetVal) :-
    flatten([["nix", "build", Installable], Options], Args),
    run(command(shell(Args)), RetVal).

% Store commands
entity(nix(store)).
component(nix(store), ctor, gc).
component(nix(store), ctor, repair).
component(nix(store), ctor, optimise).

docstring(nix(store),
    {|string(_)||
    Manipulate the Nix store - essential for system maintenance.
    Format: command(nix(store(Subcommand)))
    Subcommands: gc (garbage collect), repair, optimise
    |}
).

run(command(nix(store(gc))), RetVal) :-
    Args = ["nix", "store", "gc"],
    run(command(shell(Args)), RetVal).

run(command(nix(store(repair(Path)))), RetVal) :-
    Args = ["nix", "store", "repair", Path],
    run(command(shell(Args)), RetVal).

run(command(nix(store(optimise))), RetVal) :-
    Args = ["nix", "store", "optimise"],
    run(command(shell(Args)), RetVal).

% Utility commands for debugging and analysis
docstring(nix(why_depends),
    {|string(_)||
    Show why a package has another package in its closure.
    Format: command(nix(why_depends(Package1, Package2)))
    Essential for understanding dependency chains.
    |}
).

run(command(nix(why_depends(Pkg1, Pkg2))), RetVal) :-
    Args = ["nix", "why-depends", Pkg1, Pkg2],
    run(command(shell(Args)), RetVal).

docstring(nix(log),
    {|string(_)||
    Show the build log of specified packages or paths.
    Format: command(nix(log(Installable)))
    Essential for debugging build failures.
    |}
).

run(command(nix(log(Installable))), RetVal) :-
    Args = ["nix", "log", Installable],
    run(command(shell(Args)), RetVal).

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

%% memoize the one–time JSON fetch
:- table nix_flake_templates/1.
nix_flake_templates(Pairs) :-
    nix_templates_expr_base(PathExpr),
    process_create(path(nix),
                   ["flake","show","--json",PathExpr],
                   [stdout(pipe(Out))]),
    json_read_dict(Out, JsonDict),
    close(Out),
    dict_pairs(JsonDict.templates, _Tag, Pairs).

%% component/3 now yields one template/2 per backtrack
component(nix(flake(template)), instance, nix(flake(template(Id))))  :-
    nix_flake_templates(Pairs),
    member(Id-V, Pairs),
    Desc = V.description.

%% entity/1 and docstring/2 for nix(flake(template(Id)))
entity(nix(flake(template(Id)))) :-
    nix_flake_templates(Pairs),
    member(Id-_, Pairs).

docstring(nix(flake(template(Id))), Desc) :-
    nix_flake_templates(Pairs),
    member(Id-V, Pairs),
    Desc = V.description.

docstring(command(nix(flake(new))),
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

% Nix subsystem extension for load_entity
% Lazy loading for nix semantic folders
load_entity(Entity, semantic(nix_folder(Path), lazy)) :-
    assertz(to_be_loaded(Entity, semantic(folder(Path)))),
    asserta(entity(Entity)).