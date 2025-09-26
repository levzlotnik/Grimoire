:- use_module(library(http/json)).

% Declare run/2 as discontiguous since it's spread throughout the file
:- discontiguous run/2.

% Core Nix entity with automatic self-location
:- self_entity(nix).

% Main Nix command categories from nix --help
component(nix, concept, nix(store)).
component(nix, concept, nix(derivation)).
component(nix, concept, nix(package)).
component(nix, concept, nix(target)).
component(nix, concept, nix(build)).
component(nix, concept, nix(flake)).
component(nix, concept, nix(develop)).
component(nix, concept, nix(search)).
component(nix, concept, nix(run)).

% Nix concepts as namespaces
entity(nix(store)).
entity(nix(derivation)).
entity(nix(package)).
entity(nix(target)).

% Nix command entities
entity(nix(build)).
entity(nix(develop)).
entity(nix(run)).
entity(nix(flake(new))).
entity(nix(flake(show))).
entity(nix(store(gc))).
entity(nix(store(repair))).
entity(nix(store(optimise))).
entity(nix(store(query))).
entity(nix(search)).
entity(nix(log)).
entity(nix(why_depends)).

% Spell constructors - separate mutable vs query operations
% Conjure constructors (state-changing operations)
component(conjure, ctor, nix(build)).
component(conjure, ctor, nix(develop)).
component(conjure, ctor, nix(run)).

% Perceive constructors (query operations)
component(perceive, ctor, nix(flake(show))).
component(perceive, ctor, nix(search)).
component(perceive, ctor, nix(store(query))).
component(perceive, ctor, nix(log)).
component(perceive, ctor, nix(why_depends)).

% Legacy command constructors (for backwards compatibility)
% Conjure constructors (state-changing operations)
component(conjure, ctor, nix(build)).
component(conjure, ctor, nix(develop)).
component(conjure, ctor, nix(run)).
component(conjure, ctor, nix(flake(new))).
component(conjure, ctor, nix(store(gc))).
component(conjure, ctor, nix(store(repair))).
component(conjure, ctor, nix(store(optimise))).

% Perceive constructors (read-only operations)
component(perceive, ctor, nix(search)).
component(perceive, ctor, nix(log)).
component(perceive, ctor, nix(why_depends)).
component(perceive, ctor, nix(flake(show))).

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

docstring(nix(target(package)),
    {|string(_)||
    Nix package build target specification.
    Represents a buildable package output from a flake.
    Can be parameterized: nix(target(package(FlakeRef, AttrPath)))
    Used in nix build operations to specify package outputs.
    |}).

docstring(nix(target(app)),
    {|string(_)||
    Nix application target specification.
    Represents an executable application from a flake.
    Can be parameterized: nix(target(app(FlakeRef, AttrPath)))
    Used with nix run to execute applications directly.
    |}).

docstring(nix(target(devShell)),
    {|string(_)||
    Nix development shell target specification.
    Provides development environment with tools and dependencies.
    Can be parameterized: nix(target(devShell(FlakeRef, AttrPath)))
    Used with nix develop to enter development environments.
    |}).

docstring(nix(target(check)),
    {|string(_)||
    Nix check/test target specification.
    Represents test suites and checks defined in flakes.
    Can be parameterized: nix(target(check(FlakeRef, AttrPath)))
    Used to run tests and validation checks.
    |}).

docstring(nix(target(formatter)),
    {|string(_)||
    Nix code formatter target specification.
    Provides code formatting tools from flake outputs.
    Can be parameterized: nix(target(formatter(FlakeRef, AttrPath)))
    Used to format code according to project standards.
    |}).

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

perceive(nix(search(Query, Results))) :-
    perceive(nix(search(nixpkgs, Query, Results))).

perceive(nix(search(Flake, Query, Results))) :-
    format(atom(FlakeQuery), '~w#~w', [Flake, Query]),
    Args = ["nix", "search", FlakeQuery],
    cast(conjure(shell(Args)), ok(result(Results, _))).

% Run command
docstring(nix(run),
    {|string(_)||
    Run a Nix application.
    Format: command(nix(run(Installable))) or command(nix(run(Installable, Args)))
      Installable: Package/flake to run
      Args: Arguments to pass to the application
    |}
).

cast(conjure(nix(run(Installable))), RetVal) :-
    Args = ["nix", "run", Installable],
    cast(conjure(shell(Args, interactive)), RetVal).

cast(conjure(nix(run(Installable, AppArgs))), RetVal) :-
    flatten([["nix", "run", Installable, "--"], AppArgs], Args),
    cast(conjure(shell(Args, interactive)), RetVal).

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
cast(conjure(nix(build(Installable))), RetVal) :-
    Args = ["nix", "build", Installable],
    cast(conjure(shell(Args)), RetVal),
    % Track built derivation
    (RetVal = ok(Output) ->
        assert(entity(nix(derivation(Output)))),
        assert(component(build_result, output_path, Output))
    ; true).

cast(conjure(nix(build(Installable, Options))), RetVal) :-
    flatten([["nix", "build", Installable], Options], Args),
    cast(conjure(shell(Args)), RetVal).

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

docstring(nix(store(gc)), "Garbage collect unused store paths").
docstring(nix(store(repair)), "Repair corrupted store paths").  
docstring(nix(store(optimise)), "Optimize store by hardlinking identical files").

cast(conjure(nix(store(gc))), RetVal) :-
    Args = ["nix", "store", "gc"],
    cast(conjure(shell(Args)), RetVal).

cast(conjure(nix(store(repair(Path)))), RetVal) :-
    Args = ["nix", "store", "repair", Path],
    cast(conjure(shell(Args)), RetVal).

cast(conjure(nix(store(optimise))), RetVal) :-
    Args = ["nix", "store", "optimise"],
    cast(conjure(shell(Args)), RetVal).

% Utility commands for debugging and analysis
docstring(nix(why_depends),
    {|string(_)||
    Show why a package has another package in its closure.
    Format: command(nix(why_depends(Package1, Package2)))
    Essential for understanding dependency chains.
    |}
).

perceive(nix(why_depends(Pkg1, Pkg2, Result))) :-
    Args = ["nix", "why-depends", Pkg1, Pkg2],
    cast(conjure(shell(Args)), ok(result(Result, _))).

docstring(nix(log),
    {|string(_)||
    Show the build log of specified packages or paths.
    Format: command(nix(log(Installable)))
    Essential for debugging build failures.
    |}
).

perceive(nix(log(Installable, Result))) :-
    Args = ["nix", "log", Installable],
    cast(conjure(shell(Args)), ok(result(Result, _))).

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

cast(conjure(nix(develop(Options))), RetVal) :-
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
    cast(conjure(shell(Args, interactive)), RetVal).

% Flake commands
entity(nix(flake)).
% Removed legacy command ctor - using perceive above
component(nix(flake), ctor, new).
docstring(nix(flake), S) :-
    S = {|string(_)||
    Represents a Nix flake.
    Flakes are a concept in Nix for reproducible, composable, and shareable development environments and packages.
    Refer to `nix(flake(template))` for more information on existing templates.
    |}.

component(nix(flake), templates_source, source(folder(Path))) :-
    component(nix, self, semantic(folder(SemanticDir))),
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
    member(Id-_V, Pairs).

%% entity/1 and docstring/2 for nix(flake(template(Id)))
entity(nix(flake(template(Id)))) :-
    nix_flake_templates(Pairs),
    member(Id-_, Pairs).

docstring(nix(flake(template(Id))), Desc) :-
    nix_flake_templates(Pairs),
    member(Id-V, Pairs),
    Desc = V.description.

docstring(nix(flake(new)),
    {|string(_)||
    Initialize a new flake from template.
    Creates a new flake.nix from specified template.
    Format: conjure(nix(flake(new(TemplateId, DestPath)))).
        TemplateId - the ID of the template as it appears in `templates.*` attribute within the flake. default: none
        DestPath - where to create the new flake
    |}
).

docstring(nix(flake(show)),
    {|string(_)||
    Show flake metadata, apps, and packages.
    Displays available apps, packages, and development shells from a flake.
    Format: perceive(nix(flake(show(FlakeRef, Apps, Packages, DevShells)))).
        FlakeRef - flake reference (path or URL)
        Apps - unifies with list of available applications
        Packages - unifies with list of available packages  
        DevShells - unifies with list of available development shells
    |}
).

docstring(nix(store(query)),
    {|string(_)||
    Query the Nix store for packages and dependencies.
    Searches store paths and package information.
    Format: perceive(nix(store(query(Pattern, Results)))).
        Pattern - search pattern for store paths
        Results - unifies with list of matching store paths
    |}
).


cast(conjure(nix(flake(new(TemplateId, DestPath)))), RetVal) :- 
    ( TemplateId = none -> TId = default ; TId = TemplateId ),
    nix_templates_expr_id(TId, Template),
    Args = ["nix", "flake", "new", DestPath, "-t", Template],
    cast(conjure(shell(Args)), RetVal).

% Nix subsystem extension for load_entity
% Lazy loading for nix semantic folders
load_entity(Entity, semantic(nix_folder(Path), lazy)) :-
    assertz(to_be_loaded(Entity, semantic(folder(Path)))),
    asserta(entity(Entity)).

% === PERCEIVE PREDICATES - Structured Nix Queries ===

% Nix flake show perception - parse flake structure into organized data
perceive(nix(flake(show(FlakeRef, Apps, Packages, DevShells)))) :-
    get_nix_flake_targets(FlakeRef, Targets),
    partition_flake_targets(Targets, Apps, Packages, DevShells).

% Partition flake targets by type
partition_flake_targets([], [], [], []).
partition_flake_targets([Target|Rest], Apps, Packages, DevShells) :-
    partition_flake_targets(Rest, RestApps, RestPackages, RestDevShells),
    (Target = nix(target(app(FlakeRef, AttrPath))) ->
        Apps = [app(FlakeRef, AttrPath)|RestApps],
        Packages = RestPackages,
        DevShells = RestDevShells
    ; Target = nix(target(package(FlakeRef, AttrPath))) ->
        Apps = RestApps,
        Packages = [package(FlakeRef, AttrPath)|RestPackages],
        DevShells = RestDevShells
    ; Target = nix(target(devShell(FlakeRef, AttrPath))) ->
        Apps = RestApps,
        Packages = RestPackages,
        DevShells = [devShell(FlakeRef, AttrPath)|RestDevShells]
    ;
        Apps = RestApps,
        Packages = RestPackages,
        DevShells = RestDevShells
    ).