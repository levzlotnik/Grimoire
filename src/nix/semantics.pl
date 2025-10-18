:- use_module(library(http/json)).

% Core Nix entity with automatic self-location
:- self_entity(nix, {|string(_)||
    Symbolic configuration, package management, and build subsystem.
    Provides reproducible environments and declarative system configuration through Nix flakes.
    Manages dependencies, build processes, and development shells with perfect reproducibility.
    Enables functional package management where builds are pure functions of their inputs.
    |}).

% === ENTITY DECLARATIONS ===

% Main Nix concept entities
entity(nix(store)).
entity(nix(derivation)).
entity(nix(package)).
entity(nix(target)).
entity(nix(flake)).
entity(nix(develop)).
entity(nix(develop(phase))).
entity(nix(flake(template))).

% Nix concepts
component(nix, concept, nix(store)).
component(nix, concept, nix(derivation)).
component(nix, concept, nix(package)).
component(nix, concept, nix(target)).
component(nix, concept, nix(flake)).
component(nix, concept, nix(develop)).
component(nix, concept, nix(search)).
component(nix, concept, nix(run)).

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

% Store command constructors
component(nix(store), ctor, gc).
component(nix(store), ctor, repair).
component(nix(store), ctor, optimise).

% Develop command options
component(nix(develop), option(unique), shell_cmd).
component(nix(develop), option(unique), phase).

% Develop phase constructors
component(nix(develop(phase)), ctor, unpack).
component(nix(develop(phase)), ctor, configure).
component(nix(develop(phase)), ctor, build).
component(nix(develop(phase)), ctor, check).
component(nix(develop(phase)), ctor, install).
component(nix(develop(phase)), ctor, installcheck).

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

docstring(nix(store),
    "Manipulate the Nix store - essential for system maintenance. Subcommands: gc, repair, optimise"
).

docstring(nix(store(gc)),
    "Garbage collect unused store paths"
).

docstring(nix(store(repair)),
    "Repair corrupted store paths"
).

docstring(nix(store(optimise)),
    "Optimize store by hardlinking identical files"
).

docstring(nix(develop(phase)),
    {|string(_)||
    Nix flake build phase: unpack / configure / build / check / install / installcheck
    |}
).

docstring(nix(flake), S) :-
    S = {|string(_)||
    Represents a Nix flake.
    Flakes are a concept in Nix for reproducible, composable, and shareable development environments and packages.
    Refer to `nix(flake(template))` for more information on existing templates.
    |}.

docstring(nix(flake(template)),
   {|string(_)||
   Nix flake templates that can be used to initialize projects.
   Format: nix(flake(template(TemplateId)))
   You can find the existing templates through the `instance` component
   of `nix(flake(template))`: `component(nix(flake), instance, Templates)`.
   |}).

docstring(nix(templates),
    "Grimoire project templates from grimoire-templates flake. Use perceive(nix(templates)) to list available templates.").

% === MEMOIZED FLAKE INTROSPECTION ===

% Memoized flake target discovery - DO NOT TOUCH (critical for performance)
:- table get_nix_flake_targets/2.
get_nix_flake_targets(FlakeRef, Targets) :-
    magic_cast(conjure(shell(["nix", "flake", "show", "--json", FlakeRef])), Result),
    (Result = ok(result(JsonOutput, _Stderr)) ->
        atom_json_dict(JsonOutput, JsonDict, []),
        findall(Target, extract_flake_target(JsonDict, FlakeRef, Target), Targets)
    ;
        throw(error(flake_show_failed(FlakeRef, Result)))
    ).

% Extract flake targets from JSON - DO NOT TOUCH
extract_flake_target(JsonDict, FlakeRef, Target) :-
    % Extract packages
    get_dict(packages, JsonDict, Packages),
    dict_pairs(Packages, _, SystemPairs),
    member(System-SystemPackages, SystemPairs),
    dict_pairs(SystemPackages, _, PackagePairs),
    member(PackageName-_, PackagePairs),
    format(atom(AttrPath), 'packages.~w.~w', [System, PackageName]),
    Target = nix(target(package(FlakeRef, AttrPath))).

extract_flake_target(JsonDict, FlakeRef, Target) :-
    % Extract apps
    get_dict(apps, JsonDict, Apps),
    dict_pairs(Apps, _, SystemPairs),
    member(System-SystemApps, SystemPairs),
    dict_pairs(SystemApps, _, AppPairs),
    member(AppName-_, AppPairs),
    format(atom(AttrPath), 'apps.~w.~w', [System, AppName]),
    Target = nix(target(app(FlakeRef, AttrPath))).

extract_flake_target(JsonDict, FlakeRef, Target) :-
    % Extract devShells
    get_dict(devShells, JsonDict, DevShells),
    dict_pairs(DevShells, _, SystemPairs),
    member(System-SystemShells, SystemPairs),
    dict_pairs(SystemShells, _, ShellPairs),
    member(ShellName-_, ShellPairs),
    format(atom(AttrPath), 'devShells.~w.~w', [System, ShellName]),
    Target = nix(target(devShell(FlakeRef, AttrPath))).

extract_flake_target(JsonDict, FlakeRef, Target) :-
    % Extract checks
    get_dict(checks, JsonDict, Checks),
    dict_pairs(Checks, _, SystemPairs),
    member(System-SystemChecks, SystemPairs),
    dict_pairs(SystemChecks, _, CheckPairs),
    member(CheckName-_, CheckPairs),
    format(atom(AttrPath), 'checks.~w.~w', [System, CheckName]),
    Target = nix(target(check(FlakeRef, AttrPath))).

extract_flake_target(JsonDict, FlakeRef, Target) :-
    % Extract formatters
    (get_dict(formatter, JsonDict, Formatters) ->
        dict_pairs(Formatters, _, SystemPairs),
        member(System-_, SystemPairs),
        format(atom(AttrPath), 'formatter.~w', [System]),
        Target = nix(target(formatter(FlakeRef, AttrPath)))
    ; fail).

% === DYNAMIC ENTITY GENERATION ===

% Dynamic entity generation for flake targets - DO NOT TOUCH
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

% === TEMPLATE SYSTEM ===

% Template tools executables
component(nix(flake), templates_tools, tools(GetCmd, InitCmd)) :-
    grimoire_templates_tools_path(ToolsPath),
    directory_file_path(ToolsPath, 'getGrimoireTemplates', GetCmd),
    directory_file_path(ToolsPath, 'initGrimoireTemplate', InitCmd).

% Memoize the one-time JSON fetch - DO NOT TOUCH
:- table nix_flake_templates/1.
nix_flake_templates(Pairs) :-
    component(nix(flake), templates_tools, tools(GetCmd, _InitCmd)),
    magic_cast(conjure(shell([GetCmd])), Result),
    (Result = ok(result(JsonOutput, _Stderr)) ->
        atom_json_dict(JsonOutput, JsonDict, []),
        dict_pairs(JsonDict.templates, _Tag, Pairs)
    ;
        throw(error(template_fetch_failed(Result)))
    ).

% Component yields one template per backtrack - DO NOT TOUCH
component(nix(flake(template)), instance, nix(flake(template(Id))))  :-
    nix_flake_templates(Pairs),
    member(Id-_V, Pairs).

% Entity and docstring for templates - DO NOT TOUCH
entity(nix(flake(template(Id)))) :-
    nix_flake_templates(Pairs),
    member(Id-_, Pairs).

docstring(nix(flake(template(Id))), Desc) :-
    nix_flake_templates(Pairs),
    member(Id-V, Pairs),
    Desc = V.description.

% === PHASE 3: DSL PATTERNS ===

% Flake declaration schema with expansion
component(Entity, has(nix(flake)), nix(flake(ref(FlakeRef))))
    ==> component(Entity, nix_flake_ref, FlakeRef).

% Build target declaration expansion
component(Entity, has(nix(build_target)), nix(build_target(Target)))
    ==> component(Entity, nix_build_target_path, Target),
        component(Entity, nix_build_target_buildable, true).

% Dev environment declaration expansion
component(Entity, has(nix(dev_env)), nix(dev_env(shell(Shell))))
    ==> component(Entity, nix_dev_env_shell, Shell),
        component(Entity, nix_dev_env_available, true).

% === DYNAMIC COMPONENT GENERATION RULES ===

% These are NOT wrapped in ==> because they're generation rules querying external state
% They dynamically discover components from OS reality (nix flake show output)

% Flake targets - dynamic discovery from nix flake show
component(Entity, nix_flake_targets, Targets) :-
    component(Entity, nix_flake_ref, FlakeRef),
    get_nix_flake_targets(FlakeRef, Targets).

% Apps list generation from targets
component(Entity, nix_flake_apps, Apps) :-
    component(Entity, nix_flake_targets, Targets),
    findall(App, (member(nix(target(app(_, AttrPath))), Targets), App = AttrPath), Apps).

% Packages list generation from targets
component(Entity, nix_flake_packages, Packages) :-
    component(Entity, nix_flake_targets, Targets),
    findall(Pkg, (member(nix(target(package(_, AttrPath))), Targets), Pkg = AttrPath), Packages).

% Dev shells list generation from targets
component(Entity, nix_flake_dev_shells, DevShells) :-
    component(Entity, nix_flake_targets, Targets),
    findall(Shell, (member(nix(target(devShell(_, AttrPath))), Targets), Shell = AttrPath), DevShells).

% === PHASE 3: LEAF VERIFICATIONS ===

% Flake ref verification with filesystem checks for local paths
component(_, nix_flake_ref, Ref)
    :: (atom(Ref) ; string(Ref)),
       (Ref = '' -> throw(verification_error(nix, empty_flake_ref)) ; true),
       (   % Local path check (. or ./ or /)
           (atom_concat('./', _, Ref) ; atom_concat('/', _, Ref) ; Ref = '.')
       ->  % Verify flake.nix exists
           (atom(Ref) -> RefPath = Ref ; atom_string(RefPath, Ref)),
           atom_concat(RefPath, '/flake.nix', FlakePath),
           (exists_file(FlakePath) ->
               true
           ;
               throw(verification_error(nix, flake_path_not_found(Ref)))
           )
       ;   % URL or flake registry reference - no filesystem check
           true
       ).

% Targets list verification
component(_, nix_flake_targets, Targets)
    :: is_list(Targets).

% Apps list verification
component(_, nix_flake_apps, Apps)
    :: is_list(Apps).

% Packages list verification
component(_, nix_flake_packages, Packages)
    :: is_list(Packages).

% Dev shells list verification
component(_, nix_flake_dev_shells, DevShells)
    :: is_list(DevShells).

% Build target path verification
component(_, nix_build_target_path, Target)
    :: (atom(Target) ; string(Target)),
       validate_build_target_syntax(Target).

% Dev shell path verification
component(_, nix_dev_env_shell, Shell)
    :: (atom(Shell) ; string(Shell)),
       validate_dev_shell_syntax(Shell).

% === VALIDATION HELPERS ===

% Validate build target syntax (flake reference or attribute path)
validate_build_target_syntax(Target) :-
    (atom(Target) -> T = Target ; atom_string(T, Target)),
    % Valid forms: .#attr, path#attr, flakeref#attr, or just attr, or /nix/store/...
    (atom_concat(_, '#', T) ; sub_atom(T, 0, 5, _, '/nix/') ; atom(T)).

% Validate dev shell syntax (similar to build target)
validate_dev_shell_syntax(Shell) :-
    (atom(Shell) -> S = Shell ; atom_string(S, Shell)),
    (atom_concat(_, '#', S) ; atom(S)).

% === PHASE 3: SPELL IMPLEMENTATIONS ===

% Search for packages in nixpkgs
register_spell(
    perceive(nix(search)),
    input(nix(search(query('Query')))),
    output(either(
        ok(search_results('Results')),
        error(nix_error('Reason'))
    )),
    "Search for packages in nixpkgs. Query: search term(s).",
    [],
    implementation(perceive(nix(search(Query))), Result, (
        catch(
            (format(atom(FlakeQuery), 'nixpkgs#~w', [Query]),
             Args = ["nix", "search", FlakeQuery],
             magic_cast(conjure(shell(Args)), ShellResult),
             (ShellResult = ok(result(Results, _)) ->
                 Result = ok(search_results(Results))
             ;
                 Result = ShellResult
             )),
            Error,
            Result = error(nix_error(Error))
        )
    ))
).

% Search with custom flake
register_spell(
    perceive(nix(search_flake)),
    input(nix(search_flake(flake('Flake'), query('Query')))),
    output(either(
        ok(search_results('Results')),
        error(nix_error('Reason'))
    )),
    "Search for packages in a specific flake. Flake: flake reference, Query: search term(s).",
    [],
    implementation(perceive(nix(search_flake(Flake, Query))), Result, (
        catch(
            (format(atom(FlakeQuery), '~w#~w', [Flake, Query]),
             Args = ["nix", "search", FlakeQuery],
             magic_cast(conjure(shell(Args)), ShellResult),
             (ShellResult = ok(result(Results, _)) ->
                 Result = ok(search_results(Results))
             ;
                 Result = ShellResult
             )),
            Error,
            Result = error(nix_error(Error))
        )
    ))
).

% Run a Nix application
register_spell(
    conjure(nix(run)),
    input(nix(run(installable('Installable')))),
    output(either(
        ok(result(stdout('StdOut'), stderr('StdErr'))),
        error(run_error(exit('ExitCode'), stderr('StdErr')))
    )),
    "Run a Nix application. Installable: package/flake to run.",
    [],
    implementation(conjure(nix(run(Installable))), RetVal, (
        Args = ["nix", "run", Installable],
        magic_cast(conjure(shell(Args, interactive)), RetVal)
    ))
).

% Run with arguments
register_spell(
    conjure(nix(run_with_args)),
    input(nix(run_with_args(installable('Installable'), args('Args')))),
    output(either(
        ok(result(stdout('StdOut'), stderr('StdErr'))),
        error(run_error(exit('ExitCode'), stderr('StdErr')))
    )),
    "Run a Nix application with arguments. Installable: package/flake to run, Args: list of arguments to pass.",
    [],
    implementation(conjure(nix(run_with_args(Installable, AppArgs))), RetVal, (
        flatten([["nix", "run", Installable, "--"], AppArgs], Args),
        magic_cast(conjure(shell(Args, interactive)), RetVal)
    ))
).

% Build a Nix target
register_spell(
    conjure(nix(build)),
    input(nix(build(target('Target')))),
    output(either(
        ok(result(stdout('StdOut'), stderr('StdErr'))),
        error(build_error(exit('ExitCode'), stderr('StdErr')))
    )),
    "Build a Nix flake or derivation. Target: flake reference or installable to build (e.g., '.#default', 'nixpkgs#hello').",
    [],
    implementation(conjure(nix(build(Target))), Result, (
        Args = ["nix", "build", Target],
        magic_cast(conjure(shell(Args)), ShellResult),
        (ShellResult = ok(result(StdOut, StdErr)) ->
            Result = ok(result(StdOut, StdErr))
        ; ShellResult = error(shell_error(_, ExitCode, _StdOut, StdErr)) ->
            Result = error(build_error(ExitCode, StdErr))
        ;
            Result = ShellResult
        )
    ))
).

% Store garbage collection
register_spell(
    conjure(nix(store(gc))),
    input(nix(store(gc))),
    output(either(
        ok(result(stdout('StdOut'), stderr('StdErr'))),
        error(store_error(exit('ExitCode'), stderr('StdErr')))
    )),
    "Garbage collect unused store paths",
    [],
    implementation(conjure(nix(store(gc))), RetVal, (
        Args = ["nix", "store", "gc"],
        magic_cast(conjure(shell(Args)), RetVal)
    ))
).

% Store repair
register_spell(
    conjure(nix(store(repair))),
    input(nix(store(repair(path('Path'))))),
    output(either(
        ok(result(stdout('StdOut'), stderr('StdErr'))),
        error(store_error(exit('ExitCode'), stderr('StdErr')))
    )),
    "Repair corrupted store paths",
    [],
    implementation(conjure(nix(store(repair(Path)))), RetVal, (
        Args = ["nix", "store", "repair", Path],
        magic_cast(conjure(shell(Args)), RetVal)
    ))
).

% Store optimise
register_spell(
    conjure(nix(store(optimise))),
    input(nix(store(optimise))),
    output(either(
        ok(result(stdout('StdOut'), stderr('StdErr'))),
        error(store_error(exit('ExitCode'), stderr('StdErr')))
    )),
    "Optimize store by hardlinking identical files",
    [],
    implementation(conjure(nix(store(optimise))), RetVal, (
        Args = ["nix", "store", "optimise"],
        magic_cast(conjure(shell(Args)), RetVal)
    ))
).

% Why-depends query
register_spell(
    perceive(nix(why_depends)),
    input(nix(why_depends(pkg1('Pkg1'), pkg2('Pkg2')))),
    output(either(
        ok(dependency_chain('Result')),
        error(nix_error('Reason'))
    )),
    "Show why a package has another package in its closure. Essential for understanding dependency chains.",
    [],
    implementation(perceive(nix(why_depends(Pkg1, Pkg2))), Result, (
        catch(
            (Args = ["nix", "why-depends", Pkg1, Pkg2],
             magic_cast(conjure(shell(Args)), ShellResult),
             (ShellResult = ok(result(DepResult, _)) ->
                 Result = ok(dependency_info(DepResult))
             ;
                 Result = ShellResult
             )),
            Error,
            Result = error(nix_error(Error))
        )
    ))
).

% Build log query
register_spell(
    perceive(nix(log)),
    input(nix(log(installable('Installable')))),
    output(either(
        ok(build_log('Result')),
        error(nix_error('Reason'))
    )),
    "Show the build log of specified packages or paths. Essential for debugging build failures.",
    [],
    implementation(perceive(nix(log(Installable))), Result, (
        catch(
            (Args = ["nix", "log", Installable],
             magic_cast(conjure(shell(Args)), ShellResult),
             (ShellResult = ok(result(LogResult, _)) ->
                 Result = ok(build_log(LogResult))
             ;
                 Result = ShellResult
             )),
            Error,
            Result = error(nix_error(Error))
        )
    ))
).

% Develop environment
register_spell(
    conjure(nix(develop)),
    input(nix(develop(options('Options')))),
    output(either(
        ok(result(stdout('StdOut'), stderr('StdErr'))),
        error(develop_error(exit('ExitCode'), stderr('StdErr')))
    )),
    "Enters a development environment. Options: shell_cmd(Args) to execute command, phase(Phase) for specific build phase.",
    [],
    implementation(conjure(nix(develop(Options))), RetVal, (
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
        magic_cast(conjure(shell(Args, interactive)), RetVal)
    ))
).

% Flake new - initialize from template
register_spell(
    conjure(nix(flake(new))),
    input(nix(flake(new(template_id('TemplateId'), dest_path('DestPath'))))),
    output(either(
        ok(template_initialized('TemplateId', 'DestPath')),
        error(nix_error('Reason'))
    )),
    "Initialize a new flake from template. TemplateId: template ID (default: none), DestPath: where to create the new flake.",
    [],
    implementation(conjure(nix(flake(new(TemplateId, DestPath)))), RetVal, (
        component(nix(flake), templates_tools, tools(_GetCmd, InitCmd)),
        ( TemplateId = none -> TId = "default" ; atom_string(TemplateId, TId) ),
        atom_string(DestPath, DestPathStr),
        magic_cast(conjure(shell([InitCmd, TId, DestPathStr])), Result),
        (Result = ok(result(_Stdout, _Stderr)) ->
            RetVal = ok(template_initialized(TemplateId, DestPath))
        ; Result = error(Error) ->
            RetVal = error(nix_error(Error))
        ;
            RetVal = error(nix_error(unexpected_result))
        )
    ))
).

% Flake show - display flake metadata
register_spell(
    perceive(nix(flake(show))),
    input(nix(flake(show(ref('FlakeRef'))))),
    output(either(
        ok(flake_info(apps('Apps'), packages('Packages'), dev_shells('DevShells'))),
        error(nix_error('Reason'))
    )),
    "Show flake metadata, apps, and packages. FlakeRef: flake reference (path or URL).",
    [],
    implementation(perceive(nix(flake(show(FlakeRef)))), Result, (
        catch(
            (get_nix_flake_targets(FlakeRef, Targets),
             partition_flake_targets(Targets, Apps, Packages, DevShells),
             Result = ok(flake_info(apps(Apps), packages(Packages), dev_shells(DevShells)))
            ),
            Error,
            Result = error(nix_error(Error))
        )
    ))
).

% Helper: Partition flake targets by type
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

% List Grimoire templates
register_spell(
    perceive(nix(templates)),
    input(nix(templates)),
    output(either(
        ok(templates('TemplateList')),
        error(nix_error('Reason'))
    )),
    "List available Grimoire project templates from grimoire-templates flake.",
    [],
    implementation(perceive(nix(templates)), Result, (
        catch(
            (magic_cast(conjure(shell(["getGrimoireTemplates"])), ShellResult),
             (ShellResult = ok(result(JsonOutput, _)) ->
                 atom_json_dict(JsonOutput, Dict, []),
                 get_dict(templates, Dict, TemplatesDict),
                 dict_keys(TemplatesDict, TemplateList),
                 Result = ok(templates(TemplateList))
             ;
                 Result = error(nix_error(failed_to_get_templates))
             )),
            Error,
            Result = error(nix_error(Error))
        )
    ))
).
