:- use_module(library(plunit)).

% === DISCRIMINATIVE FLOW: VERIFICATION IMPLEMENTATIONS ===

% Main flake declaration verification - verify against OS reality
verify(component(_Entity, has(nix(flake)), nix(flake(ref(FlakeRef))))) :-
    % Verify flake exists and is valid by running nix flake show
    magic_cast(perceive(nix(flake(show(FlakeRef)))), Result),
    (Result = ok(_) ->
        true
    ; Result = error(_) ->
        throw(verification_error(nix, invalid_flake(FlakeRef)))
    ;
        throw(verification_error(nix, flake_verification_failed(FlakeRef)))
    ).

% Flake ref component verification - verify flake exists in nix
verify(component(_Entity, nix_flake_ref, FlakeRef)) :-
    magic_cast(perceive(nix(flake(show(FlakeRef)))), Result),
    (Result = ok(_) ->
        true
    ; Result = error(_Error) ->
        throw(verification_error(nix, flake_not_found(FlakeRef)))
    ;
        throw(verification_error(nix, flake_verification_failed(FlakeRef)))
    ).

% Flake targets component verification - verify targets exist in actual flake
verify(component(Entity, nix_flake_targets, ClaimedTargets)) :-
    user:please_verify(component(Entity, nix_flake_ref, FlakeRef)),
    magic_cast(perceive(nix(flake(show(FlakeRef)))), Result),
    (Result = ok(flake_info(apps(Apps), packages(Packages), dev_shells(DevShells))) ->
        (% Collect all actual targets from the flake
         append([Apps, Packages, DevShells], ActualTargets),
         % Verify claimed targets are a subset of actual targets
         forall(member(T, ClaimedTargets), member(T, ActualTargets)))
    ; Result = error(_) ->
        throw(verification_error(nix, flake_not_found(FlakeRef)))
    ;
        throw(verification_error(nix, verification_failed))
    ).

% Build target declaration verification - verify target is buildable
verify(component(Entity, has(nix(build_target)), nix(build_target(Target)))) :-
    user:please_verify(component(Entity, nix_build_target_path, Target)),
    % Actually attempt to evaluate the build (nix build --dry-run could work too)
    % For now, verify the target exists in the flake
    user:please_verify(component(Entity, nix_flake_ref, FlakeRef)),
    magic_cast(perceive(nix(flake(show(FlakeRef)))), Result),
    (Result = ok(flake_info(apps(Apps), packages(Packages), dev_shells(_DevShells))) ->
        (append(Apps, Packages, AllBuildables),
         % Extract just the target names
         findall(Name,
             (member(T, AllBuildables),
              (T = app(_, AttrPath) ; T = package(_, AttrPath)),
              atomic_list_concat(Parts, '.', AttrPath),
              last(Parts, Name)),
             BuildableNames),
         (member(Target, BuildableNames) ->
             true
         ;
             throw(verification_error(nix, build_target_not_found(Target)))
         ))
    ; Result = error(_) ->
        throw(verification_error(nix, flake_not_found(FlakeRef)))
    ;
        throw(verification_error(nix, verification_failed))
    ).

% Build target path verification - verify target exists in flake
verify(component(Entity, nix_build_target_path, Target)) :-
    user:please_verify(component(Entity, nix_flake_ref, FlakeRef)),
    magic_cast(perceive(nix(flake(show(FlakeRef)))), Result),
    (Result = ok(flake_info(apps(Apps), packages(Packages), dev_shells(_))) ->
        (append(Apps, Packages, AllTargets),
         % Extract just the target names from app(..., 'apps.system.name') and package(..., 'packages.system.name')
         findall(Name,
             (member(T, AllTargets),
              (T = app(_, AttrPath) ; T = package(_, AttrPath)),
              % Extract last part after final dot (e.g., 'apps.x86_64-linux.hello' -> 'hello')
              atomic_list_concat(Parts, '.', AttrPath),
              last(Parts, Name)),
             TargetNames),
         (member(Target, TargetNames) ->
             true
         ;
             throw(verification_error(nix, target_not_in_flake(Target)))
         ))
    ; Result = error(_) ->
        throw(verification_error(nix, flake_not_found(FlakeRef)))
    ;
        throw(verification_error(nix, verification_failed))
    ).

% Build target buildable flag verification
verify(component(Entity, nix_build_target_buildable, true)) :-
    user:please_verify(component(Entity, nix_build_target_path, _Target)).

% Dev environment declaration verification - verify devShell exists
verify(component(Entity, has(nix(dev_env)), nix(dev_env(shell(Shell))))) :-
    user:please_verify(component(Entity, nix_flake_ref, FlakeRef)),
    magic_cast(perceive(nix(flake(show(FlakeRef)))), Result),
    (Result = ok(flake_info(apps(_Apps), packages(_Packages), dev_shells(DevShells))) ->
        (% Extract just the shell names
         findall(Name,
             (member(devShell(_, AttrPath), DevShells),
              atomic_list_concat(Parts, '.', AttrPath),
              last(Parts, Name)),
             ShellNames),
         (member(Shell, ShellNames) ->
             true
         ;
             throw(verification_error(nix, dev_shell_not_found(Shell)))
         ))
    ; Result = error(_) ->
        throw(verification_error(nix, flake_not_found(FlakeRef)))
    ;
        throw(verification_error(nix, verification_failed))
    ).

% Dev env shell verification - verify shell exists in flake
verify(component(Entity, nix_dev_env_shell, Shell)) :-
    user:please_verify(component(Entity, nix_flake_ref, FlakeRef)),
    magic_cast(perceive(nix(flake(show(FlakeRef)))), Result),
    (Result = ok(flake_info(apps(_Apps), packages(_Packages), dev_shells(DevShells))) ->
        (% Extract just the shell names from devShell(..., 'devShells.system.name')
         findall(Name,
             (member(devShell(_, AttrPath), DevShells),
              atomic_list_concat(Parts, '.', AttrPath),
              last(Parts, Name)),
             ShellNames),
         (member(Shell, ShellNames) ->
             true
         ;
             throw(verification_error(nix, shell_not_in_flake(Shell)))
         ))
    ; Result = error(_) ->
        throw(verification_error(nix, flake_not_found(FlakeRef)))
    ;
        throw(verification_error(nix, verification_failed))
    ).

% Dev env available flag verification
verify(component(Entity, nix_dev_env_available, true)) :-
    user:please_verify(component(Entity, nix_dev_env_shell, _Shell)).

% === PLUNIT TESTS ===

:- begin_tests(nix).

% === BASIC EXISTENCE TESTS ===

% Debug test - check what entities exist at start of nix tests
test(debug_entities_at_start, [true]) :-
    open('/tmp/nix_test_debug.txt', write, Stream),
    findall(E, entity(E), Entities),
    length(Entities, N),
    format(Stream, 'At start of nix tests: ~w entities exist~n', [N]),
    (entity(nix) -> format(Stream, '  entity(nix) EXISTS~n', []) ; format(Stream, '  entity(nix) MISSING~n', [])),
    (user:entity(nix) -> format(Stream, '  user:entity(nix) EXISTS~n', []) ; format(Stream, '  user:entity(nix) MISSING~n', [])),
    (component(nix, defined, true) -> format(Stream, '  component(nix, defined, true) EXISTS~n', []) ; format(Stream, '  component(nix, defined, true) MISSING~n', [])),
    (user:component(nix, defined, true) -> format(Stream, '  user:component(nix, defined, true) EXISTS~n', []) ; format(Stream, '  user:component(nix, defined, true) MISSING~n', [])),
    % Check if the rule exists in current module
    (clause(component(_, defined, true), entity(_)) -> format(Stream, '  RULE component(Entity, defined, true) :- entity(Entity) EXISTS (current)~n', []) ; format(Stream, '  RULE component(Entity, defined, true) :- entity(Entity) MISSING (current)~n', [])),
    % Check if the rule exists in user module
    (user:clause(component(_, defined, true), entity(_)) -> format(Stream, '  RULE user:component(Entity, defined, true) :- user:entity(Entity) EXISTS~n', []) ; format(Stream, '  RULE user:component(Entity, defined, true) :- user:entity(Entity) MISSING~n', [])),
    % Check what clauses exist for component/3
    findall(M:H-B, clause(M:component(H1, H2, H3), B), Clauses),
    length(Clauses, NC),
    format(Stream, '  Total component/3 clauses visible: ~w~n', [NC]),
    findall(M2:H2-B2, user:clause(component(Ha, Hb, Hc), B2), UserClauses),
    length(UserClauses, NUC),
    format(Stream, '  Total user:component/3 clauses visible: ~w~n', [NUC]),
    % Check for the specific rule we're looking for
    findall(Body, user:clause(component(_, defined, true), Body), DefinedBodies),
    length(DefinedBodies, NDB),
    format(Stream, '  component(_,defined,true) clauses: ~w~n', [NDB]),
    (DefinedBodies \= [] -> format(Stream, '    First body: ~w~n', [DefinedBodies]) ; true),
    (entity(utils) -> format(Stream, '  entity(utils) EXISTS~n', []) ; format(Stream, '  entity(utils) MISSING~n', [])),
    (entity(fs) -> format(Stream, '  entity(fs) EXISTS~n', []) ; format(Stream, '  entity(fs) MISSING~n', [])),
    close(Stream), !.

test(nix_entity_exists, [true]) :-
    user:please_verify(component(nix, defined, true)), !.

test(nix_command_constructors, [true]) :-
    user:please_verify(component(conjure, ctor, nix(build))),
    user:please_verify(component(conjure, ctor, nix(develop))),
    user:please_verify(component(conjure, ctor, nix(run))), !.

test(nix_target_constructors, [true]) :-
    user:please_verify(component(nix(target), ctor, package)),
    user:please_verify(component(nix(target), ctor, app)),
    user:please_verify(component(nix(target), ctor, devShell)), !.

test(nix_docstrings_exist, [
    forall(component(conjure, ctor, nix(Cmd)))
]) :-
    docstring(nix(Cmd), _).

% === SPELL FORMAT REGISTRATION TESTS ===

test(nix_build_spell_registered, [true]) :-
    register_spell(conjure(nix(build)), _, _, _).

test(nix_run_spell_registered, [true]) :-
    register_spell(conjure(nix(run)), _, _, _).

test(nix_flake_show_spell_registered, [true]) :-
    register_spell(perceive(nix(flake(show))), _, _, _).

% === DSL PATTERN TESTS WITH PLEASE_VERIFY ===

% Test flake declaration expansion with assertz→please_verify→retractall pattern
test(nix_flake_dsl_expansion, [
    setup(setup_mock_flake_component),
    cleanup(cleanup_mock_flake_component)
]) :-
    % Assert mock flake declaration (use local test_flake)
    user:assertz(component(test_nix_entity, has(nix(flake)), nix(flake(ref('./src/nix/test_flake'))))),
    % Verify using please_verify (calls verify/1 under the hood)
    user:please_verify(component(test_nix_entity, has(nix(flake)), nix(flake(ref('./src/nix/test_flake'))))),
    % Verify generated components exist
    user:please_verify(component(test_nix_entity, nix_flake_ref, './src/nix/test_flake')), !.

% Test build target declaration expansion
test(nix_build_target_dsl_expansion, [
    setup(setup_mock_build_target),
    cleanup(cleanup_mock_build_target)
]) :-
    % Assert mock build target declaration
    user:assertz(component(test_build_entity, has(nix(build_target)), nix(build_target('.#default')))),
    % Verify generated components exist (derived from DSL pattern)
    user:please_verify(component(test_build_entity, nix_build_target_path, '.#default')),
    user:please_verify(component(test_build_entity, nix_build_target_buildable, true)), !.

% Test dev environment declaration expansion
test(nix_dev_env_dsl_expansion, [
    setup(setup_mock_dev_env),
    cleanup(cleanup_mock_dev_env)
]) :-
    % Assert mock dev env declaration
    user:assertz(component(test_dev_entity, has(nix(dev_env)), nix(dev_env(shell('default'))))),
    % Verify generated components exist (derived from DSL pattern)
    user:please_verify(component(test_dev_entity, nix_dev_env_shell, 'default')),
    user:please_verify(component(test_dev_entity, nix_dev_env_available, true)), !.

% === VERIFY/1 OVERLOAD TESTS ===

% Test flake ref verification with known-good flake
test(verify_flake_ref_syntax_valid, [
    setup(setup_flake_ref_test),
    cleanup(cleanup_flake_ref_test)
]) :-
    user:assertz(component(test_flake_ref, nix_flake_ref, './src/nix/test_flake')),
    user:please_verify(component(test_flake_ref, nix_flake_ref, './src/nix/test_flake')), !.

test(verify_flake_ref_syntax_invalid, [
    setup(setup_flake_ref_test),
    cleanup(cleanup_flake_ref_test),
    throws(verification_error(nix, _))
]) :-
    user:assertz(component(test_bad_flake, nix_flake_ref, '')),
    user:please_verify(component(test_bad_flake, nix_flake_ref, '')).

% Test build target path verification
test(verify_build_target_syntax_valid, [
    setup(setup_build_target_test),
    cleanup(cleanup_build_target_test)
]) :-
    user:assertz(component(test_build, nix_flake_ref, './src/nix/test_flake')),
    user:assertz(component(test_build, nix_build_target_path, 'hello')),
    user:please_verify(component(test_build, nix_build_target_path, 'hello')), !.

test(verify_build_target_syntax_invalid, [
    setup(setup_build_target_test),
    cleanup(cleanup_build_target_test),
    throws(verification_error(nix, _))
]) :-
    user:assertz(component(test_bad_build, nix_flake_ref, './src/nix/test_flake')),
    user:assertz(component(test_bad_build, nix_build_target_path, 'nonexistent_target_xyz')),
    user:please_verify(component(test_bad_build, nix_build_target_path, 'nonexistent_target_xyz')).

% Test dev shell verification
test(verify_dev_shell_syntax_valid, [
    setup(setup_dev_shell_test),
    cleanup(cleanup_dev_shell_test)
]) :-
    user:assertz(component(test_shell, nix_flake_ref, './src/nix/test_flake')),
    user:assertz(component(test_shell, nix_dev_env_shell, 'default')),
    user:please_verify(component(test_shell, nix_dev_env_shell, 'default')), !.

% === MOCK SETUP/CLEANUP HELPERS ===

setup_flake_ref_test :-
    true.

cleanup_flake_ref_test :-
    forall(
        clause(user:component(test_flake_ref, C, V), true),
        retract(user:component(test_flake_ref, C, V))
    ),
    forall(
        clause(user:component(test_bad_flake, C, V), true),
        retract(user:component(test_bad_flake, C, V))
    ).

setup_build_target_test :-
    true.

cleanup_build_target_test :-
    forall(
        clause(user:component(test_build, C, V), true),
        retract(user:component(test_build, C, V))
    ),
    forall(
        clause(user:component(test_bad_build, C, V), true),
        retract(user:component(test_bad_build, C, V))
    ).

setup_dev_shell_test :-
    true.

cleanup_dev_shell_test :-
    forall(
        clause(user:component(test_shell, C, V), true),
        retract(user:component(test_shell, C, V))
    ).

setup_mock_flake_component :-
    user:assertz(entity(test_nix_entity)).

cleanup_mock_flake_component :-
    user:retractall(entity(test_nix_entity)),
    forall(
        clause(user:component(test_nix_entity, C, V), true),
        retract(user:component(test_nix_entity, C, V))
    ).

setup_mock_build_target :-
    user:assertz(entity(test_build_entity)).

cleanup_mock_build_target :-
    user:retractall(entity(test_build_entity)),
    forall(
        clause(user:component(test_build_entity, C, V), true),
        retract(user:component(test_build_entity, C, V))
    ).

setup_mock_dev_env :-
    user:assertz(entity(test_dev_entity)).

cleanup_mock_dev_env :-
    user:retractall(entity(test_dev_entity)),
    forall(
        clause(user:component(test_dev_entity, C, V), true),
        retract(user:component(test_dev_entity, C, V))
    ).

% === TEMPLATE SYSTEM TESTS ===

% Test listing templates
test(nix_list_templates) :-
    magic_cast(perceive(nix(templates)), Result),
    assertion((Result = ok(templates(TemplateList)), is_list(TemplateList))).

:- end_tests(nix).
