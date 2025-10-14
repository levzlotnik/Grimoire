:- use_module(library(plunit)).

% Load test entities from file
:- load_entity(semantic(file('@/src/tests/nix_test_entities.pl'))).

% === DISCRIMINATIVE FLOW: VERIFICATION IMPLEMENTATIONS ===

% Nix entity verification
verify(component(nix, concept, Concept)) :-
    member(Concept, [nix(store), nix(derivation), nix(package), nix(target),
                     nix(flake), nix(develop), nix(search), nix(run)]).

% Store entity and constructors verification
verify(component(nix(store), ctor, Ctor)) :-
    member(Ctor, [gc, repair, optimise]).

% Package constructors verification
verify(component(nix(package), ctor, Ctor)) :-
    member(Ctor, [versioned, flake]).

% Target constructors verification
verify(component(nix(target), ctor, Ctor)) :-
    member(Ctor, [package, app, devShell, check, formatter]).

% Develop options verification
verify(component(nix(develop), option(unique), Option)) :-
    member(Option, [shell_cmd, phase]).

% Develop phase constructors verification
verify(component(nix(develop(phase)), ctor, Phase)) :-
    member(Phase, [unpack, configure, build, check, install, installcheck]).

% Flake constructors verification
verify(component(nix(flake), ctor, new)).

% Store relationships verification
verify(component(nix(store), contains, nix(derivation))).
verify(component(nix(derivation), outputs, nix(package))).
verify(component(nix(flake), exposes, nix(target))).
verify(component(nix(target), builds_to, nix(derivation))).

% Main flake declaration verification - verify against OS reality
verify(component(_Entity, has(nix(flake)), nix(flake(ref(FlakeRef))))) :-
    % Verify flake.nix exists at the reference path
    (FlakeRef = '.' ->
        working_directory(Dir, Dir),
        atom_concat(Dir, '/flake.nix', FlakePath)
    ; atom_concat(FlakeRef, '/flake.nix', FlakePath)
    ),
    (exists_file(FlakePath) ->
        true
    ;
        throw(verification_error(nix, flake_not_found(FlakeRef)))
    ).

% Flake ref component verification - verify flake exists
verify(component(_Entity, nix_flake_ref, FlakeRef)) :-
    atom(FlakeRef) ->
        (FlakeRef = '.' ->
            working_directory(Dir, Dir),
            atom_concat(Dir, '/flake.nix', FlakePath)
        ; atom_concat(FlakeRef, '/flake.nix', FlakePath)
        ),
        (exists_file(FlakePath) ->
            true
        ;
            throw(verification_error(nix, flake_not_found(FlakeRef)))
        )
    ;
        throw(verification_error(nix, invalid_flake_ref(FlakeRef))).

% Flake targets component verification - verify targets list is valid
verify(component(Entity, nix_flake_targets, ClaimedTargets)) :-
    user:please_verify(component(Entity, nix_flake_ref, _FlakeRef)),
    is_list(ClaimedTargets) ->
        true
    ;
        throw(verification_error(nix, invalid_targets_list(ClaimedTargets))).

% Flake apps component verification - verify apps list is valid
verify(component(Entity, nix_flake_apps, Apps)) :-
    user:please_verify(component(Entity, nix_flake_targets, _Targets)),
    (is_list(Apps) ->
        true
    ;
        throw(verification_error(nix, invalid_apps_list(Apps)))
    ).

% Flake packages component verification - verify packages list is valid
verify(component(Entity, nix_flake_packages, Packages)) :-
    user:please_verify(component(Entity, nix_flake_targets, _Targets)),
    (is_list(Packages) ->
        true
    ;
        throw(verification_error(nix, invalid_packages_list(Packages)))
    ).

% Flake dev shells component verification - verify dev shells list is valid
verify(component(Entity, nix_flake_dev_shells, DevShells)) :-
    user:please_verify(component(Entity, nix_flake_targets, _Targets)),
    (is_list(DevShells) ->
        true
    ;
        throw(verification_error(nix, invalid_dev_shells_list(DevShells)))
    ).

% Build target declaration verification - compose sub-component verifications
verify(component(Entity, has(nix(build_target)), nix(build_target(Target)))) :-
    user:please_verify(component(Entity, nix_build_target_path, Target)),
    user:please_verify(component(Entity, nix_build_target_buildable, true)).

% Build target path verification - verify target is an atom
verify(component(_Entity, nix_build_target_path, Target)) :-
    (atom(Target) ->
        true
    ;
        throw(verification_error(nix, invalid_target_path(Target)))
    ).

% Build target buildable flag verification - verify target passes syntax validation
verify(component(Entity, nix_build_target_buildable, true)) :-
    user:please_verify(component(Entity, nix_build_target_path, Target)),
    (validate_build_target_syntax(Target) ->
        true
    ;
        throw(verification_error(nix, unbuildable_target(Target)))
    ).

% Dev environment declaration verification - compose sub-component verifications
verify(component(Entity, has(nix(dev_env)), nix(dev_env(shell(Shell))))) :-
    user:please_verify(component(Entity, nix_dev_env_shell, Shell)),
    user:please_verify(component(Entity, nix_dev_env_available, true)).

% Dev env shell verification - verify shell syntax
verify(component(_Entity, nix_dev_env_shell, Shell)) :-
    (atom(Shell) ->
        (validate_dev_shell_syntax(Shell) ->
            true
        ;
            throw(verification_error(nix, invalid_shell_syntax(Shell)))
        )
    ;
        throw(verification_error(nix, invalid_shell_name(Shell)))
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

test(nix_run_spell_registered, [true]) :-
    register_spell(conjure(nix(run)), _, _, _).

test(nix_flake_show_spell_registered, [true]) :-
    register_spell(perceive(nix(flake(show))), _, _, _).

% === DSL PATTERN TESTS WITH PLEASE_VERIFY ===

% Test flake declaration expansion - entities loaded from file
test(nix_flake_dsl_expansion, [true]) :-
    % Entities already loaded from file
    % Verify using please_verify (calls verify/1 under the hood)
    user:please_verify(component(test_nix_entity, has(nix(flake)), nix(flake(ref('./src/nix/test_flake'))))),
    % Verify generated components exist
    user:please_verify(component(test_nix_entity, nix_flake_ref, './src/nix/test_flake')), !.

% Test build target declaration expansion
test(nix_build_target_dsl_expansion) :-
    % test_build_entity loaded from file with DSL pattern
    % Verify generated components exist (derived from DSL pattern)
    user:please_verify(component(test_build_entity, nix_build_target_path, '.#default')),
    user:please_verify(component(test_build_entity, nix_build_target_buildable, true)), !.

% Test dev environment declaration expansion
test(nix_dev_env_dsl_expansion) :-
    % test_dev_entity loaded from file with DSL pattern
    % Verify generated components exist (derived from DSL pattern)
    user:please_verify(component(test_dev_entity, nix_dev_env_shell, 'default')),
    user:please_verify(component(test_dev_entity, nix_dev_env_available, true)), !.

% === VERIFY/1 OVERLOAD TESTS ===

% Test flake ref verification with known-good flake
test(verify_flake_ref_syntax_valid) :-
    % test_flake_ref loaded from file
    user:please_verify(component(test_flake_ref, nix_flake_ref, './src/nix/test_flake')), !.

test(verify_flake_ref_syntax_invalid, [
    throws(verification_error(nix, _))
]) :-
    % test_bad_flake loaded from file with empty ref
    user:please_verify(component(test_bad_flake, nix_flake_ref, '')).

% Test build target path verification
test(verify_build_target_syntax_valid) :-
    % test_build loaded from file
    user:please_verify(component(test_build, nix_build_target_path, 'hello')), !.

test(verify_build_target_buildable_invalid) :-
    % test_bad_build loaded from file with invalid target
    % The buildable component should not exist because validation fails
    \+ component(test_bad_build, nix_build_target_buildable, true).

% Test dev shell verification
test(verify_dev_shell_syntax_valid) :-
    % test_shell loaded from file
    user:please_verify(component(test_shell, nix_dev_env_shell, 'default')), !.

% === MOCK SETUP/CLEANUP HELPERS ===


% === TEMPLATE SYSTEM TESTS ===

% Test listing templates
test(nix_list_templates) :-
    user:magic_cast(perceive(nix(templates)), Result),
    assertion((Result = ok(templates(TemplateList)), is_list(TemplateList))).

:- end_tests(nix).
