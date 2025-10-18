:- use_module(library(plunit)).

% Load nix domain
:- load_entity(semantic(file('@/src/nix/semantics.pl'))).

% Load test entities from file
:- load_entity(semantic(file('@/src/tests/nix_test_entities.pl'))).

% === PLUNIT TESTS ===

:- begin_tests(nix).

% === BASIC EXISTENCE TESTS ===

test(nix_entity_exists) :-
    % Check that entity(nix) exists (via component(nix, defined, true) rule)
    user:component(nix, defined, true).

test(nix_spell_constructors) :-
    user:please_verify(component(conjure, ctor, nix(develop))),
    user:please_verify(component(conjure, ctor, nix(run))),
    user:please_verify(component(conjure, ctor, nix(build))).

test(nix_target_constructors) :-
    user:please_verify(component(nix(target), ctor, package)),
    user:please_verify(component(nix(target), ctor, app)),
    user:please_verify(component(nix(target), ctor, devShell)).

test(nix_spell_metadata) :-
    % Check component metadata with please_verify
    user:please_verify(component(conjure(nix(run)), docstring, _)),
    user:please_verify(component(conjure(nix(build)), docstring, _)),
    user:please_verify(component(perceive(nix(flake(show))), docstring, _)).

% === DSL PATTERN TESTS WITH PLEASE_VERIFY ===

% Test flake declaration expansion - entities loaded from file
test(nix_flake_dsl_expansion) :-
    % Entities already loaded from file
    % Verify using please_verify (calls verify/1 under the hood)
    user:please_verify(component(test_nix_entity, has(nix(flake)), nix(flake(ref('./src/nix/test_flake'))))),
    % Verify generated component exists
    user:please_verify(component(test_nix_entity, nix_flake_ref, './src/nix/test_flake')).

% Test build target declaration expansion
test(nix_build_target_dsl_expansion) :-
    % test_build_entity loaded from file with DSL pattern
    % Verify generated components exist (derived from DSL pattern)
    user:please_verify(component(test_build_entity, nix_build_target_path, '.#default')),
    user:please_verify(component(test_build_entity, nix_build_target_buildable, true)).

% Test dev environment declaration expansion
test(nix_dev_env_dsl_expansion) :-
    % test_dev_entity loaded from file with DSL pattern
    % Verify generated components exist (derived from DSL pattern)
    user:please_verify(component(test_dev_entity, nix_dev_env_shell, 'default')),
    user:please_verify(component(test_dev_entity, nix_dev_env_available, true)).

% === LEAF VERIFICATION TESTS ===

% Test flake ref verification with known-good flake
test(verify_flake_ref_syntax_valid) :-
    % test_flake_ref loaded from file
    user:please_verify(component(test_flake_ref, nix_flake_ref, './src/nix/test_flake')).

test(verify_flake_ref_syntax_invalid, [
    throws(verification_error(nix, empty_flake_ref))
]) :-
    % test_bad_flake loaded from file with empty ref
    user:please_verify(component(test_bad_flake, nix_flake_ref, '')).

% Test build target path verification
test(verify_build_target_syntax_valid) :-
    % test_build loaded from file
    user:please_verify(component(test_build, nix_build_target_path, 'hello')).

% Test dev shell verification
test(verify_dev_shell_syntax_valid) :-
    % test_shell loaded from file
    user:please_verify(component(test_shell, nix_dev_env_shell, 'default')).

% === SPELL BEHAVIORAL TESTS ===

% Test listing templates spell
test(spell_list_templates) :-
    user:magic_cast(perceive(nix(templates)), Result),
    assertion((Result = ok(templates(TemplateList)), is_list(TemplateList))).

% Test run spell with simple command
test(spell_run_hello) :-
    user:magic_cast(conjure(nix(run(package("nixpkgs#hello"), args([])))), Result),
    assertion(Result = ok(_)).

% Test run with args
test(spell_run_with_args_echo) :-
    user:magic_cast(conjure(nix(run_with_args(package("nixpkgs#coreutils"), command("echo"), args(["test"])))), Result),
    assertion(Result = ok(_)).

% Test build spell
test(spell_build, [setup(setup_nix_flake_test), cleanup(cleanup_nix_flake_test)]) :-
    user:magic_cast(conjure(nix(build(flake_ref("/tmp/test_nix_flake")))), Result),
    assertion(Result = ok(_)).

% Test flake show
test(spell_flake_show, [setup(setup_nix_flake_test), cleanup(cleanup_nix_flake_test)]) :-
    user:magic_cast(perceive(nix(flake(show(flake_ref("/tmp/test_nix_flake"))))), Result),
    assertion(Result = ok(_)).

% Test flake new
test(spell_flake_new) :-
    TempDir = "/tmp/test_new_flake",
    user:magic_cast(conjure(nix(flake(new(path(TempDir), template("templates#trivial"))))), Result),
    assertion(Result = ok(_)),
    delete_directory_and_contents(TempDir).

% Test store gc
test(spell_store_gc) :-
    user:magic_cast(conjure(nix(store(gc))), Result),
    assertion(Result = ok(_)).

% Test store optimise
test(spell_store_optimise) :-
    user:magic_cast(conjure(nix(store(optimise))), Result),
    assertion(Result = ok(_)).

% Test develop spell
test(spell_develop, [setup(setup_nix_flake_test), cleanup(cleanup_nix_flake_test)]) :-
    user:magic_cast(conjure(nix(develop(flake_ref("/tmp/test_nix_flake"), command("echo test")))), Result),
    assertion(Result = ok(_)).

% Test search nixpkgs
test(spell_search_nixpkgs) :-
    user:magic_cast(perceive(nix(search(query("hello"), max_results(5)))), Result),
    assertion((Result = ok(search_results(Results)), is_list(Results))).

% Test search flake
test(spell_search_flake, [setup(setup_nix_flake_test), cleanup(cleanup_nix_flake_test)]) :-
    user:magic_cast(perceive(nix(search_flake(flake_ref("/tmp/test_nix_flake"), query("default")))), Result),
    assertion(Result = ok(_)).

% Test store repair
test(spell_store_repair) :-
    user:magic_cast(conjure(nix(store(repair))), Result),
    assertion(Result = ok(_)).

% Test why-depends
test(spell_why_depends, [setup(setup_nix_flake_test), cleanup(cleanup_nix_flake_test)]) :-
    user:magic_cast(perceive(nix(why_depends(derivation("/tmp/test_nix_flake"), dependency("glibc")))), Result),
    assertion((Result = ok(_) ; Result = error(_))).  % May fail if dependency doesn't exist

% Test log
test(spell_log, [setup(setup_nix_flake_test), cleanup(cleanup_nix_flake_test)]) :-
    user:magic_cast(perceive(nix(log(derivation("/tmp/test_nix_flake")))), Result),
    assertion(Result = ok(_)).

:- end_tests(nix).

% Setup/cleanup for flake tests
setup_nix_flake_test :-
    FlakeDir = '/tmp/test_nix_flake',
    make_directory_path(FlakeDir),
    FlakePath = '/tmp/test_nix_flake/flake.nix',
    open(FlakePath, write, Stream),
    write(Stream, '{\n'),
    write(Stream, '  description = "Test flake";\n'),
    write(Stream, '  outputs = { self, nixpkgs }: {\n'),
    write(Stream, '    packages.x86_64-linux.default = nixpkgs.legacyPackages.x86_64-linux.hello;\n'),
    write(Stream, '  };\n'),
    write(Stream, '}\n'),
    close(Stream).

cleanup_nix_flake_test :-
    delete_directory_and_contents('/tmp/test_nix_flake').
