:- use_module(library(plunit)).

% Note: ecs_kernel.plt and grimoire.plt are automatically loaded by run_tests.pl
% This provides please_verify/1 to all test files

% === DISCRIMINATIVE FLOW: GRIMOIRE SYSTEM TESTS ===

:- begin_tests(grimoire_core).

% Test subsystem loading using component pattern with please_verify
test(subsystems_loaded) :-
    user:please_verify(component(system, subsystem, git)),
    user:please_verify(component(system, subsystem, nix)),
    user:please_verify(component(system, subsystem, fs)),
    user:please_verify(component(system, subsystem, project)),
    user:please_verify(component(system, subsystem, session)),
    user:please_verify(component(system, subsystem, golems)), !.

% Test spell constructors using component pattern
test(spell_constructors) :-
    user:please_verify(component(spell, ctor, conjure)),
    user:please_verify(component(spell, ctor, perceive)), !.

% Test grimoire-level conjure constructors
test(grimoire_conjure_constructors) :-
    user:please_verify(component(conjure, ctor, shell)),
    user:please_verify(component(conjure, ctor, executable_program)), !.

% Test grimoire-level perceive constructors
test(grimoire_perceive_constructors) :-
    user:please_verify(component(perceive, ctor, entities)), !.

% Test spell format registrations exist for grimoire-level spells only
test(spell_format_registrations) :-
    register_spell(conjure(shell), input(_), output(_), docstring(_)),
    register_spell(conjure(executable_program), input(_), output(_), docstring(_)),
    register_spell(perceive(entities), input(_), output(_), docstring(_)).

% Test grimoire shell spell execution
test(grimoire_shell_spell) :-
    cast(conjure(shell(['echo', 'test'])), ok(result(Output, _))),
    sub_string(Output, _, _, _, 'test'), !.

% Test grimoire executable_program spell
test(grimoire_executable_program) :-
    cast(conjure(executable_program(echo, ['hello'])), ok(result(Output, _))),
    sub_string(Output, _, _, _, 'hello'), !.

% Test perceive entities query
test(grimoire_entities_query) :-
    cast(perceive(entities), ok(entity_list(Entities))),
    assertion(is_list(Entities)),
    assertion(member(system, Entities)), !.

% Test system entity existence
test(system_entity_exists) :-
    user:please_verify(component(system, defined, true)).

% Test system docstring exists
test(system_docstring_exists) :-
    user:please_verify(component(system, docstring, _Doc)).

% Test source/semantic infrastructure
test(semantic_entities_exist) :-
    user:please_verify(component(source, defined, true)),
    user:please_verify(component(semantic, defined, true)).

:- end_tests(grimoire_core).
