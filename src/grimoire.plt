:- use_module(library(plunit)).

%% ============================================================================
%% Test Spell Registrations (OUTSIDE test block for user module visibility)
%% ============================================================================

% Simple test spell
register_spell(
    conjure(test_spell(simple)),
    input(test_spell(simple(arg('Arg')))),
    output(either(ok(result('Result')), error(test_error('Reason')))),
    "Test spell for grimoire.plt verification",
    [],
    implementation(conjure(test_spell(simple(Arg))), Result, (
        string(Arg),
        Result = ok(result(Arg))
    ))
).

% Persistent test spell
register_spell(
    conjure(test_spell(persistent)),
    input(test_spell(persistent(data('Data')))),
    output(ok(saved('Data'))),
    "Test spell with session persistence",
    [session_persistent(true)],
    implementation(conjure(test_spell(persistent(Data))), Result, (
        Result = ok(saved(Data))
    ))
).

% Query test spell
register_spell(
    perceive(test_spell(query)),
    input(test_spell(query(entity('Entity'), component_type('Type')))),
    output(either(ok(value('Value')), error(not_found))),
    "Test spell that queries components",
    [],
    implementation(perceive(test_spell(query(Entity, Type))), Result, (
        atom(Entity),
        atom(Type),
        (component(Entity, Type, Value)
        -> Result = ok(value(Value))
        ; Result = error(not_found))
    ))
).

%% ============================================================================
%% Test Suite
%% ============================================================================

:- begin_tests(grimoire).

%% Test Case 1: Spell registration generates metadata

test(spell_generates_ctor_metadata) :-
    user:please_verify(component(conjure, ctor, test_spell(simple))),
    user:please_verify(component(conjure, ctor, test_spell(persistent))),
    user:please_verify(component(perceive, ctor, test_spell(query))).

test(spell_generates_docstring_metadata) :-
    user:please_verify(component(conjure(test_spell(simple)), docstring, Doc)),
    assertion(Doc = "Test spell for grimoire.plt verification").

test(spell_generates_format_input_metadata) :-
    user:please_verify(component(conjure(test_spell(simple)), format_input, Input)),
    assertion(Input = input(test_spell(simple(arg(_))))).

test(spell_generates_format_output_metadata) :-
    user:please_verify(component(conjure(test_spell(simple)), format_output, Output)),
    assertion(Output = output(either(ok(result(_)), error(test_error(_))))).

test(spell_generates_spell_options_metadata) :-
    user:please_verify(component(conjure(test_spell(simple)), spell_options, Options)),
    assertion(Options = []).

%% Test Case 2: magic_cast executes spell

test(magic_cast_executes_spell) :-
    user:magic_cast(conjure(test_spell(simple("foo"))), Result),
    assertion(Result = ok(result("foo"))).

test(magic_cast_requires_grounded_input, [throws(error(instantiation_error(_), _))]) :-
    user:magic_cast(conjure(test_spell(simple(_UnboundArg))), _Result).

test(magic_cast_requires_registered_spell, [throws(error(existence_error(spell, _), _))]) :-
    user:magic_cast(conjure(nonexistent_spell(foo)), _Result).

%% Test Case 3: cast_impl guard prevents direct calls

test(cast_impl_guard_forbids_direct_call, [throws(error(direct_cast_forbidden(_), _))]) :-
    cast_impl(conjure(test_spell(simple(foo))), _Result).

%% Test Case 4: Session-persistent flag detection

test(persistent_spell_has_option) :-
    user:please_verify(component(conjure(test_spell(persistent)), spell_options, Options)),
    assertion(Options = [session_persistent(true)]).

test(magic_cast_detects_persistent_flag) :-
    user:magic_cast(conjure(test_spell(persistent("test_data"))), Result),
    assertion(Result = ok(saved("test_data"))).

%% Test Case 5: find_matching_spell_sig helper - must be unique

test(find_matching_spell_for_simple) :-
    user:find_matching_spell_sig(conjure(test_spell(simple("foo"))), SpellSig),
    assertion(SpellSig = conjure(test_spell(simple))).

test(find_matching_spell_for_query) :-
    user:find_matching_spell_sig(perceive(test_spell(query(system, root_dir))), SpellSig),
    assertion(SpellSig = perceive(test_spell(query))).

%% Test Case 6: Complex spell executes

test(complex_spell_executes) :-
    user:magic_cast(perceive(test_spell(query(system, root_dir))), Result),
    assertion(Result = ok(value(_))).

test(complex_spell_handles_not_found) :-
    user:magic_cast(perceive(test_spell(query(nonexistent_entity, some_type))), Result),
    assertion(Result = error(not_found)).

%% Test Case 7: Grimoire spells - executable_program with echo

test(executable_program_echo) :-
    user:magic_cast(conjure(executable_program(program(echo), args(["hello", "world"]))), Result),
    Result = ok(result(stdout(Stdout), stderr(_Stderr))),
    assertion(sub_string(Stdout, _, _, _, "hello world")).

%% Test Case 8: Grimoire spells - executable_program with cat

test(executable_program_cat, [setup(setup_test_file), cleanup(cleanup_test_file)]) :-
    user:magic_cast(conjure(executable_program(program(cat), args(["/tmp/grimoire_test_file.txt"]))), Result),
    Result = ok(result(stdout(Stdout), stderr(_Stderr))),
    assertion(sub_string(Stdout, _, _, _, "test content")).

%% Test Case 9: Grimoire spells - shell command

test(shell_echo) :-
    user:magic_cast(conjure(shell(args(["echo", "shell test"]))), Result),
    Result = ok(result(stdout(Stdout), stderr(_Stderr))),
    assertion(sub_string(Stdout, _, _, _, "shell test")).

%% Test Case 10: Grimoire spells - perceive entities

test(perceive_entities) :-
    user:magic_cast(perceive(entities), Result),
    Result = ok(entity_list(Entities)),
    assertion(is_list(Entities)),
    assertion(member(system, Entities)).

:- end_tests(grimoire).

%% Setup/cleanup for test file
setup_test_file :-
    open('/tmp/grimoire_test_file.txt', write, Stream),
    write(Stream, 'test content'),
    close(Stream).

cleanup_test_file :-
    (exists_file('/tmp/grimoire_test_file.txt') ->
        delete_file('/tmp/grimoire_test_file.txt')
    ; true).
