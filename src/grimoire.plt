:- use_module(library(plunit)).

%% ============================================================================
%% Test Entity with Skills (OUTSIDE test block)
%% ============================================================================

entity(test_skill_entity).
component(test_skill_entity, test_data, [foo, bar, baz]).

% Derive test skills from test_data
component(E, test_data, Data)
    ==> (component(E, skill(test(item(Item))), SpellTerm) :-
            member(Item, Data),
            format(string(Arg), 'test_~w', [Item]),
            SpellTerm = conjure(test_spell(simple(Arg)))).

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
        (string(Arg)
        -> Result = ok(result(Arg))
        ; Result = error(test_error(invalid_arg_type(Arg))))
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

test(cast_impl_guard_forbids_direct_call) :-
    format('About to call cast_impl~n'),
    (user:in_magic_cast ->
        format('WARNING: in_magic_cast IS asserted before test!~n')
    ;
        format('in_magic_cast NOT asserted (correct)~n')
    ),
    catch(
        (
            format('Calling cast_impl...~n'),
            cast_impl(conjure(test_spell(simple(foo))), Result),
            format('cast_impl succeeded with Result: ~w~n', [Result])
        ),
        Error,
        format('Caught error: ~w~n', [Error])
    ),
    format('After catch, Error = ~w~n', [Error]),
    assertion(Error = error(direct_cast_forbidden(_), _)).

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

%% Test Case 11: Meta-introspection spells - prove_it

test(prove_it_derived_component) :-
    % Test with a simple component to verify prove_it works
    % First find the value so it's grounded
    user:component(system, root_dir, RootDir),
    user:magic_cast(perceive(prove_it(component(system, root_dir, RootDir))), Result),
    assertion(Result = ok(qed(
        component(system, root_dir, RootDir),
        generated_by(_Generation),
        _Verification
    ))).

%% Test Case 12: Meta-introspection spells - sauce_me

test(sauce_me_perceive_spell) :-
    % Test with perceive(entities)
    user:magic_cast(perceive(sauce_me(spell(perceive(entities)))), Result),
    Result = ok(magic_sauce(
        spell(perceive(entities)),
        registered_at(source_location(File, _Line)),
        implementation(ImplText),
        input_format(_Input),
        output_format(_Output),
        docstring(Doc),
        options(_Options)
    )),
    % Verify we got the source file (grimoire.pl)
    assertion(atom(File)),
    assertion(atom_concat(_, 'grimoire.pl', File)),
    % Verify we got documentation (stored as string)
    assertion(string(Doc)),
    assertion(Doc \= ''),
    % Verify we got implementation (stored as string)
    assertion(string(ImplText)).

test(sauce_me_conjure_spell) :-
    % Test with conjure(it_is_what_it_is)
    user:magic_cast(perceive(sauce_me(spell(conjure(it_is_what_it_is)))), Result),
    Result = ok(magic_sauce(
        spell(conjure(it_is_what_it_is)),
        registered_at(source_location(File, _Line)),
        implementation(ImplText),
        input_format(_Input),
        output_format(_Output),
        docstring(Doc),
        options(_Options)
    )),
    % Verify source location (grimoire.pl)
    assertion(atom(File)),
    assertion(atom_concat(_, 'grimoire.pl', File)),
    % Verify docstring contains "It is what it is"
    assertion(string(Doc)),
    assertion(sub_string(Doc, _, _, _, 'It is what it is')),
    % Verify implementation (stored as string)
    assertion(string(ImplText)),
    assertion(sub_string(ImplText, _, _, _, 'implementation')).

%% Skill System Tests

test(skills_perception_empty) :-
    % Test entity with no skills
    user:magic_cast(perceive(skills(entity(system))), Result),
    Result = ok(skills_list(Skills)),
    assertion(is_list(Skills)).

test(skills_derivation_from_component) :-
    % Test that skills are derived from test_skill_entity
    user:magic_cast(perceive(skills(entity(test_skill_entity))), Result),
    Result = ok(skills_list(Skills)),
    assertion(is_list(Skills)),
    assertion(member(skill(test(item(foo)), _), Skills)),
    assertion(member(skill(test(item(bar)), _), Skills)),
    assertion(member(skill(test(item(baz)), _), Skills)),
    assertion(length(Skills, 3)).

test(invoke_skill_executes_spell) :-
    % Invoke a derived skill and verify it executes the spell
    user:magic_cast(conjure(invoke_skill(entity(test_skill_entity), skill(test(item(foo))))), Result),
    assertion(Result == ok(skill_result(ok(result("test_foo"))))).

test(invoke_skill_not_found) :-
    % Try to invoke nonexistent skill
    user:magic_cast(conjure(invoke_skill(entity(system), skill(nonexistent(skill)))), Result),
    assertion(Result == error(skill_error(skill_not_found(system, nonexistent(skill))))).

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
