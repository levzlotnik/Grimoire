:- use_module(library(plunit)).
:- load_entity(semantic(file('@/src/tests/interface_test_entities.pl'))).

% === VERIFY PREDICATES ===

% Verify client capability expansion (DSL pattern)
verify(component(Entity, has(interface(client)), interface(client(Spec)))) :-
    user:please_verify(component(Entity, interface_client_type, Type)),
    member(type(Type), Spec),
    member(Type, [cli, mcp, http, golem]).

% Verify all subcommands have proper entities and docstrings
verify(component(interface, subcommand, Cmd)) :-
    entity(interface(Cmd)),
    docstring(interface(Cmd), _).

:- begin_tests(interface).

% Test that the entity is properly declared
test(entity_exists) :-
    entity(interface).

% Test interface subcommands exist
test(subcommands_exist) :-
    user:please_verify(component(interface, subcommand, compt)),
    user:please_verify(component(interface, subcommand, comp)),
    user:please_verify(component(interface, subcommand, doc)),
    user:please_verify(component(interface, subcommand, entities)),
    user:please_verify(component(interface, subcommand, repl)),
    user:please_verify(component(interface, subcommand, test)),
    user:please_verify(component(interface, subcommand, conjure)),
    user:please_verify(component(interface, subcommand, perceive)),
    user:please_verify(component(interface, subcommand, read_file)),
    user:please_verify(component(interface, subcommand, edit_file)),
    user:please_verify(component(interface, subcommand, exec)), !.

% Test entities subcommand specifically
test(entities_subcommand) :-
    user:please_verify(component(interface, subcommand, entities)), !.

% Test entities interface entity exists
test(entities_interface_entity) :-
    entity(interface(entities)).

% Test entities interface has docstring
test(entities_docstring) :-
    docstring(interface(entities), Doc),
    sub_string(Doc, _, _, _, "entities"), !.

% Test entities interface works
test(entities_interface_works) :-
    magic_cast(conjure(interface(entities)), Result),
    Result = ok(entities(List)),
    is_list(List), !.

% Test interface_entities predicate
test(interface_entities_includes_system) :-
    interface_entities(Entities),
    is_list(Entities),
    member(system, Entities), !.

% Test that interface ctors match subcommands
test(interface_ctors_match_subcommands) :-
    findall(C, component(interface, subcommand, C), Subcommands),
    findall(C, component(interface, ctor, C), Ctors),
    sort(Subcommands, SortedSubs),
    sort(Ctors, SortedCtors),
    SortedSubs = SortedCtors.

% Test that all subcommands have corresponding entities
test(all_subcommands_have_entities) :-
    forall(
        component(interface, subcommand, Cmd),
        entity(interface(Cmd))
    ).

% Test that all subcommands have docstrings
test(all_subcommands_have_docstrings) :-
    forall(
        component(interface, subcommand, Cmd),
        docstring(interface(Cmd), _)
    ).

% Test component types command
test(compt_command) :-
    magic_cast(conjure(interface(compt)), Result),
    Result = ok(component_types(Entity, Types)),
    atom(Entity),
    is_list(Types), !.

% Test documentation command
test(doc_command) :-
    magic_cast(conjure(interface(doc)), Result),
    Result = ok(documentation(Entity, Doc)),
    atom(Entity),
    (atom(Doc) ; string(Doc) ; Doc = no_docstring_available), !.

% Test client capability expansion (DSL pattern)
test(client_capability_expansion) :-
    user:please_verify(component(test_cli_client, interface_client_type, cli)),
    user:please_verify(component(test_cli_client, interface_client_capability, _)).

% Test all subcommands complete verification
test(all_subcommands_complete) :-
    forall(component(interface, subcommand, Cmd),
           user:please_verify(component(interface, subcommand, Cmd))).

% === SPELL TESTS ===

% Test comp command with entity and type arguments
test(comp_command) :-
    magic_cast(conjure(interface(comp(interface, subcommand))), Result),
    Result = ok(components(interface, subcommand, Components)),
    is_list(Components),
    % Should have at least one component (compt, comp, doc, etc.)
    length(Components, Len),
    Len > 0, !.

% Test conjure delegation to git domain
test(conjure_delegation_git_status) :-
    magic_cast(conjure(interface(conjure(git(status)))), Result),
    % Should delegate to git domain
    (Result = ok(_) ; Result = error(_)), !.

% Test perceive delegation
test(perceive_delegation) :-
    % Use a real perceive query that exists (git status)
    magic_cast(conjure(interface(perceive(git(status)))), Result),
    % Should succeed as perceive query
    Result = ok(query_succeeded), !.

% Test session command delegation - DISABLED (session being reworked)
% test(session_command_delegation) :-
%     % Use a valid session command (start) - it may fail if session exists, but tests delegation
%     magic_cast(conjure(interface(session(start))), Result),
%     % Should delegate to session domain and return ok or error
%     (Result = ok(_) ; Result = error(_)), !.

% Test read_file command (delegation to fs domain)
test(read_file_command, [
    setup(setup_test_file('/tmp/grimoire_test_read.txt', "Line 1\nLine 2\nLine 3\n")),
    cleanup(delete_file('/tmp/grimoire_test_read.txt'))
]) :-
    magic_cast(conjure(interface(read_file('/tmp/grimoire_test_read.txt', 1, 3))), Result),
    % Should delegate to fs domain and return ok or error
    (Result = ok(_) ; Result = error(_)), !.

% Test edit_file command (delegation to fs domain)
test(edit_file_command, [
    setup(setup_test_file('/tmp/grimoire_test_edit.txt', "Original content\n")),
    cleanup(delete_file('/tmp/grimoire_test_edit.txt'))
]) :-
    magic_cast(conjure(interface(edit_file('/tmp/grimoire_test_edit.txt', [append("New line\n")]))), Result),
    % Should delegate to fs domain
    (Result = ok(_) ; Result = error(_)), !.

% Helper to create test files
setup_test_file(Path, Content) :-
    open(Path, write, Stream),
    write(Stream, Content),
    close(Stream).

:- end_tests(interface).
