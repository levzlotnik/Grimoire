% Tests for interface semantics
:- begin_tests(interface).

% Test that the entity is properly declared
test(entity_exists) :-
    entity(interface).

% Test interface subcommands exist
test(subcommands_exist) :-
    component(interface, subcommand, compt),
    component(interface, subcommand, comp),
    component(interface, subcommand, doc),
    component(interface, subcommand, entities),
    component(interface, subcommand, repl),
    component(interface, subcommand, status),
    component(interface, subcommand, test),
    component(interface, subcommand, session),
    component(interface, subcommand, conjure),
    component(interface, subcommand, perceive),
    component(interface, subcommand, load), !.

% Test entities subcommand specifically
test(entities_subcommand) :-
    component(interface, subcommand, entities), !.

% Test entities interface entity exists
test(entities_interface_entity) :-
    entity(interface(entities)).

% Test entities interface has docstring
test(entities_docstring) :-
    docstring(interface(entities), Doc),
    sub_string(Doc, _, _, _, "entities"), !.

% Test entities interface works
test(entities_interface_works) :-
    cast(conjure(interface(entities)), Result),
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
    cast(conjure(interface(compt)), Result),
    Result = ok(component_types(Entity, Types)),
    atom(Entity),
    is_list(Types), !.

% Test documentation command
test(doc_command) :-
    cast(conjure(interface(doc)), Result),
    Result = ok(documentation(Entity, Doc)),
    atom(Entity),
    (atom(Doc) ; string(Doc) ; Doc = no_docstring_available), !.

% Test status command
test(status_command) :-
    cast(conjure(interface(status)), Result),
    Result = ok(session_status(status_info(_, _, _))), !.

:- end_tests(interface).