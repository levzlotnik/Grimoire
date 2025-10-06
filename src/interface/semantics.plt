:- use_module(library(plunit)).
:- load_entity(semantic(file('@/src/tests/interface_test_entities.pl'))).

verify(component(Entity, has(interface(client)), interface(client(Spec)))) :-
    please_verify(component(Entity, interface_client_type, Type)),
    member(type(Type), Spec),
    member(Type, [cli, mcp, http, golem]).

verify(component(interface, subcommand, Cmd)) :-
    entity(interface(Cmd)),
    docstring(interface(Cmd), _).

:- begin_tests(interface).

test(interface_entity_exists) :- entity(interface).

test(subcommands_exist) :-
    component(interface, subcommand, compt),
    component(interface, subcommand, entities).

test(entities_subcommand) :-
    cast(conjure(interface(entities)), Result),
    format('~nDEBUG: cast result = ~w~n', [Result]),
    Result = ok(entities(List)),
    format('~nDEBUG: List = ~w~n', [List]),
    is_list(List).

test(client_capability_expansion) :-
    please_verify(component(test_cli_client, interface_client_type, cli)),
    component(test_cli_client, interface_client_capability, _).

test(all_subcommands_complete) :-
    forall(component(interface, subcommand, Cmd),
           please_verify(component(interface, subcommand, Cmd))).

:- end_tests(interface).
