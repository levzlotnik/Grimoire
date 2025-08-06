% Interactive REPL frontend for Grimoire
% Loads grimoire.pl and provides user interface

:- ensure_loaded('src/prolog/grimoire.pl').

% Show welcome banner and helpful info on startup
:- initialization(grimoire_startup).

grimoire_startup :-
    grimoire_banner,
    grimoire_repl.

grimoire_banner :-
    writeln(''),
    writeln('╔═══════════════════════════════════════════════════════════╗'),
    writeln('║                      🔮 GRIMOIRE 🔮                       ║'),
    writeln('║             Knowledge-Based Operating System              ║'),
    writeln('║                                                           ║'),
    writeln('║    "In the pursuit of knowledge, even the smallest        ║'),
    writeln('║     spell can unlock great understanding."                ║'),
    writeln('╚═══════════════════════════════════════════════════════════╝'),
    writeln(''),
    % Show current status
    findall(E, entity(E), Entities),
    length(Entities, NumEntities),
    findall(S, component(system, subsystem, S), Subsystems),
    length(Subsystems, NumSubsystems),
    format('🔮 System ready: ~w entities, ~w subsystems loaded~n', [NumEntities, NumSubsystems]),
    writeln(''),
    writeln('Grimoire Commands:'),
    writeln('  help.                    - Show this help'),
    writeln('  status.                  - Show system status'),
    writeln('  entities.                - List all entities'),
    writeln('  domains.                 - List loaded domains'),
    writeln('  test.                    - Run all tests'),
    writeln('  discover(Project).       - Discover project artifacts'),
    writeln('  quit.                    - Exit grimoire'),
    writeln(''),
    writeln('Type quit. to exit or use Ctrl+D'),
    writeln('').

% Interactive REPL
grimoire_repl :-
    repeat,
    write('grimoire> '),
    catch(read_term(Term, []), _Error, (writeln(''), writeln('Farewell, knowledge seeker! 🌟'), halt)),
    (var(Term) -> (writeln(''), writeln('Farewell, knowledge seeker! 🌟'), halt) ; true),
    (Term == end_of_file -> (writeln(''), writeln('Farewell, knowledge seeker! 🌟'), halt) ; true),
    (grimoire_command(Term) -> true ; handle_prolog_query(Term)),
    (Term == quit -> (!, writeln('Farewell, knowledge seeker! 🌟'), halt) ; fail).

% Built-in grimoire commands
grimoire_command(help) :-
    !,
    writeln(''),
    writeln('Grimoire Commands:'),
    writeln('  help.                    - Show this help'),
    writeln('  status.                  - Show system status'),
    writeln('  entities.                - List all entities'),
    writeln('  domains.                 - List loaded domains'),
    writeln('  test.                    - Run all tests'),
    writeln('  test(Domain).            - Run tests for specific domain'),
    writeln('  discover(Project).       - Discover project artifacts'),
    writeln('  load_template(Type, Name). - Load template as project'),
    writeln('  quit.                    - Exit grimoire'),
    writeln(''),
    writeln('You can also use regular Prolog queries.'),
    writeln('').

grimoire_command(status) :-
    !,
    writeln(''),
    writeln('🔮 Grimoire System Status:'),
    writeln(''),

    % Count entities
    findall(E, entity(E), Entities),
    length(Entities, NumEntities),
    format('  Entities loaded: ~w~n', [NumEntities]),

    % List subsystems
    findall(S, component(system, subsystem, S), Subsystems),
    format('  Subsystems: ~w~n', [Subsystems]),

    % Count mounted semantics
    findall(P, mounted_semantic(P, _), MountedPaths),
    length(MountedPaths, NumMounted),
    format('  Mounted semantics: ~w~n', [NumMounted]),

    writeln('').

grimoire_command(entities) :-
    !,
    writeln(''),
    writeln('📚 Known Entities:'),
    findall(E, entity(E), Entities),
    sort(Entities, SortedEntities),
    forall(member(E, SortedEntities),
           format('  - ~w~n', [E])),
    writeln('').

grimoire_command(domains) :-
    !,
    writeln(''),
    writeln('🏛️  Loaded Domains:'),
    findall(D, component(system, subsystem, D), Domains),
    forall(member(D, Domains), (
        format('  - ~w', [D]),
        (component(D, source, Source) ->
            format(' (~w)', [Source])
        ; true),
        nl
    )),
    writeln('').

grimoire_command(test) :-
    !,
    writeln(''),
    writeln('🧪 Running all Grimoire tests...'),
    writeln(''),
    catch(
        (consult('src/prolog/tests/run_tests.pl'), run_tests),
        Error,
        (format('Error running tests: ~w~n', [Error]))
    ).

grimoire_command(test(Domain)) :-
    !,
    format('🧪 Running tests for domain: ~w~n', [Domain]),
    % Try to find and run domain-specific tests
    atomic_list_concat(['src/prolog/', Domain, '.plt'], TestFile),
    (exists_file(TestFile) ->
        catch(
            (consult(TestFile), run_tests),
            Error,
            (format('Error running ~w tests: ~w~n', [Domain, Error]))
        )
    ;
        format('No test file found for domain: ~w~n', [Domain])
    ).

grimoire_command(discover(Project)) :-
    !,
    format('🔍 Discovering artifacts for project: ~w~n', [Project]),
    catch(
        discover_project_artifacts(Project),
        Error,
        (format('Error during discovery: ~w~n', [Error]))
    ).

grimoire_command(load_template(Type, Name)) :-
    !,
    format('📝 Loading ~w template as project ~w~n', [Type, Name]),
    atomic_list_concat(['src/prolog/nix/templates/', Type, '/semantics.pl'], TemplatePath),
    (exists_file(TemplatePath) ->
        catch(
            load_entity(Name, semantic(file(TemplatePath))),
            Error,
            (format('Error loading template: ~w~n', [Error]))
        )
    ;
        format('Template not found: ~w~n', [TemplatePath])
    ).

% Handle regular Prolog queries
handle_prolog_query(Term) :-
    catch(
        (call(Term) ->
            writeln('true.')
        ;
            writeln('false.')
        ),
        Error,
        (format('Error: ~w~n', [Error]))
    ).
