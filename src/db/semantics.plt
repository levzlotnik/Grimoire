% Database entity tests - reverse proxy predicating system
:- grimoire_ensure_loaded('@/src/db/semantics.pl').

:- dynamic(test_db_path/1).
:- dynamic(registered_db/3).

% Test setup: create database file and register it
setup_test_db :-
    cleanup_test_db,
    % Use /tmp directory for test database files
    TestDbPath = '/tmp/test_discovery.db',
    retractall(test_db_path(_)),
    assertz(test_db_path(TestDbPath)),
    % Create the database with a test table
    sqlite3_exec(TestDbPath, 'CREATE TABLE IF NOT EXISTS test_table (id INTEGER PRIMARY KEY, name TEXT);'),
    % Register the database for testing
    retractall(registered_db(database(test_discovery), _, _)),
    assertz(registered_db(database(test_discovery), data(file(TestDbPath)), schema(file('/tmp/test_schema.sql')))).

% Cleanup
cleanup_test_db :-
    (test_db_path(TestDbPath) ->
        (exists_file(TestDbPath) -> delete_file(TestDbPath) ; true)
    ; true),
    retractall(test_db_path(_)),
    retractall(registered_db(database(test_discovery), _, _)).

:- begin_tests(db_entity).

% Test database entity exists
test(db_entity_exists) :-
    entity(db), !.

% Test database entity creation
test(database_entity_creation, [cleanup(cleanup_test_db)]) :-
    setup_test_db,
    % Verify the registration worked
    registered_db(database(test_discovery), data(file(_)), schema(file(_))),
    entity(database(test_discovery)), !.

% Test database file component
test(database_file_component, [cleanup(cleanup_test_db)]) :-
    setup_test_db,
    test_db_path(TestDbPath),
    component(database(test_discovery), data, data(file(TestDbPath))), !.

% Test table discovery
test(table_discovery, [cleanup(cleanup_test_db)]) :-
    setup_test_db,
    component(database(test_discovery), table, table(test_table)), !.

% Test column discovery
test(column_discovery, [cleanup(cleanup_test_db)]) :-
    setup_test_db,
    component(database(test_discovery), column, column(test_table, id, _)), !.

% Test column discovery - name column
test(column_discovery_name, [cleanup(cleanup_test_db)]) :-
    setup_test_db,
    component(database(test_discovery), column, column(test_table, name, _)), !.

% Test SQLite3 functions exist
test(sqlite3_functions_exist) :-
    current_predicate(sqlite3_exec/2),
    current_predicate(sqlite3_query/3),
    current_predicate(get_database_tables/2),
    current_predicate(get_table_columns/3).

% Test docstring exists
test(db_docstring_exists) :-
    docstring(db, _).

:- end_tests(db_entity).
