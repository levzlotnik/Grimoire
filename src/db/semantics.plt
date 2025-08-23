% Database entity tests - reverse proxy predicating system
:- ensure_loaded('semantics.pl').

:- begin_tests(db_entity).

:- dynamic(test_db_path/1).

% Test database registration - static fact that depends on file existence
registered_db(database(test_discovery), data(file(TestDbPath)), schema(file('test_schema.sql'))) :-
    test_db_path(TestDbPath),
    exists_file(TestDbPath).

% Test setup: create database file
setup_test_db :-
    cleanup_test_db,
    % Use temporary directory for tests
    (getenv('TMPDIR', TmpDir) -> true ; TmpDir = '/tmp'),
    atomic_list_concat([TmpDir, '/test_discovery.db'], TestDbPath),
    retractall(test_db_path(_)),
    assertz(test_db_path(TestDbPath)),
    sqlite3_exec(TestDbPath, 'CREATE TABLE test_table (id INTEGER PRIMARY KEY, name TEXT);').

% Test database entity exists
test(db_entity_exists) :-
    entity(db), !.

% Test database entity creation
test(database_entity_creation) :-
    setup_test_db,
    entity(database(test_discovery)).

% Test database file component
test(database_file_component) :-
    setup_test_db,
    test_db_path(TestDbPath),
    component(database(test_discovery), data, data(file(TestDbPath))).

% Test table discovery
test(table_discovery) :-
    setup_test_db,
    component(database(test_discovery), table, table(test_table)).

% Test column discovery
test(column_discovery) :-
    setup_test_db,
    component(database(test_discovery), column, column(test_table, id, _)), !.

% Test column discovery - name column
test(column_discovery_name) :-
    setup_test_db,
    component(database(test_discovery), column, column(test_table, name, _)).

% Test SQLite3 functions exist
test(sqlite3_functions_exist) :-
    current_predicate(sqlite3_exec/2),
    current_predicate(sqlite3_query/3),
    current_predicate(get_database_tables/2),
    current_predicate(get_table_columns/3).

% Test docstring exists
test(db_docstring_exists) :-
    docstring(db, _).

% Cleanup
cleanup_test_db :-
    (test_db_path(TestDbPath) -> 
        (exists_file(TestDbPath) -> delete_file(TestDbPath) ; true)
    ; true),
    retractall(test_db_path(_)).

:- end_tests(db_entity).
