% Database entity tests - reverse proxy predicating system

% Note: db/semantics.pl already loaded by grimoire.pl
% ECS predicates (entity/1, component/3, etc.) are multifile and globally available

:- use_module(library(plunit)).

:- dynamic(test_db_path/1).
:- dynamic(registered_db/3).

% === VERIFICATION RULES ===
% Domain-specific verify/1 implementations using please_verify/1

% Verify SQLite database DSL pattern
verify(component(Entity, has(db(sqlite)), db(sqlite(database_id(_Id), file(DbPath), schema(SchemaPath))))) :-
    % Verify expanded components exist and are valid
    please_verify(component(Entity, db_sqlite_file, DbPath)),
    please_verify(component(Entity, db_sqlite_schema, SchemaPath)),
    please_verify(component(Entity, db_sqlite_valid, true)).

% Verify database file component
verify(component(_Entity, db_sqlite_file, DbPath)) :-
    % Validate the file exists
    (exists_file(DbPath) ->
        validate_database(DbPath)
    ;
        throw(verification_error(db, missing_database_file(DbPath)))
    ).

% Verify database tables component
verify(component(Entity, db_sqlite_tables, Tables)) :-
    component(Entity, db_sqlite_file, DbPath),
    get_database_tables(DbPath, ActualTables),
    (Tables = ActualTables ->
        true
    ;
        throw(verification_error(db, table_mismatch(expected(Tables), actual(ActualTables))))
    ).

% Verify table DSL pattern
verify(component(Entity, has(db(table)), db(table(TableName)))) :-
    component(Entity, db_sqlite_file, DbPath),
    get_database_tables(DbPath, TableStrings),
    (   (atom(TableName) -> atom_string(TableName, TableString) ; TableString = TableName),
        member(TableString, TableStrings)
    ->  true
    ;   throw(verification_error(db, table_not_found(TableName)))
    ).

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

:- begin_tests(db).

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
    user:please_verify(component(database(test_discovery), data, data(file(TestDbPath)))), !.

% Test table discovery
test(table_discovery, [cleanup(cleanup_test_db)]) :-
    setup_test_db,
    user:please_verify(component(database(test_discovery), table, table(test_table))), !.

% Test column discovery
test(column_discovery, [cleanup(cleanup_test_db)]) :-
    setup_test_db,
    user:please_verify(component(database(test_discovery), column, column(test_table, id, _))), !.

% Test column discovery - name column
test(column_discovery_name, [cleanup(cleanup_test_db)]) :-
    setup_test_db,
    user:please_verify(component(database(test_discovery), column, column(test_table, name, _))), !.

% Test SQLite3 functions exist
test(sqlite3_functions_exist) :-
    current_predicate(sqlite3_exec/2),
    current_predicate(sqlite3_query/3),
    current_predicate(get_database_tables/2),
    current_predicate(get_table_columns/3).

% Test docstring exists
test(db_docstring_exists) :-
    docstring(db, _).

% === VERIFICATION TESTS ===

% Test DSL pattern verification with please_verify
test(verify_dsl_pattern, [cleanup(cleanup_verify_dsl)]) :-
    % Setup: assertz a DSL pattern component
    TestDbPath = '/tmp/test_verify_dsl.db',
    TestSchemaPath = '/tmp/test_verify_schema.sql',
    % Clean up any existing files first
    (exists_file(TestDbPath) -> delete_file(TestDbPath) ; true),
    (exists_file(TestSchemaPath) -> delete_file(TestSchemaPath) ; true),
    % Create schema file
    open(TestSchemaPath, write, Stream),
    write(Stream, 'CREATE TABLE test_table (id INTEGER PRIMARY KEY);'),
    close(Stream),
    % Create database
    sqlite3_init_db(TestDbPath, TestSchemaPath),
    % assertz the DSL component
    user:assertz(component(test_verify_entity, has(db(sqlite)),
        db(sqlite(database_id(test_verify_db), file(TestDbPath), schema(TestSchemaPath))))),
    % Verify with please_verify
    user:please_verify(component(test_verify_entity, has(db(sqlite)), _)),
    % Cleanup
    forall(
        clause(user:component(test_verify_entity, C, V), true),
        retract(user:component(test_verify_entity, C, V))
    ).

cleanup_verify_dsl :-
    (exists_file('/tmp/test_verify_dsl.db') -> delete_file('/tmp/test_verify_dsl.db') ; true),
    (exists_file('/tmp/test_verify_schema.sql') -> delete_file('/tmp/test_verify_schema.sql') ; true),
    forall(
        clause(user:component(test_verify_entity, C, V), true),
        retract(user:component(test_verify_entity, C, V))
    ).

% Test expanded components with please_verify
test(verify_expanded_components, [cleanup(cleanup_verify_expanded)]) :-
    TestDbPath = '/tmp/test_verify_expanded.db',
    TestSchemaPath = '/tmp/test_verify_expanded_schema.sql',
    % Clean up any existing files first
    (exists_file(TestDbPath) -> delete_file(TestDbPath) ; true),
    (exists_file(TestSchemaPath) -> delete_file(TestSchemaPath) ; true),
    % Create schema and database
    open(TestSchemaPath, write, Stream),
    write(Stream, 'CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT);'),
    close(Stream),
    sqlite3_init_db(TestDbPath, TestSchemaPath),
    % assertz DSL component
    user:assertz(component(test_expanded_entity, has(db(sqlite)),
        db(sqlite(database_id(test_expanded_db), file(TestDbPath), schema(TestSchemaPath))))),
    % Verify expanded components
    user:please_verify(component(test_expanded_entity, db_sqlite_file, TestDbPath)),
    user:please_verify(component(test_expanded_entity, db_sqlite_schema, TestSchemaPath)),
    user:please_verify(component(test_expanded_entity, db_sqlite_valid, true)),
    % Cleanup
    forall(
        clause(user:component(test_expanded_entity, C, V), true),
        retract(user:component(test_expanded_entity, C, V))
    ), !.

cleanup_verify_expanded :-
    (exists_file('/tmp/test_verify_expanded.db') -> delete_file('/tmp/test_verify_expanded.db') ; true),
    (exists_file('/tmp/test_verify_expanded_schema.sql') -> delete_file('/tmp/test_verify_expanded_schema.sql') ; true),
    forall(
        clause(user:component(test_expanded_entity, C, V), true),
        retract(user:component(test_expanded_entity, C, V))
    ).

% Test verify table pattern
test(verify_table_pattern, [cleanup(cleanup_verify_table)]) :-
    TestDbPath = '/tmp/test_verify_table.db',
    TestSchemaPath = '/tmp/test_verify_table_schema.sql',
    % Clean up any existing files first
    (exists_file(TestDbPath) -> delete_file(TestDbPath) ; true),
    (exists_file(TestSchemaPath) -> delete_file(TestSchemaPath) ; true),
    % Create database with table
    open(TestSchemaPath, write, Stream),
    write(Stream, 'CREATE TABLE products (id INTEGER PRIMARY KEY);'),
    close(Stream),
    sqlite3_init_db(TestDbPath, TestSchemaPath),
    % assertz components
    user:assertz(component(test_table_entity, db_sqlite_file, TestDbPath)),
    user:assertz(component(test_table_entity, has(db(table)), db(table(products)))),
    % Verify table exists
    user:please_verify(component(test_table_entity, has(db(table)), db(table(products)))),
    % Cleanup
    forall(
        clause(user:component(test_table_entity, C, V), true),
        retract(user:component(test_table_entity, C, V))
    ), !.

cleanup_verify_table :-
    (exists_file('/tmp/test_verify_table.db') -> delete_file('/tmp/test_verify_table.db') ; true),
    (exists_file('/tmp/test_verify_table_schema.sql') -> delete_file('/tmp/test_verify_table_schema.sql') ; true),
    forall(
        clause(user:component(test_table_entity, C, V), true),
        retract(user:component(test_table_entity, C, V))
    ).

% Test verify missing database file throws error
test(verify_missing_db_file, [
    cleanup(cleanup_verify_missing),
    throws(verification_error(db, missing_database_file(_)))
]) :-
    % assertz component pointing to non-existent database
    user:assertz(component(test_missing_entity, db_sqlite_file, '/nonexistent/database.db')),
    user:please_verify(component(test_missing_entity, db_sqlite_file, '/nonexistent/database.db')).

cleanup_verify_missing :-
    forall(
        clause(user:component(test_missing_entity, C, V), true),
        retract(user:component(test_missing_entity, C, V))
    ).

% Test assertz→please_verify→retractall pattern with in-memory SQLite
test(assertz_verify_retractall_pattern, [cleanup(cleanup_assertz_pattern)]) :-
    % Use in-memory database for fast test
    _TestDbPath = ':memory:',
    TestSchemaPath = '/tmp/test_assertz_schema.sql',
    TempDbPath = '/tmp/test_assertz_pattern.db',
    % Clean up any existing files first
    (exists_file(TempDbPath) -> delete_file(TempDbPath) ; true),
    (exists_file(TestSchemaPath) -> delete_file(TestSchemaPath) ; true),
    % Create schema file
    open(TestSchemaPath, write, Stream),
    write(Stream, 'CREATE TABLE temp_table (id INTEGER PRIMARY KEY);'),
    close(Stream),
    % Note: in-memory databases can't use file-based schema loading
    % So we'll use a temporary file instead
    sqlite3_init_db(TempDbPath, TestSchemaPath),
    % assertz pattern
    user:assertz(component(test_assertz_entity, has(db(sqlite)),
        db(sqlite(database_id(test_mem), file(TempDbPath), schema(TestSchemaPath))))),
    % Verify it
    user:please_verify(component(test_assertz_entity, has(db(sqlite)), _)),
    % retractall
    forall(
        clause(user:component(test_assertz_entity, C, V), true),
        retract(user:component(test_assertz_entity, C, V))
    ), !.

cleanup_assertz_pattern :-
    (exists_file('/tmp/test_assertz_pattern.db') -> delete_file('/tmp/test_assertz_pattern.db') ; true),
    (exists_file('/tmp/test_assertz_schema.sql') -> delete_file('/tmp/test_assertz_schema.sql') ; true),
    forall(
        clause(user:component(test_assertz_entity, C, V), true),
        retract(user:component(test_assertz_entity, C, V))
    ).

:- end_tests(db).
