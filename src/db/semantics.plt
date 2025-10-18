:- use_module(library(plunit)).

% Load domain semantics first (includes sqlite3.pl)
:- grimoire_ensure_loaded('@/src/db/semantics.pl').

% Load test entities from file-based knowledge
:- load_entity(semantic(file('@/src/tests/db_test_entities.pl'))).
:- load_entity(semantic(file('@/src/db/tests/semantics.pl'))).

:- dynamic(registered_db/3).

%% ============================================================================
%% Test Suite
%% ============================================================================

:- begin_tests(db).

%% ============================================================================
%% Test Case 1: Entity and component existence
%% ============================================================================

test(db_entity_exists) :-
    entity(db).

test(database_entity_from_registration) :-
    % test_discovery database loaded from file
    registered_db(database(test_discovery), data(file(_)), schema(file(_))),
    entity(database(test_discovery)).

%% ============================================================================
%% Test Case 2: Component expansion from has(db(sqlite))
%% ============================================================================

test(dsl_expands_to_db_sqlite_id, [
    setup(setup_dsl_test),
    cleanup(cleanup_dsl_test)
]) :-
    user:please_verify(component(test_verify_entity, db_sqlite_id, DbId)),
    assertion(DbId = test_db).

test(dsl_expands_to_db_sqlite_file, [
    setup(setup_dsl_test),
    cleanup(cleanup_dsl_test)
]) :-
    user:please_verify(component(test_verify_entity, db_sqlite_file, File)),
    assertion(File = '/tmp/test_verify_dsl.db').

test(dsl_expands_to_db_sqlite_schema, [
    setup(setup_dsl_test),
    cleanup(cleanup_dsl_test)
]) :-
    user:please_verify(component(test_verify_entity, db_sqlite_schema, Schema)),
    assertion(Schema = '/tmp/test_schema.sql').

%% ============================================================================
%% Test Case 3: Composite verification using please_verify
%% ============================================================================

test(composite_verification_succeeds, [
    setup(setup_dsl_test),
    cleanup(cleanup_dsl_test)
]) :-
    % This verifies all expanded components using please_verify composition
    user:please_verify(component(test_verify_entity, has(db(sqlite)), _)).

%% ============================================================================
%% Test Case 4: Leaf verification failures
%% ============================================================================

test(db_sqlite_file_missing_throws, [
    throws(verification_error(db, file_not_found(_)))
]) :-
    user:please_verify(component(test_missing_entity, db_sqlite_file, '/nonexistent/database.db')).

test(db_sqlite_file_invalid_throws, [
    setup(setup_invalid_db_test),
    cleanup(cleanup_invalid_db_test),
    throws(error(sqlite_error(_), _))
]) :-
    % Create a non-SQLite file
    InvalidPath = '/tmp/test_invalid_db.db',
    open(InvalidPath, write, Stream),
    write(Stream, 'This is not a SQLite database'),
    close(Stream),
    % Try to verify it as a database file - validate_database throws sqlite_error
    user:please_verify(component(test_invalid_entity, db_sqlite_file, InvalidPath)).

test(db_sqlite_schema_missing_throws, [
    throws(verification_error(db, schema_not_found(_)))
]) :-
    user:please_verify(component(test_invalid_schema_entity, db_sqlite_schema, '/nonexistent/schema.sql')).

%% ============================================================================
%% Test Case 5: Registered database components
%% ============================================================================

test(registered_database_data_component) :-
    user:please_verify(component(database(test_discovery), data, data(file(_)))).

test(registered_database_schema_component) :-
    user:please_verify(component(database(test_discovery), schema, schema(file(_)))).

%% ============================================================================
%% Test Case 6: Table and column discovery
%% ============================================================================

test(table_discovery) :-
    % test_discovery database has test_table from schema
    user:please_verify(component(database(test_discovery), table, table(test_table))).

test(column_discovery_id) :-
    % test_table has id column
    user:please_verify(component(database(test_discovery), column, column(test_table, id, _))).

test(column_discovery_name) :-
    % test_table has name column
    user:please_verify(component(database(test_discovery), column, column(test_table, name, _))).

%% ============================================================================
%% Test Case 7: Derived components
%% ============================================================================

test(db_sqlite_valid_component, [
    setup(setup_derived_test),
    cleanup(cleanup_derived_test)
]) :-
    % Derived component query (not using please_verify since it's a derived query, not stored fact)
    component(test_expanded_entity, db_sqlite_valid, Valid),
    assertion(Valid = true).

test(db_sqlite_tables_component, [
    setup(setup_derived_test),
    cleanup(cleanup_derived_test)
]) :-
    % Derived component query (not using please_verify since it's a derived query, not stored fact)
    component(test_expanded_entity, db_sqlite_tables, Tables),
    assertion(is_list(Tables)).

%% ============================================================================
%% Test Case 8: SQLite helper functions
%% ============================================================================

test(sqlite3_functions_exist) :-
    current_predicate(sqlite3_exec/2),
    current_predicate(sqlite3_query/3),
    current_predicate(get_database_tables/2),
    current_predicate(get_table_columns/3).

%% ============================================================================
%% Test Case 9: Spell - conjure(db(create)) with file schema
%% ============================================================================

test(spell_create_with_file_schema, [
    cleanup(cleanup_spell_create_file)
]) :-
    TestDbPath = '/tmp/test_spell_create_file.db',
    TestSchemaPath = '/tmp/test_spell_create_file_schema.sql',

    % Create schema file
    open(TestSchemaPath, write, Stream),
    write(Stream, 'CREATE TABLE test_create (id INTEGER PRIMARY KEY, name TEXT);'),
    close(Stream),

    % Cast the spell
    user:magic_cast(conjure(db(create(test_spell_file_db, TestDbPath, schema(file(TestSchemaPath))))), Result),

    % Verify result structure
    Result = ok(database_created(DbId, DbPath, RegisteredDbFact)),
    assertion(DbId = test_spell_file_db),
    assertion(DbPath = TestDbPath),
    assertion(RegisteredDbFact = registered_db(database(test_spell_file_db), data(file(TestDbPath)), schema(file(TestSchemaPath)))),

    % Verify database file was created
    assertion(exists_file(TestDbPath)),

    % Verify database is valid SQLite
    validate_database(TestDbPath).

%% ============================================================================
%% Test Case 10: Spell - conjure(db(create)) with SQL string
%% ============================================================================

test(spell_create_with_sql_string, [
    cleanup(cleanup_spell_create_sql)
]) :-
    TestDbPath = '/tmp/test_spell_create_sql.db',
    SchemaSQL = 'CREATE TABLE test_sql (id INTEGER PRIMARY KEY, value TEXT);',

    % Cast the spell
    user:magic_cast(conjure(db(create(test_spell_sql_db, TestDbPath, schema(sql(SchemaSQL))))), Result),

    % Verify result structure
    assertion(Result = ok(database_created(_, _, _))),
    Result = ok(database_created(DbId, DbPath, RegisteredDbFact)),
    assertion(DbId = test_spell_sql_db),
    assertion(DbPath = TestDbPath),

    % Extract schema file from registered fact
    RegisteredDbFact = registered_db(database(test_spell_sql_db), data(file(TestDbPath)), schema(file(SchemaFile))),

    % Verify database file was created
    assertion(exists_file(TestDbPath)),

    % Verify schema file was generated
    assertion(exists_file(SchemaFile)),

    % Verify database is valid SQLite
    validate_database(TestDbPath).

%% ============================================================================
%% Test Case 11: Spell - conjure(db(execute))
%% ============================================================================

test(spell_execute, [
    setup(setup_spell_execute_test),
    cleanup(cleanup_spell_execute_test)
]) :-
    % Cast the spell to insert data
    user:magic_cast(conjure(db(execute(database_id(test_execute_db), sql("INSERT INTO test_exec (name) VALUES ('test_value')")))), Result),

    % Verify result
    Result = ok(executed(_)),

    % Verify data was inserted by querying (sqlite3_query returns JSON dicts)
    sqlite3_query('/tmp/test_spell_execute.db', "SELECT name FROM test_exec", QueryResult),
    % QueryResult is a list of dicts with name field
    assertion(is_list(QueryResult)),
    assertion(length(QueryResult, 1)).

%% ============================================================================
%% Test Case 12: Spell - perceive(db(query))
%% ============================================================================

test(spell_query, [
    setup(setup_spell_query_test),
    cleanup(cleanup_spell_query_test)
]) :-
    % Cast the spell
    user:magic_cast(perceive(db(query(database_id(test_query_db), sql("SELECT name FROM test_query")))), Result),

    % Verify result - sqlite3_query returns JSON dicts
    Result = ok(query_results(QueryResults)),
    assertion(is_list(QueryResults)),
    assertion(length(QueryResults, 1)).

%% ============================================================================
%% Test Case 13: Spell - perceive(db(tables))
%% ============================================================================

test(spell_tables, [
    setup(setup_spell_tables_test),
    cleanup(cleanup_spell_tables_test)
]) :-
    % Cast the spell
    user:magic_cast(perceive(db(tables(database_id(test_tables_db)))), Result),

    % Verify result
    Result = ok(tables(TableList)),
    assertion(is_list(TableList)),
    assertion(member("test_tables", TableList)).

%% ============================================================================
%% Test Case 14: Spell error conditions
%% ============================================================================

test(spell_execute_missing_database) :-
    user:magic_cast(conjure(db(execute(database_id(nonexistent_db), sql("INSERT INTO foo VALUES (1)")))), Result),
    assertion(Result = error(db_error(database_not_found(nonexistent_db)))).

test(spell_query_missing_database) :-
    user:magic_cast(perceive(db(query(database_id(nonexistent_db), sql("SELECT 1")))), Result),
    assertion(Result = error(query_error(database_not_found(nonexistent_db)))).

test(spell_tables_missing_database) :-
    user:magic_cast(perceive(db(tables(database_id(nonexistent_db)))), Result),
    assertion(Result = error(tables_error(database_not_found(nonexistent_db)))).

test(spell_create_already_exists, [
    setup(setup_create_exists_test),
    cleanup(cleanup_create_exists_test),
    throws(error(db_error(database_already_exists(_))))
]) :-
    TestDbPath = '/tmp/test_spell_create_exists.db',
    TestSchemaPath = '/tmp/test_spell_create_exists_schema.sql',
    user:magic_cast(conjure(db(create(test_create_exists_db, TestDbPath, schema(file(TestSchemaPath))))), _).

:- end_tests(db).

%% ============================================================================
%% Setup/Cleanup Helpers - FILESYSTEM ONLY
%% ============================================================================

% Test entity for invalid DB test
:- dynamic(entity/1).
:- dynamic(component/3).

% Setup for DSL test
setup_dsl_test :-
    TestDbPath = '/tmp/test_verify_dsl.db',
    TestSchemaPath = '/tmp/test_schema.sql',
    (exists_file(TestDbPath) -> delete_file(TestDbPath) ; true),
    (exists_file(TestSchemaPath) -> delete_file(TestSchemaPath) ; true),
    open(TestSchemaPath, write, Stream),
    write(Stream, 'CREATE TABLE test_table (id INTEGER PRIMARY KEY);'),
    close(Stream),
    sqlite3_init_db(TestDbPath, TestSchemaPath).

cleanup_dsl_test :-
    (exists_file('/tmp/test_verify_dsl.db') -> delete_file('/tmp/test_verify_dsl.db') ; true),
    (exists_file('/tmp/test_schema.sql') -> delete_file('/tmp/test_schema.sql') ; true).

% Setup for invalid DB test
setup_invalid_db_test :-
    assertz(entity(test_invalid_entity)),
    assertz(component(test_invalid_entity, db_sqlite_file, '/tmp/test_invalid_db.db')).

cleanup_invalid_db_test :-
    (exists_file('/tmp/test_invalid_db.db') -> delete_file('/tmp/test_invalid_db.db') ; true),
    retractall(entity(test_invalid_entity)),
    retractall(component(test_invalid_entity, _, _)).

% Setup for invalid schema test
:- assertz(entity(test_invalid_schema_entity)).
:- assertz(component(test_invalid_schema_entity, db_sqlite_schema, '/nonexistent/schema.sql')).

% Setup for derived test
setup_derived_test :-
    TestDbPath = '/tmp/test_verify_expanded.db',
    TestSchemaPath = '/tmp/test_schema_expanded.sql',
    (exists_file(TestDbPath) -> delete_file(TestDbPath) ; true),
    (exists_file(TestSchemaPath) -> delete_file(TestSchemaPath) ; true),
    open(TestSchemaPath, write, Stream),
    write(Stream, 'CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT);'),
    close(Stream),
    sqlite3_init_db(TestDbPath, TestSchemaPath).

cleanup_derived_test :-
    (exists_file('/tmp/test_verify_expanded.db') -> delete_file('/tmp/test_verify_expanded.db') ; true),
    (exists_file('/tmp/test_schema_expanded.sql') -> delete_file('/tmp/test_schema_expanded.sql') ; true).

% Spell cleanup helpers
cleanup_spell_create_file :-
    (exists_file('/tmp/test_spell_create_file.db') -> delete_file('/tmp/test_spell_create_file.db') ; true),
    (exists_file('/tmp/test_spell_create_file_schema.sql') -> delete_file('/tmp/test_spell_create_file_schema.sql') ; true),
    retractall(registered_db(database(test_spell_file_db), _, _)).

cleanup_spell_create_sql :-
    (exists_file('/tmp/test_spell_create_sql.db') -> delete_file('/tmp/test_spell_create_sql.db') ; true),
    (exists_file('/tmp/test_spell_create_sql.schema.sql') -> delete_file('/tmp/test_spell_create_sql.schema.sql') ; true),
    retractall(registered_db(database(test_spell_sql_db), _, _)).

% Setup for execute spell test
setup_spell_execute_test :-
    TestDbPath = '/tmp/test_spell_execute.db',
    TestSchemaPath = '/tmp/test_spell_execute_schema.sql',
    (exists_file(TestDbPath) -> delete_file(TestDbPath) ; true),
    (exists_file(TestSchemaPath) -> delete_file(TestSchemaPath) ; true),
    open(TestSchemaPath, write, Stream),
    write(Stream, 'CREATE TABLE test_exec (id INTEGER PRIMARY KEY, name TEXT);'),
    close(Stream),
    sqlite3_init_db(TestDbPath, TestSchemaPath),
    assertz(registered_db(database(test_execute_db), data(file(TestDbPath)), schema(file(TestSchemaPath)))).

cleanup_spell_execute_test :-
    (exists_file('/tmp/test_spell_execute.db') -> delete_file('/tmp/test_spell_execute.db') ; true),
    (exists_file('/tmp/test_spell_execute_schema.sql') -> delete_file('/tmp/test_spell_execute_schema.sql') ; true),
    retractall(registered_db(database(test_execute_db), _, _)).

% Setup for query spell test
setup_spell_query_test :-
    TestDbPath = '/tmp/test_spell_query.db',
    TestSchemaPath = '/tmp/test_spell_query_schema.sql',
    (exists_file(TestDbPath) -> delete_file(TestDbPath) ; true),
    (exists_file(TestSchemaPath) -> delete_file(TestSchemaPath) ; true),
    open(TestSchemaPath, write, Stream),
    write(Stream, 'CREATE TABLE test_query (id INTEGER PRIMARY KEY, name TEXT);'),
    close(Stream),
    sqlite3_init_db(TestDbPath, TestSchemaPath),
    sqlite3_exec(TestDbPath, "INSERT INTO test_query (name) VALUES ('test_data')"),
    assertz(registered_db(database(test_query_db), data(file(TestDbPath)), schema(file(TestSchemaPath)))).

cleanup_spell_query_test :-
    (exists_file('/tmp/test_spell_query.db') -> delete_file('/tmp/test_spell_query.db') ; true),
    (exists_file('/tmp/test_spell_query_schema.sql') -> delete_file('/tmp/test_spell_query_schema.sql') ; true),
    retractall(registered_db(database(test_query_db), _, _)).

% Setup for tables spell test
setup_spell_tables_test :-
    TestDbPath = '/tmp/test_spell_tables.db',
    TestSchemaPath = '/tmp/test_spell_tables_schema.sql',
    (exists_file(TestDbPath) -> delete_file(TestDbPath) ; true),
    (exists_file(TestSchemaPath) -> delete_file(TestSchemaPath) ; true),
    open(TestSchemaPath, write, Stream),
    write(Stream, 'CREATE TABLE test_tables (id INTEGER PRIMARY KEY);'),
    close(Stream),
    sqlite3_init_db(TestDbPath, TestSchemaPath),
    assertz(registered_db(database(test_tables_db), data(file(TestDbPath)), schema(file(TestSchemaPath)))).

cleanup_spell_tables_test :-
    (exists_file('/tmp/test_spell_tables.db') -> delete_file('/tmp/test_spell_tables.db') ; true),
    (exists_file('/tmp/test_spell_tables_schema.sql') -> delete_file('/tmp/test_spell_tables_schema.sql') ; true),
    retractall(registered_db(database(test_tables_db), _, _)).

% Setup for create exists test
setup_create_exists_test :-
    TestDbPath = '/tmp/test_spell_create_exists.db',
    TestSchemaPath = '/tmp/test_spell_create_exists_schema.sql',
    (exists_file(TestDbPath) -> delete_file(TestDbPath) ; true),
    (exists_file(TestSchemaPath) -> delete_file(TestSchemaPath) ; true),
    % Create schema file
    open(TestSchemaPath, write, Stream),
    write(Stream, 'CREATE TABLE test_exists (id INTEGER PRIMARY KEY);'),
    close(Stream),
    % Create database file to trigger "already exists" error
    sqlite3_init_db(TestDbPath, TestSchemaPath).

cleanup_create_exists_test :-
    (exists_file('/tmp/test_spell_create_exists.db') -> delete_file('/tmp/test_spell_create_exists.db') ; true),
    (exists_file('/tmp/test_spell_create_exists_schema.sql') -> delete_file('/tmp/test_spell_create_exists_schema.sql') ; true),
    retractall(registered_db(database(test_create_exists_db), _, _)).
