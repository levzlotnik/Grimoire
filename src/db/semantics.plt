:- use_module(library(plunit)).

% Load domain semantics first (includes sqlite3.pl)
:- load_entity(semantic(file('@/src/db/semantics.pl'))).

% Load test entities from file-based knowledge
:- load_entity(semantic(file('@/src/db/tests/db_test_entities.pl'))).

%% ============================================================================
%% Test Suite
%% ============================================================================

:- begin_tests(db).

%% ============================================================================
%% Test Case 1: Entity and component existence
%% ============================================================================

test(db_entity_exists) :-
    entity(db).

%% ============================================================================
%% Test Case 2: Component expansion from has(db(sqlite))
%% ============================================================================

test(dsl_expands_to_db_sqlite_file) :-
    user:please_verify(component(test_verify_entity, db_sqlite_file, _File)).

test(dsl_expands_to_db_sqlite_schema) :-
    user:please_verify(component(test_verify_entity, db_sqlite_schema, _Schema)).

%% ============================================================================
%% Test Case 3: Composite verification using please_verify
%% ============================================================================

test(composite_verification_succeeds) :-
    % This verifies all expanded components using please_verify composition
    user:please_verify(component(test_verify_entity, has(db(sqlite)), _)).

%% ============================================================================
%% Test Case 4: Derived components
%% ============================================================================

test(db_sqlite_tables_component) :-
    user:please_verify(component(test_expanded_entity, db_sqlite_tables, Tables)),
    assertion(is_list(Tables)).

%% ============================================================================
%% Test Case 5: Leaf verification failures
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


%% ============================================================================
%% Test Case 6: Spell - conjure(db(create)) with file schema
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
    user:magic_cast(conjure(db(create(file(TestDbPath), schema(file(TestSchemaPath))))), Result),

    % Verify result structure
    Result = ok(db(sqlite(file(DbPath)))),
    assertion(DbPath = TestDbPath),

    % Verify database file was created
    assertion(exists_file(TestDbPath)),

    % Verify database is valid SQLite
    assertion(validate_database(TestDbPath)).

%% ============================================================================
%% Test Case 7: Spell - conjure(db(create)) with SQL string
%% ============================================================================

test(spell_create_with_sql_string, [
    cleanup(cleanup_spell_create_sql)
]) :-
    TestDbPath = '/tmp/test_spell_create_sql.db',
    SchemaSQL = 'CREATE TABLE test_sql (id INTEGER PRIMARY KEY, value TEXT);',

    % Cast the spell
    user:magic_cast(conjure(db(create(file(TestDbPath), schema(sql(SchemaSQL))))), Result),

    % Verify result structure
    Result = ok(db(sqlite(file(DbPath)))),
    assertion(DbPath = TestDbPath),

    % Verify database file was created
    assertion(exists_file(TestDbPath)),

    % Verify generated schema file exists
    assertion(exists_file('/tmp/test_spell_create_sql.schema.sql')),

    % Verify database is valid SQLite
    assertion(validate_database(TestDbPath)).

%% ============================================================================
%% Test Case 8: Spell - conjure(db(execute))
%% ============================================================================

test(spell_execute) :-
    TestDbPath = '/tmp/test_e2e_execute.db',
    SchemaSQL = 'CREATE TABLE test_exec (id INTEGER PRIMARY KEY, name TEXT);',

    % Clean up any existing files
    (exists_file(TestDbPath) -> delete_file(TestDbPath) ; true),

    % Step 1: Create database using create spell with SQL schema
    user:magic_cast(conjure(db(create(file(TestDbPath), schema(sql(SchemaSQL))))), CreateResult),
    CreateResult = ok(db(sqlite(file(Db)))),

    % Step 2: Execute insertion using execute spell
    user:magic_cast(conjure(db(execute(database(Db), sql("INSERT INTO test_exec (name) VALUES ('test_value')")))), ExecResult),
    assertion(ExecResult = ok(executed(_))),

    % Step 3: Query to verify using query spell
    user:magic_cast(perceive(db(query(database(Db), sql("SELECT name FROM test_exec")))), QueryResult),
    QueryResult = ok(query_results(Rows)),
    assertion(is_list(Rows)),
    length(Rows, Len),
    assertion(Len = 1),

    % Step 4: Clean up (db file and generated schema file)
    delete_file(TestDbPath),
    (exists_file('/tmp/test_e2e_execute.schema.sql') -> delete_file('/tmp/test_e2e_execute.schema.sql') ; true).

%% ============================================================================
%% Test Case 9: Spell - perceive(db(query))
%% ============================================================================

test(spell_query) :-
    TestDbPath = '/tmp/test_spell_query.db',
    SchemaSQL = 'CREATE TABLE test_query (id INTEGER PRIMARY KEY, name TEXT);',

    % Clean up any existing files
    (exists_file(TestDbPath) -> delete_file(TestDbPath) ; true),

    % Step 1: Create database
    user:magic_cast(conjure(db(create(file(TestDbPath), schema(sql(SchemaSQL))))), CreateResult),
    CreateResult = ok(db(sqlite(file(Db)))),

    % Step 2: Insert test data
    user:magic_cast(conjure(db(execute(database(Db), sql("INSERT INTO test_query (name) VALUES ('test_name')")))), _),

    % Step 3: Query the database
    user:magic_cast(perceive(db(query(database(Db), sql("SELECT name FROM test_query")))), Result),

    % Verify result - sqlite3_query returns JSON dicts
    Result = ok(query_results(QueryResults)),
    assertion(is_list(QueryResults)),
    assertion(length(QueryResults, 1)),

    % Step 4: Clean up
    delete_file(TestDbPath),
    (exists_file('/tmp/test_spell_query.schema.sql') -> delete_file('/tmp/test_spell_query.schema.sql') ; true).

%% ============================================================================
%% Test Case 10: Spell - perceive(db(tables))
%% ============================================================================

test(spell_tables) :-
    TestDbPath = '/tmp/test_spell_tables.db',
    SchemaSQL = 'CREATE TABLE test_tables (id INTEGER PRIMARY KEY);',

    % Clean up any existing files
    (exists_file(TestDbPath) -> delete_file(TestDbPath) ; true),

    % Step 1: Create database with a table
    user:magic_cast(conjure(db(create(file(TestDbPath), schema(sql(SchemaSQL))))), CreateResult),
    CreateResult = ok(db(sqlite(file(Db)))),

    % Step 2: Get list of tables
    user:magic_cast(perceive(db(tables(database(Db)))), Result),

    % Verify result
    Result = ok(tables(TableList)),
    assertion(is_list(TableList)),
    assertion(member("test_tables", TableList)),

    % Step 3: Clean up
    delete_file(TestDbPath),
    (exists_file('/tmp/test_spell_tables.schema.sql') -> delete_file('/tmp/test_spell_tables.schema.sql') ; true).

%% ============================================================================
%% Test Case 11: Spell error conditions
%% ============================================================================

test(spell_execute_missing_database) :-
    user:magic_cast(conjure(db(execute(database('/nonexistent/database.db'), sql("INSERT INTO foo VALUES (1)")))), Result),
    assertion(Result = error(db_error(database_file_not_found('/nonexistent/database.db')))).

test(spell_query_missing_database) :-
    user:magic_cast(perceive(db(query(database('/nonexistent/database.db'), sql("SELECT 1")))), Result),
    assertion(Result = error(query_error(database_file_not_found('/nonexistent/database.db')))).

test(spell_tables_missing_database) :-
    user:magic_cast(perceive(db(tables(database('/nonexistent/database.db')))), Result),
    assertion(Result = error(tables_error(database_file_not_found('/nonexistent/database.db')))).

test(spell_create_already_exists, [
    setup(setup_create_exists_test),
    cleanup(cleanup_create_exists_test),
    throws(error(db_error(database_already_exists(_))))
]) :-
    TestDbPath = '/tmp/test_spell_create_exists.db',
    TestSchemaPath = '/tmp/test_spell_create_exists_schema.sql',
    user:magic_cast(conjure(db(create(file(TestDbPath), schema(file(TestSchemaPath))))), _).

%% ============================================================================
%% Test Case 12: Spell - conjure(db(write_table))
%% ============================================================================

test(spell_write_table, [
    cleanup(cleanup_spell_write_table)
]) :-
    TestDbPath = '/tmp/test_spell_write_table.db',
    TestTable = test_components,

    % Create database
    user:magic_cast(conjure(db(create(file(TestDbPath), schema(sql("CREATE TABLE dummy (id INTEGER);"))))), CreateResult),
    CreateResult = ok(db(sqlite(file(_)))),

    % Write rows with mixed types
    TestRows = [
        test_components(entity1, prop1, atom_value, ""),
        test_components(entity(2), prop2, "string_value", error(failed)),
        test_components(entity3, has(thing), compound(nested(value)), "")
    ],
    user:magic_cast(conjure(db(write_table(database(TestDbPath), table(TestTable), rows(TestRows)))), WriteResult),

    % Verify result
    assertion(WriteResult = ok(written(count(3)))).

%% ============================================================================
%% Test Case 13: Spell - perceive(db(read_table))
%% ============================================================================

test(spell_read_table, [
    cleanup(cleanup_spell_read_table)
]) :-
    TestDbPath = '/tmp/test_spell_read_table.db',
    TestTable = test_read,

    % Create database and write data
    user:magic_cast(conjure(db(create(file(TestDbPath), schema(sql("CREATE TABLE dummy (id INTEGER);"))))), _),

    TestRows = [
        test_read(e1, p1, v1, ""),
        test_read(e2, p2, v2, "err")
    ],
    user:magic_cast(conjure(db(write_table(database(TestDbPath), table(TestTable), rows(TestRows)))), _),

    % Read rows back
    user:magic_cast(perceive(db(read_table(database(TestDbPath), table(TestTable)))), ReadResult),

    % Verify result
    ReadResult = ok(rows(ReadRows)),
    assertion(is_list(ReadRows)),
    assertion(length(ReadRows, 2)).

%% ============================================================================
%% Test Case 14: Round-trip write/read with type preservation
%% ============================================================================

test(spell_write_read_roundtrip, [
    cleanup(cleanup_spell_roundtrip)
]) :-
    TestDbPath = '/tmp/test_spell_roundtrip.db',
    TestTable = components,

    % Create database
    user:magic_cast(conjure(db(create(file(TestDbPath), schema(sql("CREATE TABLE dummy (id INTEGER);"))))), _),

    % Write rows with mixed types - atoms, strings, compound terms
    TestRows = [
        components(entity1, prop1, atom_value, ""),
        components(entity(2), prop2, "string_value", error(verification_failed)),
        components(entity3, has(something), compound(nested(value)), "")
    ],
    user:magic_cast(conjure(db(write_table(database(TestDbPath), table(TestTable), rows(TestRows)))), WriteResult),
    assertion(WriteResult = ok(written(count(3)))),

    % Read rows back
    user:magic_cast(perceive(db(read_table(database(TestDbPath), table(TestTable)))), ReadResult),
    ReadResult = ok(rows(ReadRows)),

    % Verify exact round-trip with type preservation
    assertion(ReadRows == TestRows).

:- end_tests(db).

%% ============================================================================
%% Setup/Cleanup Helpers - FILESYSTEM ONLY
%% ============================================================================

% DSL test database already exists in src/db/tests/spell_test_db/

% Setup for invalid DB test - entity already in db_test_entities.pl
setup_invalid_db_test :-
    true.

cleanup_invalid_db_test :-
    (exists_file('/tmp/test_invalid_db.db') -> delete_file('/tmp/test_invalid_db.db') ; true).

% Derived test database (test_expanded_entity) already exists in src/db/tests/spell_test_db/

% Spell cleanup helpers
cleanup_spell_create_file :-
    (exists_file('/tmp/test_spell_create_file.db') -> delete_file('/tmp/test_spell_create_file.db') ; true),
    (exists_file('/tmp/test_spell_create_file_schema.sql') -> delete_file('/tmp/test_spell_create_file_schema.sql') ; true).

cleanup_spell_create_sql :-
    (exists_file('/tmp/test_spell_create_sql.db') -> delete_file('/tmp/test_spell_create_sql.db') ; true),
    (exists_file('/tmp/test_spell_create_sql.schema.sql') -> delete_file('/tmp/test_spell_create_sql.schema.sql') ; true).

% Test databases for execute, query, and tables spells already exist in src/db/tests/spell_test_db/

% Setup for create exists test
setup_create_exists_test :-
    TestDbPath = '/tmp/test_spell_create_exists.db',
    TestSchemaPath = '/tmp/test_spell_create_exists_schema.sql',
    (exists_file(TestDbPath) -> delete_file(TestDbPath) ; true),
    (exists_file(TestSchemaPath) -> delete_file(TestSchemaPath) ; true),
    % Create schema file and database using spell
    open(TestSchemaPath, write, Stream),
    write(Stream, 'CREATE TABLE test_exists (id INTEGER PRIMARY KEY);'),
    close(Stream),
    % Create database file to trigger "already exists" error
    user:magic_cast(conjure(db(create(file(TestDbPath), schema(file(TestSchemaPath))))), Result),
    assertion(Result = ok(_)).

cleanup_create_exists_test :-
    (exists_file('/tmp/test_spell_create_exists.db') -> delete_file('/tmp/test_spell_create_exists.db') ; true),
    (exists_file('/tmp/test_spell_create_exists_schema.sql') -> delete_file('/tmp/test_spell_create_exists_schema.sql') ; true).

% Cleanup for write_table test
cleanup_spell_write_table :-
    (exists_file('/tmp/test_spell_write_table.db') -> delete_file('/tmp/test_spell_write_table.db') ; true),
    (exists_file('/tmp/test_spell_write_table.schema.sql') -> delete_file('/tmp/test_spell_write_table.schema.sql') ; true).

% Cleanup for read_table test
cleanup_spell_read_table :-
    (exists_file('/tmp/test_spell_read_table.db') -> delete_file('/tmp/test_spell_read_table.db') ; true),
    (exists_file('/tmp/test_spell_read_table.schema.sql') -> delete_file('/tmp/test_spell_read_table.schema.sql') ; true).

% Cleanup for roundtrip test
cleanup_spell_roundtrip :-
    (exists_file('/tmp/test_spell_roundtrip.db') -> delete_file('/tmp/test_spell_roundtrip.db') ; true),
    (exists_file('/tmp/test_spell_roundtrip.schema.sql') -> delete_file('/tmp/test_spell_roundtrip.schema.sql') ; true).
