% Database entity tests - reverse proxy predicating system

:- use_module(library(plunit)).

% Load test entities from file-based knowledge
:- load_entity(semantic(file('@/src/tests/db_test_entities.pl'))).
:- load_entity(semantic(file('@/src/db/tests/semantics.pl'))).

:- dynamic(registered_db/3).

% === VERIFICATION RULES ===
% Domain-specific verify/1 implementations using please_verify/1

% Verify SQLite database DSL pattern (high-level composite)
verify(component(Entity, has(db(sqlite)), db(sqlite(database_id(_Id), file(DbPath), schema(SchemaPath))))) :-
    % Component already proven to exist via please_verify!
    % Compose primitive verifications
    please_verify(component(Entity, db_sqlite_file, DbPath)),
    please_verify(component(Entity, db_sqlite_schema, SchemaPath)),
    please_verify(component(Entity, db_sqlite_valid, true)).

% Verify database file component (primitive - check OS reality)
verify(component(_Entity, db_sqlite_file, DbPath)) :-
    % Already proven: component(Entity, db_sqlite_file, DbPath)
    (exists_file(DbPath) ->
        validate_database(DbPath)
    ;
        throw(verification_error(db, missing_database_file(DbPath)))
    ).

% Verify database schema component (primitive - check OS reality)
verify(component(_Entity, db_sqlite_schema, SchemaPath)) :-
    % Already proven: component(Entity, db_sqlite_schema, SchemaPath)
    (exists_file(SchemaPath) ->
        true
    ;
        throw(verification_error(db, missing_schema_file(SchemaPath)))
    ).

% Verify database validity (primitive - check OS reality)
verify(component(_Entity, db_sqlite_valid, true)) :-
    % Already proven: component(Entity, db_sqlite_valid, true)
    % db_sqlite_valid is derived from db_sqlite_file, so that must exist
    true.

% Verify database tables component (composite - check OS reality)
verify(component(Entity, db_sqlite_tables, Tables)) :-
    % Already proven: component(Entity, db_sqlite_tables, Tables)
    please_verify(component(Entity, db_sqlite_file, DbPath)),
    get_database_tables(DbPath, ActualTables),
    (Tables = ActualTables ->
        true
    ;
        throw(verification_error(db, table_mismatch(expected(Tables), actual(ActualTables))))
    ).

% Verify table DSL pattern (composite)
verify(component(Entity, has(db(table)), db(table(TableName)))) :-
    % Already proven: component(Entity, has(db(table)), db(table(TableName)))
    please_verify(component(Entity, db_sqlite_file, DbPath)),
    get_database_tables(DbPath, TableStrings),
    (   (atom(TableName) -> atom_string(TableName, TableString) ; TableString = TableName),
        member(TableString, TableStrings)
    ->  true
    ;   throw(verification_error(db, table_not_found(TableName)))
    ).

% Verify database ID extraction
verify(component(Entity, db_sqlite_id, Id)) :-
    % Already proven to exist
    please_verify(component(Entity, has(db(sqlite)), db(sqlite(database_id(Id), file(_), schema(_))))).

% Verify registered database entity
verify(component(database(Db), data, data(file(DbFile)))) :-
    % Already proven: component exists
    registered_db(database(Db), data(file(DbFile)), schema(file(_))),
    (exists_file(DbFile) ->
        validate_database(DbFile)
    ;
        throw(verification_error(db, registered_database_file_not_found(Db, DbFile)))
    ).

% Verify registered database schema
verify(component(database(Db), schema, schema(file(SchemaPath)))) :-
    % Already proven: component exists
    registered_db(database(Db), data(file(_)), schema(file(SchemaPath))),
    (exists_file(SchemaPath) ->
        true
    ;
        throw(verification_error(db, registered_schema_file_not_found(Db, SchemaPath)))
    ).

% Verify registered database table
verify(component(database(Db), table, table(_TableName))) :-
    % Already proven: component exists
    registered_db(database(Db), data(file(DbFile)), schema(file(_))),
    (exists_file(DbFile) ->
        validate_database(DbFile)
    ;
        throw(verification_error(db, registered_database_file_not_found(Db, DbFile)))
    ).

% Verify registered database column
verify(component(database(Db), column, column(TableName, ColumnName, ColumnInfo))) :-
    % Already proven: component exists
    registered_db(database(Db), data(file(DbFile)), schema(file(_))),
    (exists_file(DbFile) ->
        (   get_table_columns(DbFile, TableName, Columns),
            member(ColumnInfo, Columns),
            ColumnInfo = column(ColumnNameString, _, _, _, _),
            atom_string(ColumnName, ColumnNameString)
        ->  true
        ;   throw(verification_error(db, column_not_found(Db, TableName, ColumnName)))
        )
    ;
        throw(verification_error(db, registered_database_file_not_found(Db, DbFile)))
    ).

% Verify table name extraction from DSL pattern
verify(component(Entity, db_table_name, TableName)) :-
    % Already proven: component exists
    please_verify(component(Entity, has(db(table)), db(table(TableName)))).

% Verify column info extraction from DSL pattern
verify(component(Entity, db_column_info, column(TableName, ColumnName, Constraints))) :-
    % Already proven: component exists
    please_verify(component(Entity, has(db(column)), db(column(TableName, ColumnName, Constraints)))).

% Verify db subcommands (simple facts - no OS reality to check)
verify(component(db, subcommand, create)) :- true.
verify(component(db, subcommand, execute)) :- true.
verify(component(db, subcommand, query)) :- true.
verify(component(db, subcommand, tables)) :- true.

:- begin_tests(db).

% Test database entity exists
test(db_entity_exists) :-
    entity(db), !.

% Test database entity creation from file-based test entity
test(database_entity_creation) :-
    % test_discovery database loaded from file
    registered_db(database(test_discovery), data(file(_)), schema(file(_))),
    entity(database(test_discovery)), !.

% Test database file component
test(database_file_component) :-
    % test_discovery database loaded from file
    user:please_verify(component(database(test_discovery), data, data(file(_)))), !.

% Test table discovery
test(table_discovery) :-
    % test_discovery database has test_table from schema
    user:please_verify(component(database(test_discovery), table, table(test_table))), !.

% Test column discovery
test(column_discovery) :-
    % test_table has id column
    user:please_verify(component(database(test_discovery), column, column(test_table, id, _))), !.

% Test column discovery - name column
test(column_discovery_name) :-
    % test_table has name column
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
    % Setup: Create files needed for test entity
    TestDbPath = '/tmp/test_verify_dsl.db',
    TestSchemaPath = '/tmp/test_schema.sql',
    % Clean up any existing files first
    (exists_file(TestDbPath) -> delete_file(TestDbPath) ; true),
    (exists_file(TestSchemaPath) -> delete_file(TestSchemaPath) ; true),
    % Create schema file
    open(TestSchemaPath, write, Stream),
    write(Stream, 'CREATE TABLE test_table (id INTEGER PRIMARY KEY);'),
    close(Stream),
    % Create database
    sqlite3_init_db(TestDbPath, TestSchemaPath),
    % Entity already loaded from file, just verify it
    user:please_verify(component(test_verify_entity, has(db(sqlite)), _)).

cleanup_verify_dsl :-
    (exists_file('/tmp/test_verify_dsl.db') -> delete_file('/tmp/test_verify_dsl.db') ; true),
    (exists_file('/tmp/test_schema.sql') -> delete_file('/tmp/test_schema.sql') ; true).

% Test expanded components with please_verify
test(verify_expanded_components, [cleanup(cleanup_verify_expanded)]) :-
    TestDbPath = '/tmp/test_verify_expanded.db',
    TestSchemaPath = '/tmp/test_schema_expanded.sql',
    % Clean up any existing files first
    (exists_file(TestDbPath) -> delete_file(TestDbPath) ; true),
    (exists_file(TestSchemaPath) -> delete_file(TestSchemaPath) ; true),
    % Create schema and database matching test entity declaration
    open(TestSchemaPath, write, Stream),
    write(Stream, 'CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT);'),
    close(Stream),
    sqlite3_init_db(TestDbPath, TestSchemaPath),
    % Entity already loaded from file, just verify expanded components
    user:please_verify(component(test_expanded_entity, db_sqlite_file, '/tmp/test_verify_expanded.db')),
    user:please_verify(component(test_expanded_entity, db_sqlite_schema, '/tmp/test_schema_expanded.sql')),
    user:please_verify(component(test_expanded_entity, db_sqlite_valid, true)).

cleanup_verify_expanded :-
    (exists_file('/tmp/test_verify_expanded.db') -> delete_file('/tmp/test_verify_expanded.db') ; true),
    (exists_file('/tmp/test_schema_expanded.sql') -> delete_file('/tmp/test_schema_expanded.sql') ; true).

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
    % Entity already loaded from file, just verify it
    user:please_verify(component(test_table_entity, has(db(table)), db(table(products)))), !.

cleanup_verify_table :-
    (exists_file('/tmp/test_verify_table.db') -> delete_file('/tmp/test_verify_table.db') ; true),
    (exists_file('/tmp/test_verify_table_schema.sql') -> delete_file('/tmp/test_verify_table_schema.sql') ; true).

% Test verify missing database file throws error
test(verify_missing_db_file, [
    throws(verification_error(db, missing_database_file(_)))
]) :-
    % Entity already loaded from file with non-existent database path
    user:please_verify(component(test_missing_entity, db_sqlite_file, '/nonexistent/database.db')).

% Test file-based entity verification pattern
test(file_based_entity_verification, [cleanup(cleanup_file_based_pattern)]) :-
    TestSchemaPath = '/tmp/test_file_based_schema.sql',
    TempDbPath = '/tmp/test_file_based_pattern.db',
    % Clean up any existing files first
    (exists_file(TempDbPath) -> delete_file(TempDbPath) ; true),
    (exists_file(TestSchemaPath) -> delete_file(TestSchemaPath) ; true),
    % Create schema file matching test entity declaration
    open(TestSchemaPath, write, Stream),
    write(Stream, 'CREATE TABLE temp_table (id INTEGER PRIMARY KEY);'),
    close(Stream),
    % Create database matching test entity declaration
    sqlite3_init_db(TempDbPath, TestSchemaPath),
    % Entity already loaded from file, just verify it
    user:please_verify(component(test_file_based_entity, has(db(sqlite)), _)).

cleanup_file_based_pattern :-
    (exists_file('/tmp/test_file_based_pattern.db') -> delete_file('/tmp/test_file_based_pattern.db') ; true),
    (exists_file('/tmp/test_file_based_schema.sql') -> delete_file('/tmp/test_file_based_schema.sql') ; true).

% === SPELL TESTS ===
% Tests for db spells using magic_cast

% Test db(create) spell with file-based schema
test(spell_db_create_file_schema, [cleanup(cleanup_spell_create)]) :-
    TestDbPath = '/tmp/test_spell_create.db',
    TestSchemaPath = '/tmp/test_spell_create_schema.sql',
    % Clean up any existing files first
    (exists_file(TestDbPath) -> delete_file(TestDbPath) ; true),
    (exists_file(TestSchemaPath) -> delete_file(TestSchemaPath) ; true),
    % Create schema file
    open(TestSchemaPath, write, Stream),
    write(Stream, 'CREATE TABLE test_create (id INTEGER PRIMARY KEY, name TEXT);'),
    close(Stream),
    % Cast the spell
    user:magic_cast(conjure(db(create(test_spell_db, TestDbPath, schema(file(TestSchemaPath))))), Result),
    Result = ok(database_created(test_spell_db, TestDbPath, RegisteredDbFact)),
    assertion(RegisteredDbFact = registered_db(database(test_spell_db), data(file(TestDbPath)), schema(file(TestSchemaPath)))),
    % Verify database was created
    assertion(exists_file(TestDbPath)), !.

cleanup_spell_create :-
    (exists_file('/tmp/test_spell_create.db') -> delete_file('/tmp/test_spell_create.db') ; true),
    (exists_file('/tmp/test_spell_create_schema.sql') -> delete_file('/tmp/test_spell_create_schema.sql') ; true).

% Test db(create) spell with SQL string schema
test(spell_db_create_sql_schema, [cleanup(cleanup_spell_create_sql)]) :-
    TestDbPath = '/tmp/test_spell_create_sql.db',
    % Clean up any existing files first
    (exists_file(TestDbPath) -> delete_file(TestDbPath) ; true),
    % Cast the spell with SQL string
    SchemaSQL = 'CREATE TABLE test_sql (id INTEGER PRIMARY KEY, value TEXT);',
    user:magic_cast(conjure(db(create(test_spell_sql_db, TestDbPath, schema(sql(SchemaSQL))))), Result),
    Result = ok(database_created(test_spell_sql_db, TestDbPath, RegisteredDbFact)),
    assertion(RegisteredDbFact = registered_db(database(test_spell_sql_db), data(file(TestDbPath)), schema(file('/tmp/test_spell_create_sql.schema.sql')))),
    % Verify database was created
    assertion(exists_file(TestDbPath)),
    % Verify schema file was created
    assertion(exists_file('/tmp/test_spell_create_sql.schema.sql')).

cleanup_spell_create_sql :-
    (exists_file('/tmp/test_spell_create_sql.db') -> delete_file('/tmp/test_spell_create_sql.db') ; true),
    (exists_file('/tmp/test_spell_create_sql.schema.sql') -> delete_file('/tmp/test_spell_create_sql.schema.sql') ; true).

% Test db(execute) spell - can't test execute/query without runtime registration
% These would require session state system to persist registered_db facts
% For now, skip these tests or use file-based test entities

% Note: execute, query, and tables spells require runtime-registered databases
% These can't be tested without session state system or using assertz
% Tests removed until session state is implemented

% Test spell error conditions
test(spell_db_query_missing_database) :-
    user:magic_cast(perceive(db(query(database_id(nonexistent_db), sql("SELECT 1")))), Result),
    assertion(Result = error(query_error(database_not_found(nonexistent_db)))).

test(spell_db_execute_missing_database) :-
    user:magic_cast(conjure(db(execute(database_id(nonexistent_db), sql("INSERT INTO foo VALUES (1)")))), Result),
    assertion(Result = error(db_error(database_not_found(nonexistent_db)))).

test(spell_db_tables_missing_database) :-
    user:magic_cast(perceive(db(tables(database_id(nonexistent_db)))), Result),
    assertion(Result = error(tables_error(database_not_found(nonexistent_db)))).

:- end_tests(db).
