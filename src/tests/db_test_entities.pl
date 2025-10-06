% Test entities for db domain tests
% This file contains declarative entity/component definitions for testing

:- self_entity(test_entity(db)).

% Declare test entities for different test scenarios
entity(test_verify_entity).
entity(test_expanded_entity).
entity(test_table_entity).
entity(test_missing_entity).
entity(test_assertz_entity).

% Test entity for DSL pattern verification
component(test_verify_entity, has(db(sqlite)),
    db(sqlite(database_id(test_db), file('/tmp/test_verify_dsl.db'), schema('/tmp/test_schema.sql')))).

% Test entity for expanded components verification
component(test_expanded_entity, has(db(sqlite)),
    db(sqlite(database_id(test_expanded), file('/tmp/test_verify_expanded.db'), schema('/tmp/test_schema_expanded.sql')))).

% Test entity for table pattern verification
component(test_table_entity, db_sqlite_file, '/tmp/test_verify_table.db').
component(test_table_entity, has(db(table)), db(table(products))).

% Test entity for missing file verification (negative test)
component(test_missing_entity, db_sqlite_file, '/nonexistent/database.db').

% Test entity for assertz pattern
component(test_assertz_entity, has(db(sqlite)),
    db(sqlite(database_id(test_assertz), file('/tmp/test_assertz_pattern.db'), schema('/tmp/test_assertz_schema.sql')))).

docstring(test_entity(db), "Test entity container for db domain verification tests").
docstring(test_verify_entity, "Test entity for db DSL pattern verification").
docstring(test_expanded_entity, "Test entity for db expanded components verification").
docstring(test_table_entity, "Test entity for db table pattern verification").
docstring(test_missing_entity, "Test entity for missing db file verification").
docstring(test_assertz_entity, "Test entity for assertz pattern verification").
