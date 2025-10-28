% Test entities for db domain tests
% This file contains declarative entity/component definitions for testing

:- self_entity(test_entity(db)).

% Declare test entities for different test scenarios
entity(test_verify_entity).
entity(test_expanded_entity).
entity(test_missing_entity).
entity(test_invalid_entity).

% Test entity for DSL pattern verification
component(test_verify_entity, has(db(sqlite)), Spec) :-
    component(test_entity(db), self, semantic(file(FilePath))),
    file_directory_name(FilePath, Dir),
    directory_file_path(Dir, 'spell_test_db/test_dsl.db', DbPath),
    Spec = db(sqlite(file(DbPath))).

% Test entity for expanded components verification
component(test_expanded_entity, has(db(sqlite)), Spec) :-
    component(test_entity(db), self, semantic(file(FilePath))),
    file_directory_name(FilePath, Dir),
    directory_file_path(Dir, 'spell_test_db/test_expanded.db', DbPath),
    Spec = db(sqlite(file(DbPath))).

% Test entity for missing file verification (negative test)
component(test_missing_entity, db_sqlite_file, '/nonexistent/database.db').
component(test_missing_entity, core_dump_ignorelist, [db_sqlite_file]).

% Test entity for invalid file verification (negative test)
component(test_invalid_entity, db_sqlite_file, '/tmp/test_invalid_db.db').
component(test_invalid_entity, core_dump_ignorelist, [db_sqlite_file]).

docstring(test_entity(db), "Test entity container for db domain verification tests").
docstring(test_verify_entity, "Test entity for db DSL pattern verification").
docstring(test_expanded_entity, "Test entity for db expanded components verification").
docstring(test_missing_entity, "Test entity for missing db file verification").
docstring(test_invalid_entity, "Test entity for invalid db file verification").
