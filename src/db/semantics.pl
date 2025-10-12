% Database entity system with reverse proxy predicating
:- grimoire_ensure_loaded('@/src/db/sqlite3.pl').

:- self_entity(db).

% === ENTITY DECLARATIONS ===

% Database command entities
entity(db(create)).
entity(db(execute)).
entity(db(query)).
entity(db(tables)).

% Database type entities
entity(db(sqlite)).
entity(db(table)).
entity(db(column)).

docstring(db(create),
    {|string(_)||
    Create a new database entity with schema validation.
    Initializes database file and schema for data storage.
    Format: db(create(DatabaseId, SchemaPath)).
        DatabaseId - unique identifier for the database
        SchemaPath - path to schema definition file
    |}
).

% === DYNAMIC REGISTRATION SYSTEM ===

% Db should be a unique identifier
% registered_db(database(Db), data(file(DbPath)), schema(file(SchemaPath)))
:- dynamic registered_db/3.
:- multifile registered_db/3.
:- discontiguous registered_db/3.

% === ENTITY PATTERNS ===

% Database entities
entity(database(Db)) :-
    registered_db(database(Db), data(file(_)), schema(file(_))).

% === DSL PATTERNS ===
% User-friendly high-level component patterns that expand to rich components

% SQLite database DSL pattern
% component(Entity, has(db(sqlite)), db(sqlite(database_id(Id), file(DbPath), schema(SchemaPath)))).
% Expands to multiple granular components below

% === COMPONENT PATTERNS (REVERSE PROXY) ===

% Database file component
component(database(Db), data, data(file(DbFile))) :-
    registered_db(database(Db), data(file(DbFile)), schema(file(_))).

% Schema component
component(database(Db), schema, schema(file(SchemaPath))) :-
    registered_db(database(Db), data(file(_)), schema(file(SchemaPath))).

% Table components
component(database(Db), table, table(TableName)) :-
    registered_db_table(database(Db), table(TableName)).

% Column components
component(database(Db), column, column(TableName, ColumnName, ColumnInfo)) :-
    registered_db_column(database(Db), column(TableName, ColumnName, ColumnInfo)).

% === DSL EXPANSION PATTERNS ===
% Expansion of high-level DSL patterns to granular components

% Extract database ID from DSL pattern
component(Entity, db_sqlite_id, Id) :-
    component(Entity, has(db(sqlite)), db(sqlite(database_id(Id), file(_), schema(_)))).

% Extract database file path from DSL pattern
component(Entity, db_sqlite_file, DbPath) :-
    component(Entity, has(db(sqlite)), db(sqlite(database_id(_), file(DbPath), schema(_)))).

% Extract schema path from DSL pattern
component(Entity, db_sqlite_schema, SchemaPath) :-
    component(Entity, has(db(sqlite)), db(sqlite(database_id(_), file(_), schema(SchemaPath)))).

% Derived: database validity check
component(Entity, db_sqlite_valid, true) :-
    component(Entity, db_sqlite_file, DbPath),
    validate_database(DbPath).

% Derived: list of tables in database
component(Entity, db_sqlite_tables, Tables) :-
    component(Entity, db_sqlite_file, DbPath),
    exists_file(DbPath),
    get_database_tables(DbPath, Tables).

% Table DSL pattern expansion
component(Entity, db_table_name, TableName) :-
    component(Entity, has(db(table)), db(table(TableName))).

% Column DSL pattern expansion
component(Entity, db_column_info, column(TableName, ColumnName, Constraints)) :-
    component(Entity, has(db(column)), db(column(TableName, ColumnName, Constraints))).

% === DISCOVERY PREDICATES ===

% Query SQLite for tables
registered_db_table(database(Db), table(TableName)) :-
    registered_db(database(Db), data(file(DbFile)), schema(file(_))),
    exists_file(DbFile),
    get_database_tables(DbFile, TableStrings),
    member(TableString, TableStrings),
    atom_string(TableName, TableString).

% Query SQLite for columns
registered_db_column(database(Db), column(TableName, ColumnName, ColumnInfo)) :-
    registered_db(database(Db), data(file(DbFile)), schema(file(_))),
    get_table_columns(DbFile, TableName, Columns),
    member(ColumnInfo, Columns),
    ColumnInfo = column(ColumnNameString, _, _, _, _),
    atom_string(ColumnName, ColumnNameString).

% === COMMAND SYSTEM ===

% Database subcommands (still needed for CLI routing)
component(db, subcommand, create).
component(db, subcommand, execute).
component(db, subcommand, query).
component(db, subcommand, tables).

% === SPELL REGISTRATIONS AND IMPLEMENTATIONS ===

% Create database from schema file
register_spell(
    conjure(db(create)),
    input(db(create(database_id('DbId'), file('DbPath'), schema(file('SchemaPath'))))),
    output(either(
        ok(database_created('DbId', 'DbPath')),
        error(db_error('Reason'))
    )),
    docstring("Create a new SQLite database from a schema file. Initializes the database and registers it for use.")
).

cast(conjure(db(create(DbId, DbPath, schema(file(SchemaFile))))), RetVal) :-
    sqlite3_init_db(DbPath, SchemaFile),
    assertz(registered_db(database(DbId), data(file(DbPath)), schema(file(SchemaFile)))),
    RetVal = ok(database_created(DbId, DbPath)).

% Create database from SQL string (creates schema file)
register_spell(
    conjure(db(create)),
    input(db(create(database_id('DbId'), file('DbPath'), schema(sql('SchemaSQL'))))),
    output(either(
        ok(database_created('DbId', 'DbPath')),
        error(db_error('Reason'))
    )),
    docstring("Create a new SQLite database from a SQL string. The SQL will be written to a schema file and then used to initialize the database.")
).

cast(conjure(db(create(DbId, DbPath, schema(sql(SchemaSQL))))), RetVal) :-
    % Extract database base name and create schema file
    file_name_extension(DbBaseName, db, DbPath),
    format(atom(SchemaFile), '~w.schema.sql', [DbBaseName]),
    open(SchemaFile, write, Stream),
    write(Stream, SchemaSQL),
    close(Stream),
    % Create database using the schema file
    sqlite3_init_db(DbPath, SchemaFile),
    assertz(registered_db(database(DbId), data(file(DbPath)), schema(file(SchemaFile)))),
    RetVal = ok(database_created(DbId, DbPath)).

% Execute SQL statement (mutation)
register_spell(
    conjure(db(execute)),
    input(db(execute(database_id('DbId'), sql('SQL')))),
    output(either(
        ok(executed('SQL')),
        error(db_error('Reason'))
    )),
    docstring("Execute a SQL statement that modifies the database (INSERT, UPDATE, DELETE, etc.).")
).

cast(conjure(db(execute(database_id(DbId), sql(SQL)))), RetVal) :-
    (registered_db(database(DbId), data(file(DbPath)), schema(file(_))) ->
        sqlite3_exec(DbPath, SQL),
        RetVal = ok(executed(SQL))
    ;
        RetVal = error(db_error(database_not_found(DbId)))
    ).

% Query database and return results
register_spell(
    perceive(db(query)),
    input(db(query(database_id('DbId'), sql('SQL')))),
    output(either(
        ok(query_results('Results')),
        error(query_error('Reason'))
    )),
    docstring("Execute a SQL query and return the results. Use for SELECT statements.")
).

cast(perceive(db(query(database_id(DbId), sql(SQL)))), RetVal) :-
    (registered_db(database(DbId), data(file(DbPath)), schema(file(_))) ->
        sqlite3_query(DbPath, SQL, QueryResults),
        RetVal = ok(query_results(QueryResults))
    ;
        RetVal = error(query_error(database_not_found(DbId)))
    ).

% Get list of tables in database
register_spell(
    perceive(db(tables)),
    input(db(tables(database_id('DbId')))),
    output(either(
        ok(tables('TableList')),
        error(tables_error('Reason'))
    )),
    docstring("Get a list of all tables in the specified database.")
).

cast(perceive(db(tables(database_id(DbId)))), RetVal) :-
    (registered_db(database(DbId), data(file(DbPath)), schema(file(_))) ->
        get_database_tables(DbPath, TableList),
        RetVal = ok(tables(TableList))
    ;
        RetVal = error(tables_error(database_not_found(DbId)))
    ).

% === DOCSTRINGS ===

docstring(db, "Database entity system with reverse proxy predicating. Register databases with registered_db(database(UniqueId), data(file(DbPath)), schema(file(SchemaPath))) to enable components").


