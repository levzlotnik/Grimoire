% Database entity system with reverse proxy predicating
:- grimoire_ensure_loaded('@/src/db/sqlite3.pl').

:- self_entity(db).

% Database command entities
entity(db(create)).

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

% Database conjure constructor (state-changing)
component(conjure, ctor, db(create)).
component(db, subcommand, create).

% Create database from schema file
cast(conjure(db(create(DbId, DbPath, schema(file(SchemaFile))))), RetVal) :-
    sqlite3_init_db(DbPath, SchemaFile),
    assertz(registered_db(database(DbId), data(file(DbPath)), schema(file(SchemaFile)))),
    RetVal = ok(database_created(DbId, DbPath)).

% Create database from SQL string (creates schema file)
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

% === DOCSTRINGS ===

docstring(db, "Database entity system with reverse proxy predicating. Register databases with registered_db(database(UniqueId), data(file(DbPath)), schema(file(SchemaPath))) to enable components").


