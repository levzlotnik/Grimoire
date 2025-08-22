% Database entity system with reverse proxy predicating
:- ensure_loaded('sqlite3.pl').

:- self_entity(db).

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

% Database command constructor
component(command, ctor, db).
component(db, subcommand, create).

% Create database from schema file
run(command(db(create(DbId, DbPath, schema(file(SchemaFile))))), RetVal) :-
    sqlite3_init_db(DbPath, SchemaFile),
    assertz(registered_db(database(DbId), data(file(DbPath)), schema(file(SchemaFile)))),
    RetVal = ok(database_created(DbId, DbPath)).

% Create database from SQL string (creates schema file)
run(command(db(create(DbId, DbPath, schema(sql(SchemaSQL))))), RetVal) :-
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

docstring(command(db(create)),
    {|string(_)||
    Create SQLite database with schema.
    Creates a .db file and corresponding .schema.sql file.
    Format: command(db(create(DbId, DbPath, schema(file(SchemaFile)|sql(SchemaSQL))))).
        DbId - unique database identifier for ECS registration
        DbPath - path to .db file (must end with .db extension)
        schema(file(SchemaFile)) - path to existing .sql schema file
        schema(sql(SchemaSQL)) - SQL string (creates {db_name}.schema.sql file)
    |}
).

% === TEST REGISTRATION (for testing only) ===

% Test database registration - will be created by tests
registered_db(database(test_discovery), data(file('test_discovery.db')), schema(file('test_discovery.schema.sql'))) :-
    exists_file('test_discovery.db').