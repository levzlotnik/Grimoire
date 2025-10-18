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

% === COMPONENT EXPANSION USING ==> OPERATOR ===
% The ==> operator automatically generates BOTH:
% 1. Component expansion rules (generative)
% 2. Verify clause with please_verify calls (discriminative)

% High-level has(db(sqlite)) pattern expands to queryable properties
component(Entity, has(db(sqlite)), db(sqlite(database_id(DbId), file(File), schema(Schema))))
    ==> component(Entity, db_sqlite_id, DbId),
        component(Entity, db_sqlite_file, File),
        component(Entity, db_sqlite_schema, Schema).

% === LEAF COMPONENT VERIFICATIONS USING :: OPERATOR ===
% The :: operator ONLY generates verify clauses for leaf components

% Verify database ID is valid atom
component(_E, db_sqlite_id, DbId)
    :: (atom(DbId) ->
           (DbId \= '' ->
               true
           ;
               throw(verification_error(db, empty_database_id))
           )
       ;
           throw(verification_error(db, database_id_not_atom(DbId)))
       ).

% Verify database file exists and is readable SQLite database
component(_E, db_sqlite_file, File)
    :: (atom(File) ; string(File)),
       atom_string(File, FileStr),
       (exists_file(FileStr) ->
           (access_file(FileStr, read) ->
               (validate_database(FileStr) ->
                   true
               ;
                   throw(verification_error(db, invalid_sqlite_file(File)))
               )
           ;
               throw(verification_error(db, file_not_readable(File)))
           )
       ;
           throw(verification_error(db, file_not_found(File)))
       ).

% Verify schema file exists and is readable
component(_E, db_sqlite_schema, Schema)
    :: (atom(Schema) ; string(Schema)),
       atom_string(Schema, SchemaStr),
       (exists_file(SchemaStr) ->
           (access_file(SchemaStr, read) ->
               true
           ;
               throw(verification_error(db, schema_not_readable(Schema)))
           )
       ;
           throw(verification_error(db, schema_not_found(Schema)))
       ).

% Derived: database validity check (no verification - it's a query)
component(Entity, db_sqlite_valid, true) :-
    component(Entity, db_sqlite_file, DbPath),
    validate_database(DbPath).

% Derived: list of tables in database (no verification - it's a query)
component(Entity, db_sqlite_tables, Tables) :-
    component(Entity, db_sqlite_file, DbPath),
    exists_file(DbPath),
    get_database_tables(DbPath, Tables).

% === COMMAND SYSTEM ===

% Database subcommands (still needed for CLI routing)
component(db, subcommand, create).
component(db, subcommand, execute).
component(db, subcommand, query).
component(db, subcommand, tables).

% === PHASE 3: SPELL REGISTRATIONS ===

% Create database - single spell handling both file and SQL patterns
% Phase 3 doesn't support spell overloading, so we need ONE implementation
% that branches based on schema(file(...)) vs schema(sql(...))
register_spell(
    conjure(db(create)),
    input(db(create(database_id('DbId'), file('DbPath'), 'SchemaSpec'))),
    output(either(
        ok(database_created('DbId', 'DbPath', 'RegisteredDbFact')),
        error(db_error('Reason'))
    )),
    "Create a new SQLite database from a schema file or SQL string. Returns the registered_db fact that should be persisted to session state.",
    [session_persistent(true)],
    implementation(conjure(db(create(DbId, DbPath, SchemaSpec))), Result, (
        % Validation
        atom(DbId),
        DbId \= '',
        (atom(DbPath) ; string(DbPath)),

        % Convert to strings if needed
        (atom(DbPath) -> atom_string(DbPath, DbPathStr) ; DbPathStr = DbPath),

        % Verify database path directory exists and is writable
        file_directory_name(DbPathStr, DbDir),
        (exists_directory(DbDir) ->
            access_file(DbDir, write)
        ;
            throw(error(db_error(directory_not_found(DbDir))))
        ),

        % Verify database doesn't already exist (prevent overwrite)
        (\+ exists_file(DbPathStr) ->
            true
        ;
            throw(error(db_error(database_already_exists(DbPathStr))))
        ),

        % Handle schema(file(...)) or schema(sql(...))
        (SchemaSpec = schema(file(SchemaFile)) ->
            % File-based schema
            (atom(SchemaFile) -> atom_string(SchemaFile, SchemaFileStr) ; SchemaFileStr = SchemaFile),
            % Verify schema file exists and is valid
            exists_file(SchemaFileStr),
            access_file(SchemaFileStr, read),
            % Create database
            sqlite3_init_db(DbPathStr, SchemaFileStr),
            % Return the schema file path
            ActualSchemaFile = SchemaFile
        ; SchemaSpec = schema(sql(SchemaSQL)) ->
            % SQL string schema
            string(SchemaSQL),
            % Validate SQL string contains CREATE statements
            (sub_string(SchemaSQL, _, _, _, "CREATE") ; sub_string(SchemaSQL, _, _, _, "create")),
            % Generate schema file
            file_name_extension(DbBaseName, db, DbPathStr),
            format(atom(GeneratedSchemaFile), '~w.schema.sql', [DbBaseName]),
            % Write schema to file
            open(GeneratedSchemaFile, write, Stream),
            write(Stream, SchemaSQL),
            close(Stream),
            % Verify schema file was written
            exists_file(GeneratedSchemaFile),
            access_file(GeneratedSchemaFile, read),
            % Create database
            sqlite3_init_db(DbPathStr, GeneratedSchemaFile),
            % Return the generated schema file path
            ActualSchemaFile = GeneratedSchemaFile
        ;
            throw(error(db_error(invalid_schema_spec(SchemaSpec))))
        ),

        % Verify creation succeeded
        (exists_file(DbPathStr) ->
            validate_database(DbPathStr)
        ;
            throw(error(db_error(creation_failed(DbPathStr))))
        ),

        % Return success - hook will persist to state.pl
        RegisteredDbFact = registered_db(database(DbId), data(file(DbPath)), schema(file(ActualSchemaFile))),
        Result = ok(database_created(DbId, DbPath, RegisteredDbFact))
    ))
).

% Execute SQL statement (mutation)
register_spell(
    conjure(db(execute)),
    input(db(execute(database_id('DbId'), sql('SQL')))),
    output(either(
        ok(executed('SQL')),
        error(db_error('Reason'))
    )),
    "Execute a SQL statement that modifies the database (INSERT, UPDATE, DELETE, etc.).",
    [],
    implementation(conjure(db(execute(database_id(DbId), sql(SQL)))), Result, (
        % Validation
        atom(DbId),
        string(SQL),

        % Fetch and verify database path using please_verify
        (registered_db(database(DbId), data(file(DbPath)), schema(file(_))) ->
            % Verify file exists and is accessible
            (atom(DbPath) -> atom_string(DbPath, DbPathStr) ; DbPathStr = DbPath),
            (exists_file(DbPathStr) ->
                (access_file(DbPathStr, write) ->
                    (validate_database(DbPathStr) ->
                        % Execute
                        sqlite3_exec(DbPathStr, SQL),
                        Result = ok(executed(SQL))
                    ;
                        throw(error(db_error(invalid_database(DbId))))
                    )
                ;
                    throw(error(db_error(database_not_writable(DbId))))
                )
            ;
                throw(error(db_error(database_file_not_found(DbId, DbPathStr))))
            )
        ;
            Result = error(db_error(database_not_found(DbId)))
        )
    ))
).

% Query database and return results
register_spell(
    perceive(db(query)),
    input(db(query(database_id('DbId'), sql('SQL')))),
    output(either(
        ok(query_results('Results')),
        error(query_error('Reason'))
    )),
    "Execute a SQL query and return the results. Use for SELECT statements.",
    [],
    implementation(perceive(db(query(database_id(DbId), sql(SQL)))), Result, (
        % Validation
        atom(DbId),
        string(SQL),

        % Fetch and verify database path
        (registered_db(database(DbId), data(file(DbPath)), schema(file(_))) ->
            % Verify file exists and is readable
            (atom(DbPath) -> atom_string(DbPath, DbPathStr) ; DbPathStr = DbPath),
            (exists_file(DbPathStr) ->
                (access_file(DbPathStr, read) ->
                    (validate_database(DbPathStr) ->
                        % Query
                        sqlite3_query(DbPathStr, SQL, QueryResults),
                        Result = ok(query_results(QueryResults))
                    ;
                        throw(error(query_error(invalid_database(DbId))))
                    )
                ;
                    throw(error(query_error(database_not_readable(DbId))))
                )
            ;
                throw(error(query_error(database_file_not_found(DbId, DbPathStr))))
            )
        ;
            Result = error(query_error(database_not_found(DbId)))
        )
    ))
).

% Get list of tables in database
register_spell(
    perceive(db(tables)),
    input(db(tables(database_id('DbId')))),
    output(either(
        ok(tables('TableList')),
        error(tables_error('Reason'))
    )),
    "Get a list of all tables in the specified database.",
    [],
    implementation(perceive(db(tables(database_id(DbId)))), Result, (
        % Validation
        atom(DbId),

        % Fetch and verify database path
        (registered_db(database(DbId), data(file(DbPath)), schema(file(_))) ->
            % Verify file exists and is readable
            (atom(DbPath) -> atom_string(DbPath, DbPathStr) ; DbPathStr = DbPath),
            (exists_file(DbPathStr) ->
                (access_file(DbPathStr, read) ->
                    (validate_database(DbPathStr) ->
                        % Get tables
                        get_database_tables(DbPathStr, TableList),
                        Result = ok(tables(TableList))
                    ;
                        throw(error(tables_error(invalid_database(DbId))))
                    )
                ;
                    throw(error(tables_error(database_not_readable(DbId))))
                )
            ;
                throw(error(tables_error(database_file_not_found(DbId, DbPathStr))))
            )
        ;
            Result = error(tables_error(database_not_found(DbId)))
        )
    ))
).

% === DOCSTRINGS ===

docstring(db, "Database entity system with reverse proxy predicating. Register databases with registered_db(database(UniqueId), data(file(DbPath)), schema(file(SchemaPath))) to enable components").
