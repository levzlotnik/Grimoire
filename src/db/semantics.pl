:- grimoire_ensure_loaded('@/src/db/sqlite3.pl').

:- self_entity(db, "Database domain - create and query SQLite databases using explicit database objects").

% === ENTITY DECLARATIONS ===

% Database command entities
entity(db(create)).
entity(db(execute)).
entity(db(execute_params)).
entity(db(query)).
entity(db(query_params)).
entity(db(tables)).
entity(db(write_table)).
entity(db(read_table)).

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

% === COMPONENT EXPANSION USING ==> OPERATOR ===
% The ==> operator automatically generates BOTH:
% 1. Component expansion rules (generative)
% 2. Verify clause with please_verify calls (discriminative)

% High-level has(db(sqlite)) pattern expands to queryable properties
% Schema is derived dynamically from the database file itself
component(Entity, has(db(sqlite)), db(sqlite(file(File))))
    ==> component(Entity, db_sqlite_file, File),
        (component(Entity, db_sqlite_schema, Schema) :- get_database_schema(File, Schema))
        :: Schema = sql(Sql), atom_string(Sql, SqlStr), SqlStr \= ''.

% === LEAF COMPONENT VERIFICATIONS USING :: OPERATOR ===
% The :: operator ONLY generates verify clauses for leaf components

% Verify database file exists and is readable SQLite database
component(_, db_sqlite_file, File)
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

% Derive tables from database file
component(Entity, db_sqlite_file, DbPath) ==>
    (component(Entity, db_sqlite_tables, Tables) :- exists_file(DbPath), get_database_tables(DbPath, Tables))
    ::  exists_file(DbPath).

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
    input(db(create(file('DbPath'), schema('SchemaSpec')))),
    output(either(
        ok(db(sqlite(file('DbPath')))),
        error(db_error('Reason'))
    )),
    "Create a new SQLite database from a schema file or SQL string. Returns a database object.",
    [session_persistent(has(db(sqlite)))],
    implementation(conjure(db(create(file(DbPath), schema(SchemaSpec)))), Result, (
        % Validate path
        ((atom(DbPath) ; string(DbPath)) -> true ; throw(error(db_error(invalid_db_path(DbPath))))),
        (atom(DbPath) -> atom_string(DbPath, DbPathStr) ; DbPathStr = DbPath),

        % Validate directory exists and is writable
        file_directory_name(DbPathStr, DbDir),
        (exists_directory(DbDir) ->
            access_file(DbDir, write)
        ;
            throw(error(db_error(directory_not_found(DbDir))))
        ),

        % Check DB doesn't already exist
        (exists_file(DbPathStr) ->
            throw(error(db_error(database_already_exists(DbPathStr))))
        ;
            true
        ),

        % Process schema (file or SQL)
        process_schema_spec(SchemaSpec, DbPathStr, _ActualSchemaFile),

        % Verify DB was created
        (exists_file(DbPathStr) ->
            validate_database(DbPathStr)
        ;
            throw(error(db_error(creation_failed(DbPathStr))))
        ),

        Result = ok(db(sqlite(file(DbPath))))
    ))
).

% Execute SQL statement (mutation)
register_spell(
    conjure(db(execute)),
    input(db(execute(database('DbPath'), sql('SQL')))),
    output(either(
        ok(executed('SQL')),
        error(db_error('Reason'))
    )),
    "Execute a SQL statement that modifies the database (INSERT, UPDATE, DELETE, etc.).",
    [],
    implementation(conjure(db(execute(database(DbPath), sql(SQL)))), Result, (
        % Validation
        string(SQL),
        (atom(DbPath) -> atom_string(DbPath, DbPathStr) ; DbPathStr = DbPath),

        % Verify file exists and is accessible
        (exists_file(DbPathStr) ->
            (access_file(DbPathStr, write) ->
                (validate_database(DbPathStr) ->
                    % Execute
                    sqlite3_exec(DbPathStr, SQL),
                    Result = ok(executed(SQL))
                ;
                    Result = error(db_error(invalid_database(DbPath)))
                )
            ;
                Result = error(db_error(database_not_writable(DbPath)))
            )
        ;
            Result = error(db_error(database_file_not_found(DbPath)))
        )
    ))
).

% Execute parameterized SQL statement (mutation, SQL injection safe)
register_spell(
    conjure(db(execute_params)),
    input(db(execute_params(database('DbPath'), sql('SQL'), params('Params')))),
    output(either(
        ok(executed('SQL')),
        error(db_error('Reason'))
    )),
    "Execute a parameterized SQL statement. Use ? placeholders for parameters (SQL injection safe).",
    [],
    implementation(conjure(db(execute_params(database(DbPath), sql(SQL), params(Params)))), Result, (
        % Validation
        string(SQL),
        is_list(Params),
        (atom(DbPath) -> atom_string(DbPath, DbPathStr) ; DbPathStr = DbPath),

        % Verify file exists and is accessible
        (exists_file(DbPathStr) ->
            (access_file(DbPathStr, write) ->
                (validate_database(DbPathStr) ->
                    % Execute with parameters
                    sqlite3_exec_params(DbPathStr, SQL, Params),
                    Result = ok(executed(SQL))
                ;
                    Result = error(db_error(invalid_database(DbPath)))
                )
            ;
                Result = error(db_error(database_not_writable(DbPath)))
            )
        ;
            Result = error(db_error(database_file_not_found(DbPath)))
        )
    ))
).

% Query database and return results
register_spell(
    perceive(db(query)),
    input(db(query(database('DbPath'), sql('SQL')))),
    output(either(
        ok(query_results('Results')),
        error(query_error('Reason'))
    )),
    "Execute a SQL query and return the results. Use for SELECT statements.",
    [],
    implementation(perceive(db(query(database(DbPath), sql(SQL)))), Result, (
        % Validation
        string(SQL),
        (atom(DbPath) -> atom_string(DbPath, DbPathStr) ; DbPathStr = DbPath),

        % Verify file exists and is readable
        (exists_file(DbPathStr) ->
            (access_file(DbPathStr, read) ->
                (validate_database(DbPathStr) ->
                    % Query
                    sqlite3_query(DbPathStr, SQL, QueryResults),
                    Result = ok(query_results(QueryResults))
                ;
                    Result = error(query_error(invalid_database(DbPath)))
                )
            ;
                Result = error(query_error(database_not_readable(DbPath)))
            )
        ;
            Result = error(query_error(database_file_not_found(DbPath)))
        )
    ))
).

% Query database with parameters (SQL injection safe)
register_spell(
    perceive(db(query_params)),
    input(db(query_params(database('DbPath'), sql('SQL'), params('Params')))),
    output(either(
        ok(query_results('Results')),
        error(query_error('Reason'))
    )),
    "Execute a parameterized SQL query. Use ? placeholders for parameters (SQL injection safe).",
    [],
    implementation(perceive(db(query_params(database(DbPath), sql(SQL), params(Params)))), Result, (
        % Validation
        string(SQL),
        is_list(Params),
        (atom(DbPath) -> atom_string(DbPath, DbPathStr) ; DbPathStr = DbPath),

        % Verify file exists and is readable
        (exists_file(DbPathStr) ->
            (access_file(DbPathStr, read) ->
                (validate_database(DbPathStr) ->
                    % Query with parameters
                    sqlite3_query_params(DbPathStr, SQL, Params, QueryResults),
                    Result = ok(query_results(QueryResults))
                ;
                    Result = error(query_error(invalid_database(DbPath)))
                )
            ;
                Result = error(query_error(database_not_readable(DbPath)))
            )
        ;
            Result = error(query_error(database_file_not_found(DbPath)))
        )
    ))
).

% Get list of tables in database
register_spell(
    perceive(db(tables)),
    input(db(tables(database('DbPath')))),
    output(either(
        ok(tables('TableList')),
        error(tables_error('Reason'))
    )),
    "Get a list of all tables in the specified database.",
    [],
    implementation(perceive(db(tables(database(DbPath)))), Result, (
        % Validation
        (atom(DbPath) -> atom_string(DbPath, DbPathStr) ; DbPathStr = DbPath),

        % Verify file exists and is readable
        (exists_file(DbPathStr) ->
            (access_file(DbPathStr, read) ->
                (validate_database(DbPathStr) ->
                    % Get tables
                    get_database_tables(DbPathStr, TableList),
                    Result = ok(tables(TableList))
                ;
                    Result = error(tables_error(invalid_database(DbPath)))
                )
            ;
                Result = error(tables_error(database_not_readable(DbPath)))
            )
        ;
            Result = error(tables_error(database_file_not_found(DbPath)))
        )
    ))
).

% Write table data - rows are Prolog compound terms
register_spell(
    conjure(db(write_table)),
    input(db(write_table(database('DbPath'), table('TableName'), rows('Rows')))),
    output(either(
        ok(written(count('Count'))),
        error(write_error('Reason'))
    )),
    "Write rows (as Prolog compound terms) to a database table. Creates table if needed.",
    [],
    implementation(conjure(db(write_table(database(DbPath), table(TableName), rows(Rows)))), Result, (
        catch(
            (write_rows_to_table(DbPath, TableName, Rows, Count),
             Result = ok(written(count(Count)))),
            Error,
            Result = error(write_error(Error))
        )
    ))
).

% Read table data - returns rows as Prolog compound terms
register_spell(
    perceive(db(read_table)),
    input(db(read_table(database('DbPath'), table('TableName')))),
    output(either(
        ok(rows('Rows')),
        error(read_error('Reason'))
    )),
    "Read rows from a database table as Prolog compound terms.",
    [],
    implementation(perceive(db(read_table(database(DbPath), table(TableName)))), Result, (
        catch(
            (read_rows_from_table(DbPath, TableName, Rows),
             Result = ok(rows(Rows))),
            Error,
            Result = error(read_error(Error))
        )
    ))
).

% === HELPER PREDICATES ===

% Process schema specification (file or SQL string)
process_schema_spec(file(SchemaFile), DbPathStr, SchemaFile) :-
    !,
    (atom(SchemaFile) -> atom_string(SchemaFile, SchemaFileStr) ; SchemaFileStr = SchemaFile),
    exists_file(SchemaFileStr),
    access_file(SchemaFileStr, read),
    sqlite3_init_db(DbPathStr, SchemaFileStr).

process_schema_spec(sql(SchemaSQL), DbPathStr, GeneratedSchemaFile) :-
    !,
    (atom(SchemaSQL) -> atom_string(SchemaSQL, SchemaSQLStr) ; SchemaSQLStr = SchemaSQL),
    (sub_string(SchemaSQLStr, _, _, _, "CREATE") ; sub_string(SchemaSQLStr, _, _, _, "create")),
    % Extract directory and base filename
    file_directory_name(DbPathStr, DbDir),
    file_base_name(DbPathStr, DbFile),
    file_name_extension(DbBaseName, db, DbFile),
    % Generate schema file path in same directory
    atomic_list_concat([DbDir, '/', DbBaseName, '.schema.sql'], GeneratedSchemaFile),
    open(GeneratedSchemaFile, write, Stream),
    write(Stream, SchemaSQLStr),
    close(Stream),
    exists_file(GeneratedSchemaFile),
    access_file(GeneratedSchemaFile, read),
    sqlite3_init_db(DbPathStr, GeneratedSchemaFile).

process_schema_spec(SchemaSpec, _, _) :-
    throw(error(db_error(invalid_schema_spec(SchemaSpec)))).

% Write rows to table (rows are Prolog compound terms)
write_rows_to_table(DbPath, TableName, Rows, Count) :-
    % Validate inputs
    (atom(DbPath) -> atom_string(DbPath, DbPathStr) ; DbPathStr = DbPath),
    atom(TableName),
    is_list(Rows),

    % Verify file exists and is writable
    (exists_file(DbPathStr) -> true ; throw(error(database_file_not_found(DbPath)))),
    (access_file(DbPathStr, write) -> true ; throw(error(database_not_writable(DbPath)))),
    (validate_database(DbPathStr) -> true ; throw(error(invalid_database(DbPath)))),

    % Get first row to determine structure
    (Rows = [FirstRow|_] ->
        FirstRow =.. [_Functor|Args],
        length(Args, Arity),

        % Create table if it doesn't exist (generic structure based on arity)
        % Columns: col1, col2, col3, etc. all TEXT
        create_generic_table(DbPathStr, TableName, Arity),

        % Clear existing data
        format(atom(ClearSQL), 'DELETE FROM ~w', [TableName]),
        sqlite3_exec(DbPathStr, ClearSQL),

        % Insert rows
        forall(
            member(Row, Rows),
            insert_row(DbPathStr, TableName, Row)
        ),

        length(Rows, Count)
    ;
        % No rows to write
        Count = 0
    ).

% Create generic table with N TEXT columns
create_generic_table(DbPath, TableName, Arity) :-
    % Generate column definitions: col1 TEXT, col2 TEXT, etc.
    findall(ColDef,
        (between(1, Arity, I),
         format(atom(ColDef), 'col~w TEXT', [I])),
        ColDefs),
    atomic_list_concat(ColDefs, ', ', ColDefsStr),
    format(atom(CreateSQL), 'CREATE TABLE IF NOT EXISTS ~w (~w)', [TableName, ColDefsStr]),
    sqlite3_exec(DbPath, CreateSQL).

% Insert a single row
insert_row(DbPath, TableName, Row) :-
    Row =.. [_Functor|Args],
    % Convert args to escaped SQL values
    maplist(term_to_sql_value, Args, SQLValues),
    atomic_list_concat(SQLValues, ', ', ValuesStr),
    format(atom(InsertSQL), 'INSERT INTO ~w VALUES (~w)', [TableName, ValuesStr]),
    sqlite3_exec(DbPath, InsertSQL).

% Convert Prolog term to SQL value with type preservation
% Format: Type(Value) stored as string
term_to_sql_value(Term, SQLValue) :-
    (atom(Term) ->
        TypedTerm = atom(Term)
    ; string(Term) ->
        TypedTerm = string(Term)
    ; number(Term) ->
        TypedTerm = number(Term)
    ; is_list(Term) ->
        TypedTerm = list(Term)
    ; compound(Term) ->
        TypedTerm = compound(Term)
    ;
        TypedTerm = other(Term)
    ),
    % Serialize to string representation
    term_string(TypedTerm, TypedStr),
    escape_sql_string_value(TypedStr, Escaped),
    format(atom(SQLValue), '\'~w\'', [Escaped]).

% Escape SQL string by doubling single quotes
escape_sql_string_value(Input, Escaped) :-
    (atom(Input) -> atom_string(Input, InputStr) ; InputStr = Input),
    split_string(InputStr, "'", "", Parts),
    atomics_to_string(Parts, "''", Escaped).

% Read rows from table as Prolog compound terms
read_rows_from_table(DbPath, TableName, Rows) :-
    % Validate inputs
    (atom(DbPath) -> atom_string(DbPath, DbPathStr) ; DbPathStr = DbPath),
    atom(TableName),

    % Verify file exists and is readable
    (exists_file(DbPathStr) -> true ; throw(error(database_file_not_found(DbPath)))),
    (access_file(DbPathStr, read) -> true ; throw(error(database_not_readable(DbPath)))),
    (validate_database(DbPathStr) -> true ; throw(error(invalid_database(DbPath)))),

    % Query all rows
    format(atom(SelectSQL), 'SELECT * FROM ~w', [TableName]),
    sqlite3_query(DbPathStr, SelectSQL, DictRows),

    % Convert dict rows to compound terms
    maplist(dict_row_to_compound(TableName), DictRows, Rows).

% Convert dict row to compound term
% For a table, we extract values in column order and create row(Val1, Val2, ...)
dict_row_to_compound(TableName, DictRow, CompoundRow) :-
    % Get column names in order from the dict
    dict_pairs(DictRow, _, Pairs),
    pairs_values(Pairs, TypedValues),
    % Deserialize typed values back to original terms
    maplist(sql_value_to_term, TypedValues, Values),
    % Create compound term with table name as functor
    CompoundRow =.. [TableName|Values].

% Deserialize SQL value back to original Prolog term with type
sql_value_to_term(SQLValue, Term) :-
    % Parse the typed term string
    term_string(TypedTerm, SQLValue),
    % Extract value from Type(Value) wrapper
    (TypedTerm = atom(V) -> Term = V
    ; TypedTerm = string(V) -> Term = V
    ; TypedTerm = number(V) -> Term = V
    ; TypedTerm = list(V) -> Term = V
    ; TypedTerm = compound(V) -> Term = V
    ; TypedTerm = other(V) -> Term = V
    ; Term = SQLValue  % Fallback if not wrapped
    ).
