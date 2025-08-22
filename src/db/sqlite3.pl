% SQLite3 CLI wrapper predicates
% Pure interface to sqlite3 command line tool

:- use_module(library(http/json)).

% Execute SQL without expecting results
sqlite3_exec(DbPath, SQL) :-
    process_create(path(sqlite3), [DbPath, SQL], [stdout(null), stderr(pipe(ErrorStream)), process(PID)]),
    read_string(ErrorStream, _, ErrorString),
    close(ErrorStream),
    process_wait(PID, Status),
    (Status = exit(0) ->
        true
    ;
        (ErrorString = "" ->
            throw(sqlite_error(Status))
        ;
            throw(sqlite_error(ErrorString))
        )
    ).

% Execute SQL and parse JSON results  
sqlite3_query(DbPath, SQL, Results) :-
    process_create(path(sqlite3), [DbPath, '-json', SQL], [stdout(pipe(OutputStream)), stderr(pipe(ErrorStream)), process(PID)]),
    read_string(OutputStream, _, JsonString),
    read_string(ErrorStream, _, ErrorString),
    close(OutputStream),
    close(ErrorStream),
    process_wait(PID, Status),
    (Status = exit(0) ->
        (JsonString = "" ->
            Results = []
        ;
            open_string(JsonString, Stream),
            json_read_dict(Stream, Results),
            close(Stream)
        )
    ;
        (ErrorString = "" ->
            throw(sqlite_error(Status))
        ;
            throw(sqlite_error(ErrorString))
        )
    ).

% Initialize database with schema file
sqlite3_init_db(DbPath, SchemaFile) :-
    (exists_file(SchemaFile) ->
        open(SchemaFile, read, SchemaStream),
        read_string(SchemaStream, _, SchemaSQL),
        close(SchemaStream),
        sqlite3_exec(DbPath, SchemaSQL)
    ;
        throw(schema_file_not_found(SchemaFile))
    ).

% Escape SQL strings properly
escape_sql_string(Input, Escaped) :-
    atom_string(Input, InputStr),
    re_replace("'"/g, "''", InputStr, EscapedStr),
    atom_string(Escaped, EscapedStr).

% Check if database file exists and is valid SQLite
validate_database(DbPath) :-
    exists_file(DbPath),
    sqlite3_query(DbPath, "SELECT 1", [_|_]).

% Get database schema information
get_database_tables(DbPath, Tables) :-
    sqlite3_query(DbPath, 
        "SELECT name FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%'",
        TableResults),
    maplist(get_dict(name), TableResults, Tables).

% Get table column information
get_table_columns(DbPath, TableName, Columns) :-
    format(atom(SQL), 'PRAGMA table_info(~w)', [TableName]),
    sqlite3_query(DbPath, SQL, ColumnInfo),
    maplist(extract_column_info, ColumnInfo, Columns).

% Extract column information from PRAGMA table_info result
extract_column_info(ColumnDict, column(Name, Type, NotNull, DefaultValue, PrimaryKey)) :-
    get_dict(name, ColumnDict, Name),
    get_dict(type, ColumnDict, Type),
    get_dict(notnull, ColumnDict, NotNullInt),
    (NotNullInt = 1 -> NotNull = true ; NotNull = false),
    (get_dict(dflt_value, ColumnDict, Default) -> DefaultValue = Default ; DefaultValue = null),
    get_dict(pk, ColumnDict, PKInt),
    (PKInt = 1 -> PrimaryKey = true ; PrimaryKey = false).