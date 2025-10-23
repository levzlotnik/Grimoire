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
            throw(error(sqlite_error(Status), context(sqlite3_exec/2, DbPath)))
        ;
            throw(error(sqlite_error(ErrorString), context(sqlite3_exec/2, DbPath)))
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
            json_read_dict(Stream, RawResults, []),
            close(Stream),
            % Re-tag all dicts with 'row' tag to make them grounded
            retag_dicts(RawResults, row, Results)
        )
    ;
        (ErrorString = "" ->
            throw(error(sqlite_error(Status), context(sqlite3_query/3, DbPath)))
        ;
            throw(error(sqlite_error(ErrorString), context(sqlite3_query/3, DbPath)))
        )
    ).

% Get schema SQL from database
get_database_schema(DbPath, sql(SchemaSQL)) :-
    exists_file(DbPath),
    sqlite3_query(DbPath, "SELECT sql FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%' ORDER BY name", Results),
    findall(SQL, (member(Row, Results), get_dict(sql, Row, SQL), SQL \= null), SQLStatements),
    atomic_list_concat(SQLStatements, ';\n', SchemaSQL).

% Initialize database with schema file
sqlite3_init_db(DbPath, SchemaFile) :-
    (exists_file(SchemaFile) ->
        open(SchemaFile, read, SchemaStream),
        read_string(SchemaStream, _, SchemaSQL),
        close(SchemaStream),
        sqlite3_exec(DbPath, SchemaSQL)
    ;
        throw(error(schema_file_not_found(SchemaFile), context(sqlite3_init_db/2, DbPath)))
    ).

% Re-tag dicts to make them grounded
% retag_dicts(+Input, +Tag, -Output)
retag_dicts([], _, []).
retag_dicts([H|T], Tag, [RH|RT]) :-
    retag_dict_item(H, Tag, RH),
    retag_dicts(T, Tag, RT).
retag_dicts(Input, Tag, Output) :-
    \+ is_list(Input),
    retag_dict_item(Input, Tag, Output).

retag_dict_item(Dict, Tag, RetaggedDict) :-
    is_dict(Dict),
    !,
    dict_pairs(Dict, _, Pairs),
    dict_pairs(RetaggedDict, Tag, Pairs).
retag_dict_item(Other, _, Other).

% Escape SQL strings properly
escape_sql_string(Input, Escaped) :-
    atom_string(Input, InputStr),
    re_replace("'"/g, "''", InputStr, EscapedStr),
    atom_string(Escaped, EscapedStr).

% Execute parameterized SQL using SQLite's .param mechanism
sqlite3_exec_params(DbPath, SQL, Params) :-
    is_list(Params),
    build_param_script(SQL, Params, Script),
    process_create(path(sqlite3), [DbPath], [
        stdin(pipe(InStream)),
        stdout(null),
        stderr(pipe(ErrorStream)),
        process(PID)
    ]),
    write(InStream, Script),
    close(InStream),
    read_string(ErrorStream, _, ErrorString),
    close(ErrorStream),
    process_wait(PID, Status),
    (Status = exit(0) ->
        true
    ;
        (ErrorString = "" ->
            throw(error(sqlite_error(Status), context(sqlite3_exec_params/3, DbPath)))
        ;
            throw(error(sqlite_error(ErrorString), context(sqlite3_exec_params/3, DbPath)))
        )
    ).

% Query with parameterized SQL using SQLite's .param mechanism
sqlite3_query_params(DbPath, SQL, Params, Results) :-
    is_list(Params),
    build_param_script(SQL, Params, Script),
    process_create(path(sqlite3), [DbPath, '-json'], [
        stdin(pipe(InStream)),
        stdout(pipe(OutputStream)),
        stderr(pipe(ErrorStream)),
        process(PID)
    ]),
    write(InStream, Script),
    close(InStream),
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
            json_read_dict(Stream, RawResults, []),
            close(Stream),
            retag_dicts(RawResults, row, Results)
        )
    ;
        (ErrorString = "" ->
            throw(error(sqlite_error(Status), context(sqlite3_query_params/4, DbPath)))
        ;
            throw(error(sqlite_error(ErrorString), context(sqlite3_query_params/4, DbPath)))
        )
    ).

% Build SQLite script with .param set commands
% Converts ? placeholders to :p1, :p2, etc.
build_param_script(SQL, Params, Script) :-
    replace_placeholders(SQL, 1, Params, ParamSQL, ParamBindings),
    findall(ParamLine,
        member(binding(ParamLine, _), ParamBindings),
        ParamLines
    ),
    atomic_list_concat(ParamLines, '\n', ParamSection),
    format(string(Script), "~w\n~w;\n", [ParamSection, ParamSQL]).

% Replace ? with :p1, :p2, etc. and generate .param set commands
replace_placeholders(SQL, _, [], SQL, []) :- !.
replace_placeholders(SQL, N, [Param|RestParams], Result, [binding(ParamLine, Param)|RestBindings]) :-
    format(atom(ParamName), ':p~w', [N]),
    % Replace first ?
    (atom(SQL) -> atom_string(SQL, SQLStr) ; SQLStr = SQL),
    (sub_string(SQLStr, Before, 1, After, "?")
    -> (sub_string(SQLStr, 0, Before, _, Prefix),
        AfterPos is Before + 1,
        sub_string(SQLStr, AfterPos, After, 0, Suffix),
        atomic_list_concat([Prefix, ParamName, Suffix], NewSQL),
        % Generate .param set command
        param_to_dot_param(ParamName, Param, ParamLine),
        N1 is N + 1,
        replace_placeholders(NewSQL, N1, RestParams, Result, RestBindings))
    ; throw(error(param_count_mismatch,
                 context(build_param_script/3, 'More parameters than ? placeholders')))).

% Generate .param set command for a value
param_to_dot_param(ParamName, Param, ParamLine) :-
    (number(Param)
    -> format(string(ParamLine), ".param set ~w ~w", [ParamName, Param])
    ; Param = null
    -> format(string(ParamLine), ".param set ~w NULL", [ParamName])
    ; (% String/atom value - use double quotes and escape internal double quotes and newlines
       (atom(Param) -> atom_string(Param, ParamStr) ; ParamStr = Param),
       % Replace newlines with spaces (or could use \n escape)
       re_replace('\n'/g, ' ', ParamStr, NoNewlines),
       % Escape double quotes by doubling them
       re_replace('"'/g, '""', NoNewlines, EscapedStr),
       format(string(ParamLine), '.param set ~w "~s"', [ParamName, EscapedStr]))).

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