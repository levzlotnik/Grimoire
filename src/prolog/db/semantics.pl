
% DB initialization
init_agent_db(DbPath) :-
    component(system, root_dir, folder(SysRoot)),
    directory_file_path(SysRoot, "src/prolog/db/schema.sql", SchemaPath),
    run(command(shell(['sqlite3', DbPath, '<', SchemaPath])), RetVal),
    (RetVal = error(E) -> throw(db_error(E)) ; true).

% Entity and component declarations
:- self_entity(db).
component(db, source, source(semantic(folder("db")))) :- !.

docstring(init_agent_db,
    {|string(_)||
    Initialize agent database using schema.sql.
    Format: init_agent_db(+DbPath)
    |}).
