% Test database for discovery tests
:- self_entity(test_discovery_db, "Test database entity for table and column discovery tests").

% Registered database - use relative paths that get resolved from this file's directory
registered_db(database(test_discovery), data(file(DbPath)), schema(file(SchemaPath))) :-
   component(test_discovery_db, self, semantic(folder(Dir))),
   directory_file_path(Dir, 'test_discovery.db', DbPath),
   directory_file_path(Dir, 'test_schema.sql', SchemaPath).
