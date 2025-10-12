% Interface layer for ECS exploration and system interaction
% Returns structured data - frontends (CLI/API/MCP) handle formatting

% Interface entity - now a folder entity
:- self_entity(interface).

% Load interface API submodule
:- load_entity(semantic(folder("./api"))).

% Interface subcommands (following git pattern)
component(interface, subcommand, compt).
component(interface, subcommand, comp).
component(interface, subcommand, doc).
component(interface, subcommand, entities).
component(interface, subcommand, repl).
component(interface, subcommand, test).
component(interface, subcommand, conjure).
component(interface, subcommand, perceive).
component(interface, subcommand, read_file).
component(interface, subcommand, edit_file).
component(interface, subcommand, exec).

% Removed legacy command constructor - interface functions called directly by CLI

% Also make them available as interface constructors
component(interface, ctor, C) :- component(interface, subcommand, C).

% Namespaced entities for each interface command
entity(interface(compt)).
entity(interface(comp)).
entity(interface(doc)).
entity(interface(entities)).
entity(interface(repl)).
entity(interface(test)).
entity(interface(test_files)).
entity(interface(conjure)).
entity(interface(perceive)).
entity(interface(read_file)).
entity(interface(edit_file)).
entity(interface(exec)).

% Docstrings follow namespacing pattern with detailed format information
docstring(interface(compt), "List all component types of current entity. Format: interface(compt) or interface(compt(Entity)). Returns component_types(Entity, [Type1, Type2, ...]).").
docstring(interface(comp), "List components of specific type for current entity. Format: interface(comp(Entity, ComponentType)). Returns components(Entity, ComponentType, [Component1, Component2, ...]).").
docstring(interface(doc), "Show docstring of current entity. Format: interface(doc) or interface(doc(Entity)). Returns documentation(Entity, DocString).").
docstring(interface(entities), "List all entities in the system. Format: interface(entities). Returns entities([Entity1, Entity2, ...]).").
docstring(interface(repl), "Start interactive REPL with context awareness. Format: interface(repl). Interactive command that starts a Prolog REPL.").
docstring(interface(test), "Run the test suite. Format: interface(test) or interface(test([TestName1, TestName2, ...]). Returns test results.").
docstring(interface(test_files), "Run tests from specific files. Format: interface(test_files(TestNames, FilePaths)). Returns test results.").
docstring(interface(conjure), "Execute conjuration spells (mutable operations). Format: interface(conjure(SpellTerm)) - use 'grimoire comp conjure ctor' to see available spells.").
docstring(interface(perceive), "Execute perception spells (query operations). Format: interface(perceive(QueryTerm)) - use 'grimoire comp perceive ctor' to see available queries.").
docstring(interface(read_file), "Read lines from a file using 1-based indexing. Format: interface(read_file(FilePath, Start, End)). Returns file content with line numbers.").
docstring(interface(edit_file), "Edit file with specified operations. Format: interface(edit_file(FilePath, Edits)). Edits is a list of edit operations.").
docstring(interface(exec), "Execute arbitrary Prolog query with variable bindings. Format: interface(exec(QueryStr)). Returns solutions with variable bindings.").

% Main interface docstring
docstring(interface, S) :-
    make_ctors_docstring(interface, CtorsDoc),
    S = {|string(CtorsDoc)||\n    Interface commands for ECS exploration and system interaction.\n    Format: interface(subcommand(...))\n\n    Available subcommands:\n    {CtorsDoc}\n    |}.

% === DSL PATTERNS: CLIENT CAPABILITIES ===

% Expand client capabilities to queryable components
component(Entity, interface_client_type, Type) :-
    component(Entity, has(interface(client)), interface(client(Spec))),
    member(type(Type), Spec).

component(Entity, interface_client_capability, Cap) :-
    component(Entity, has(interface(client)), interface(client(Spec))),
    member(capabilities(Caps), Spec),
    member(Cap, Caps).

% Expand spell access permissions
component(Entity, interface_can_access_domain, Domain) :-
    component(Entity, has(interface(spell_access)), interface(spell_access(Spec))),
    member(domain(Domain), Spec).

component(Entity, interface_can_cast_spell, Spell) :-
    component(Entity, has(interface(spell_access)), interface(spell_access(Spec))),
    member(spells(Spells), Spec),
    member(Spell, Spells).

% Subcommand availability
component(interface, available_subcommands, Subcommands) :-
    findall(Cmd, component(interface, subcommand, Cmd), Subcommands).

% === CONTEXT MANAGEMENT ===

% Auto-detect current working context
current_entity(Entity) :-
    (exists_file('./semantics.pl') ->
        % Get entity name from working directory
        working_directory(Cwd, Cwd),
        file_base_name(Cwd, DirName),
        atom_string(Entity, DirName),
        % Ensure local project is loaded
        ensure_local_project_loaded(Entity)
    ;
        % Default to system entity
        Entity = system
    ).

% Resolve entity paths with shortcuts
resolve_entity_path(PathStr, Entity) :-
    (PathStr = "/" ->
        Entity = system
    ;
        % Try loading as semantic folder (works for both "." and "path/to/something")
        catch(
            (load_entity(semantic(folder(PathStr))),
             file_base_name(PathStr, DirName),
             atom_string(Entity, DirName)),
            _,
            % If loading fails, treat as entity name directly
            atom_string(Entity, PathStr)
        )
    ).

% Load local semantics.pl if not already loaded
ensure_local_project_loaded(ProjectEntity) :-
    (entity(ProjectEntity) ->
        true  % Already loaded
    ;
        load_entity(semantic(file('./semantics.pl')))
    ).


% === INTERFACE COMMAND IMPLEMENTATIONS ===

% Component types listing
register_spell(conjure(interface(compt)), input(interface(compt)), output(ok(component_types('Entity', 'TypeList'))), docstring("Query component types")).
cast(conjure(interface(compt)), RetVal) :-
    current_entity(Entity),
    interface_compt(Entity, Types),
    RetVal = ok(component_types(Entity, Types)).

cast(conjure(interface(compt(EntityPath))), RetVal) :-
    resolve_entity_path(EntityPath, Entity),
    interface_compt(Entity, Types),
    RetVal = ok(component_types(Entity, Types)).

% Component listing with new argument order: entity first, then type
register_spell(conjure(interface(comp('E','T'))), input(interface(comp('E','T'))), output(ok(components('E','T','List'))), docstring("Query components")).
cast(conjure(interface(comp(EntityPath, Type))), RetVal) :-
    resolve_entity_path(EntityPath, Entity),
    interface_comp(Entity, Type, Components),
    RetVal = ok(components(Entity, Type, Components)).

% Documentation retrieval
register_spell(conjure(interface(doc)), input(interface(doc)), output(ok(documentation('Entity', 'Doc'))), docstring("Get documentation")).
cast(conjure(interface(doc)), RetVal) :-
    current_entity(Entity),
    interface_doc(Entity, Doc),
    RetVal = ok(documentation(Entity, Doc)).

cast(conjure(interface(doc(Entity))), RetVal) :-
    interface_doc(Entity, Doc),
    RetVal = ok(documentation(Entity, Doc)).

% Entities listing
register_spell(conjure(interface(entities)), input(interface(entities)), output(ok(entities('EntityList'))), docstring("List all entities")).
cast(conjure(interface(entities)), RetVal) :-
    interface_entities(Entities),
    RetVal = ok(entities(Entities)).

% REPL command - delegate to existing implementation
register_spell(
    conjure(interface(repl)),
    input(interface(repl)),
    output(either(ok(repl_completed), error(repl_failed('Error')))),
    docstring("Start interactive REPL with context awareness")
).
cast(conjure(interface(repl)), RetVal) :-
    % Load and call existing REPL functionality
    catch(
        (grimoire_ensure_loaded('@/src/repl.pl'), grimoire_repl_command, RetVal = ok(repl_completed))
    ,
        Error,
        RetVal = error(repl_failed(Error))
    ).

% Test command - delegate to existing implementation
register_spell(
    conjure(interface(test)),
    input(interface(test)),
    output(either(ok(tests_passed), error(tests_failed('Reason')))),
    docstring("Run all tests")
).
cast(conjure(interface(test)), RetVal) :-
    catch(
        (grimoire_ensure_loaded('@/src/run_tests.pl'), run_all_tests, RetVal = ok(tests_passed))
    ,
        Error,
        RetVal = error(tests_failed(Error))
    ).

% Test command with specific test arguments
register_spell(
    conjure(interface(test('TestArgs'))),
    input(interface(test('TestArgs'))),
    output(either(ok(tests_passed), error(tests_failed('Reason')))),
    docstring("Run tests with optional filtering arguments")
).
cast(conjure(interface(test(TestArgs))), RetVal) :-
    catch(
        (grimoire_ensure_loaded('@/src/run_tests.pl'),
         (member('--list', TestArgs) ->
             (list_available_tests,
              RetVal = ok(tests_listed))
         ;
             (run_specific_tests(TestArgs),
              RetVal = ok(tests_passed))
         ))
    ,
        Error,
        RetVal = error(tests_failed(Error))
    ).

% Test files command - run tests from specific .plt files
register_spell(
    conjure(interface(test_files('TestNames', 'FilePaths'))),
    input(interface(test_files('TestNames', 'FilePaths'))),
    output(either(ok(tests_passed), error(tests_failed('Reason')))),
    docstring("Run tests from specific .plt files")
).
cast(conjure(interface(test_files(TestNames, FilePaths))), RetVal) :-
    catch(
        (grimoire_ensure_loaded('@/src/run_tests.pl'),
         run_test_files(TestNames, FilePaths),
         RetVal = ok(tests_passed))
    ,
        Error,
        RetVal = error(tests_failed(Error))
    ).

% Conjure command - execute conjuration spells
register_spell(
    conjure(interface(conjure('SpellTerm'))),
    input(interface(conjure('SpellTerm'))),
    output('DomainDependentResult'),
    docstring("Delegate conjure spell to appropriate domain")
).
cast(conjure(interface(conjure(SpellTerm))), RetVal) :-
    magic_cast(conjure(SpellTerm), RetVal).

% Perceive command - execute perception spells directly
register_spell(
    perceive(interface(perceive('QueryTerm'))),
    input(interface(perceive('QueryTerm'))),
    output(either(ok(query_succeeded), error(query_failed))),
    docstring("Delegate perceive query to appropriate domain")
).
cast(conjure(interface(perceive(QueryTerm))), RetVal) :-
    magic_cast(perceive(QueryTerm), Result),
    (Result = ok(_) ->
        RetVal = ok(query_succeeded)
    ;
        RetVal = error(query_failed)
    ).

% Read file command - delegate to fs domain
register_spell(
    conjure(interface(read_file('FilePath', 'Start', 'End'))),
    input(interface(read_file('FilePath', 'Start', 'End'))),
    output(either(ok(lines('ContentWithLineNumbers')), error(read_file_failed))),
    docstring("Read lines from a file with 1-based indexing (delegated to fs domain)")
).
cast(conjure(interface(read_file(FilePath, Start, End))), RetVal) :-
    magic_cast(perceive(fs(read_file(FilePath, Start, End))), RetVal).

% Edit file command - delegate to fs domain
register_spell(
    conjure(interface(edit_file('FilePath', 'Edits'))),
    input(interface(edit_file('FilePath', 'Edits'))),
    output('FsDomainResult'),
    docstring("Edit file with specified operations (delegated to fs domain)")
).
cast(conjure(interface(edit_file(FilePath, Edits))), RetVal) :-
    magic_cast(conjure(fs(edit_file(file(FilePath), Edits))), RetVal).

% === CORE INTERFACE FUNCTIONS ===
% These return structured data, no printing

% List component types for entity
interface_compt(Entity, Types) :-
    findall(Type, component(Entity, Type, _), AllTypes),
    sort(AllTypes, Types).

% List components of specific type with entity flags
interface_comp(Entity, Type, ComponentsWithFlags) :-
    findall(comp_entry(Comp, Flag), (
        component(Entity, Type, Comp),
        (entity(Comp) -> Flag = entity ; Flag = value)
    ), ComponentsWithFlags).

% Get entity documentation
interface_doc(Entity, Doc) :-
    docstring(Entity, Doc).

% List all entities
interface_entities(Entities) :-
    findall(E, entity(E), Entities).


% === PYTHON INTERFACE SUPPORT ===

% Python-specific cast that converts Prolog terms to Python-friendly dictionaries
python_cast(conjure(ConjureStruct), PyResult) :-
    % If it's an atom, try to convert it to a term
    % This will preserve quoted atoms as atoms (e.g., 'interface(doc)' stays as atom)
    % and convert unquoted compound terms (e.g., interface(doc)) to terms
    (   atom(ConjureStruct) ->
        atom_to_term(ConjureStruct, Term, [])
    ;
        Term = ConjureStruct  % Already a term
    ),
    cast(conjure(Term), Result),
    term_struct_to_python_dict(Result, PyResult).

% Convert Prolog term structures to Python dictionaries recursively
term_struct_to_python_dict(Term, Dict) :-
    % Handle primitive types that pass through directly
    (   atomic(Term) ->
        (   atom(Term) ->
            Dict = _{type: "atom", value: Term}
        ;   string(Term) ->
            Dict = _{type: "string", value: Term}
        ;   number(Term) ->
            (   integer(Term) ->
                Dict = _{type: "int", value: Term}
            ;   Dict = _{type: "float", value: Term}
            )
        )
    ;   is_list(Term) ->
        % Convert list elements recursively
        maplist(term_struct_to_python_dict, Term, Elements),
        Dict = _{type: "list", elements: Elements}
    ;   compound(Term) ->
        % Handle compound terms by decomposing with functor and args
        compound_name_arity(Term, Functor, Arity),
        Term =.. [Functor|Args],
        maplist(term_struct_to_python_dict, Args, ConvertedArgs),
        Dict = _{type: "term_struct", functor: Functor, arity: Arity, args: ConvertedArgs}
    ;   % Fallback for any other types
        Dict = _{type: "unknown", value: Term}
    ).

% Execute arbitrary Prolog query from Python and collect solutions
% Returns list of variable binding dictionaries
python_exec_query(QueryStr, PyDictSolutions) :-
    read_term_from_atom(QueryStr, QueryTerm, [variable_names(VarNames)]),
    findall(VarNames, QueryTerm, Solutions),
    maplist(varnames_to_pydict, Solutions, PyDictSolutions).

% Convert a list of Name=Value pairs to a Python dictionary
varnames_to_pydict(VarNames, PyDict) :-
    varnames_to_dict_list(VarNames, DictList),
    dict_create(PyDict, _, DictList).

% Convert Name=Value pairs to Key-Value list for dict_create
varnames_to_dict_list([], []).
varnames_to_dict_list([Name=Value|Rest], [Name-ConvertedValue|RestDict]) :-
    term_struct_to_python_dict(Value, ConvertedValue),
    varnames_to_dict_list(Rest, RestDict).
