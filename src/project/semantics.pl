% Core entities
:- self_entity(project, {|string(_)||
    Project domain for composing git + nix + fs into development environments.
    Manages project creation, validation, and cross-domain composition.
    |}).

entity(mkproject).
entity(conjure(mkproject)).
entity(project(app)).
entity(project(context(build))).
entity(project(context(runtime))).
entity(project(context(test))).

% === PHASE 3: COMPONENT DSL WITH ==> AND :: OPERATORS ===

% Project app DSL expansion with conditional components
component(Entity, has(project(app)), project(app(Options)))
    ==> (component(Entity, project_type, Type) :- member(type(Type), Options)),
        (component(Entity, project_git_origin, Origin) :- member(git(repository(origin(Origin))), Options)),
        (component(Entity, project_nix_flake, nix(flake(Ref))) :- member(nix(flake(ref(Ref))), Options)),
        (component(Entity, project_fs_structure, Structure) :- member(fs(structure(Structure)), Options))
    ::  is_list(Options).

% Project context build DSL expansion
component(Entity, has(project(context(build))), project(context(build(Options))))
    ==> (component(Entity, project_context_outputs, Outputs) :- member(outputs(Outputs), Options))
    ::  is_list(Options).

% === REVERSE BRIDGE RULES (Pattern 3: KB → KB expansion) ===
% These trigger OTHER domain schemas from project components

% Project git origin triggers git repository component
component(Entity, project_git_origin, Origin)
    ==> component(Entity, has(git(repository)), git(repository(origin(Origin)))).

% Project nix flake triggers nix flake component
component(Entity, project_nix_flake, NixFlake)
    ==> component(Entity, has(nix(flake)), NixFlake).

% Project fs structure triggers fs structure component
component(Entity, project_fs_structure, Patterns)
    ==> component(Entity, has(fs(structure)), fs(structure(Patterns))).

% === LEAF COMPONENT VERIFICATIONS USING :: OPERATOR ===

% Verify project type is valid
component(_, project_type, Type)
    :: atom(Type),
       member(Type, [web_service, cli_tool, library, package]).

% Verify git origin is valid URL or path
component(_, project_git_origin, Origin)
    :: (atom(Origin) ; string(Origin)).

% Verify fs structure is valid
component(_, project_fs_structure, Structure)
    :: is_list(Structure).

% Verify context outputs is valid
component(_, project_context_outputs, Outputs)
    :: is_list(Outputs).

% === PHASE 3: SPELL REGISTRATIONS WITH INLINE IMPLEMENTATIONS ===

% Create new project from template or basic structure
register_spell(
    conjure(mkproject),
    input(mkproject(folder_path('FolderPath'), project_name('ProjectName'), options('Options'))),
    output(either(
        ok(project_created(path('ProjectPath'), name('ProjectName'))),
        error(project_error('Reason'))
    )),
    "Creates a new project directory with full initialization. Options: git(bool), template(TemplateId)",
    [],
    implementation(conjure(mkproject(FolderPath, ProjectName, Options)), RetVal, (
        catch(
            mkproject_impl(FolderPath, ProjectName, Options, RetVal),
            Error,
            RetVal = error(conjure_failed(Error))
        )
    ))
).

% Validate project entity composition
register_spell(
    perceive(project(validate)),
    input(project(validate(entity('Entity')))),
    output(either(
        ok(valid),
        error(validation_failed(domain('Domain'), reason('Reason')))
    )),
    "Validates a project entity against its declared components and cross-domain requirements",
    [],
    implementation(perceive(project(validate(Entity))), Result, (
        catch(
            (please_verify(component(Entity, has(project(app)), _)),
             please_verify(component(Entity, project_type, _)),
             % Optionally verify git if specified
             (component(Entity, project_git_origin, _) ->
                 please_verify(component(Entity, has(git(repository)), _))
             ; true),
             % Optionally verify nix if specified
             (component(Entity, project_nix_flake, _) ->
                 please_verify(component(Entity, has(nix(flake)), _))
             ; true),
             Result = ok(valid)),
            verification_error(Domain, Reason),
            Result = error(validation_failed(Domain, Reason))
        )
    ))
).

% Query project structure information
register_spell(
    perceive(project(structure)),
    input(project(structure(entity('Entity')))),
    output(ok(project_info(
        type('Type'),
        sources('Sources'),
        contexts('Contexts')
    ))),
    "Queries project structure, including type, source patterns, and available contexts",
    [],
    implementation(perceive(project(structure(Entity))), Result, (
        catch(
            (please_verify(component(Entity, project_type, Type)),
             findall(Source, component(Entity, project_fs_structure, Source), Sources),
             findall(Context, component(Entity, has(project(context(Context))), _), Contexts),
             Result = ok(project_info(type(Type), sources(Sources), contexts(Contexts)))),
            Error,
            Result = error(structure_query_failed(Error))
        )
    ))
).

% === HELPER PREDICATES ===

% Main mkproject implementation
mkproject_impl(FolderPath, ProjectName, Options, RetVal) :-
    % Create full project path
    directory_file_path(FolderPath, ProjectName, ProjectPath),

    % Ensure base folder exists
    ensure_base_folder(FolderPath),

    % Check if project already exists
    (exists_directory(ProjectPath) ->
        RetVal = error(project_already_exists(ProjectPath))
    ;
        create_project_structure(ProjectPath, ProjectName, Options, RetVal)
    ).

ensure_base_folder(FolderPath) :-
    (exists_directory(FolderPath) ->
        true
    ;
        make_directory_path(FolderPath)
    ).

create_project_structure(ProjectPath, ProjectName, Options, RetVal) :-
    make_directory(ProjectPath),
    apply_template_or_basic(ProjectPath, ProjectName, Options, RenameResult),
    (RenameResult = ok ->
        init_git_if_requested(ProjectPath, Options, RetVal, ProjectName)
    ;
        RetVal = error(entity_renaming_failed(RenameResult))
    ).

apply_template_or_basic(ProjectPath, ProjectName, Options, Result) :-
    (member(template(Template), Options) ->
        magic_cast(conjure(nix(flake(new(template_id(Template), dest_path(ProjectPath))))), TemplateResult),
        (TemplateResult = ok(_) ->
            rename_project_entities(ProjectPath, Template, ProjectName, Result)
        ;
            Result = TemplateResult
        )
    ;
        create_basic_semantics_file(ProjectPath, ProjectName),
        Result = ok
    ).

init_git_if_requested(ProjectPath, Options, RetVal, ProjectName) :-
    (member(git(false), Options) ->
        load_into_session_if_requested(ProjectPath, Options, RetVal, ProjectName)
    ;
        magic_cast(conjure(git(init(path(ProjectPath)))), GitResult),
        (GitResult = ok(_) ->
            load_into_session_if_requested(ProjectPath, Options, RetVal, ProjectName)
        ;
            RetVal = GitResult
        )
    ).

load_into_session_if_requested(ProjectPath, Options, RetVal, ProjectName) :-
    (member(load_into_session(true), Options) ->
        % Load project semantics into session
        directory_file_path(ProjectPath, 'semantics.pl', SemanticsFile),
        (exists_file(SemanticsFile) ->
            magic_cast(conjure(session(load_entity(semantic(file(SemanticsFile))))), LoadResult),
            (LoadResult = ok(_) ->
                RetVal = ok(project_created(ProjectPath, ProjectName))
            ;
                RetVal = error(failed_to_load_into_session(LoadResult))
            )
        ;
            % No semantics file to load
            RetVal = ok(project_created(ProjectPath, ProjectName))
        )
    ;
        % Not requested, just return success
        RetVal = ok(project_created(ProjectPath, ProjectName))
    ).

% === HELPER PREDICATES ===

% Create basic semantics.pl file when no template is used
create_basic_semantics_file(ProjectPath, ProjectName) :-
    directory_file_path(ProjectPath, 'semantics.pl', SemanticsFile),
    open(SemanticsFile, write, Stream),
    format(Stream, ':- self_entity(~w).~n~n', [ProjectName]),
    format(Stream, 'docstring(~w,~n    {|string(_)||~n    ~w project entity.~n    |}).~n', [ProjectName, ProjectName]),
    close(Stream).

% Entity renaming in generated project files
rename_project_entities(ProjectPath, TemplateId, ProjectName, Result) :-
    % Discover the actual entity name from the generated semantics.pl file
    directory_file_path(ProjectPath, 'semantics.pl', SemanticsFile),
    (exists_file(SemanticsFile) ->
        discover_template_entity_name(SemanticsFile, TemplateEntityName)
    ;
        % Fallback: assume template_id naming pattern
        format(atom(TemplateEntityName), '~w_template', [TemplateId])
    ),

    % Find semantics.pl and semantics.plt files in project directory
    findall(File, (
        member(FileName, ['semantics.pl', 'semantics.plt']),
        directory_file_path(ProjectPath, FileName, File),
        exists_file(File)
    ), SemanticsFiles),

    % Rename entities in each file
    maplist(rename_entities_in_file(TemplateEntityName, ProjectName), SemanticsFiles, Results),

    % Check if all renamings succeeded
    (member(error(_), Results) ->
        Result = error(Results)
    ;
        Result = ok
    ).

% Rename entities in a single file
rename_entities_in_file(OldEntityName, NewEntityName, FilePath, Result) :-
    catch(
        (
            % Read the entire file
            read_file_to_string(FilePath, Content, []),

            % Perform comprehensive entity renaming
            rename_all_entity_occurrences(Content, OldEntityName, NewEntityName, NewContent),

            % Write back to file
            open(FilePath, write, Stream),
            write(Stream, NewContent),
            close(Stream),

            Result = ok
        ),
        Error,
        Result = error(Error)
    ).

% Comprehensive entity renaming in content
rename_all_entity_occurrences(Content, OldEntity, NewEntity, NewContent) :-
    % Create atom names for replacement
    atom_string(OldEntityAtom, OldEntity),
    atom_string(NewEntityAtom, NewEntity),

    % Replace various patterns:
    % 1. :- self_entity(OldEntity).
    format(string(OldSelfEntity), ':- self_entity(~w).', [OldEntityAtom]),
    format(string(NewSelfEntity), ':- self_entity(~w).', [NewEntityAtom]),

    % 2. component(OldEntity, ...)
    format(string(OldComponentPattern), 'component(~w,', [OldEntityAtom]),
    format(string(NewComponentPattern), 'component(~w,', [NewEntityAtom]),

    % 3. entity(OldEntity)
    format(string(OldEntityPattern), 'entity(~w)', [OldEntityAtom]),
    format(string(NewEntityPattern), 'entity(~w)', [NewEntityAtom]),

    % 4. docstring(OldEntity, ...)
    format(string(OldDocPattern), 'docstring(~w,', [OldEntityAtom]),
    format(string(NewDocPattern), 'docstring(~w,', [NewEntityAtom]),

    % Apply all replacements using literal replacement
    re_replace(OldSelfEntity, NewSelfEntity, Content, Content1, [literal, global]),
    re_replace(OldComponentPattern, NewComponentPattern, Content1, Content2, [literal, global]),
    re_replace(OldEntityPattern, NewEntityPattern, Content2, Content3, [literal, global]),
    re_replace(OldDocPattern, NewDocPattern, Content3, NewContent, [literal, global]).

% Discover the entity name from a semantics.pl file
discover_template_entity_name(FilePath, EntityName) :-
    catch(
        (
            read_file_to_string(FilePath, Content, []),
            % Look for :- self_entity(EntityName).
            re_matchsub(':- self_entity\\(([^)]+)\\)\\.', Content, Match, []),
            get_dict(1, Match, EntityNameString),
            atom_string(EntityName, EntityNameString)
        ),
        _Error,
        fail
    ).

% Template discovery - query nix domain for available templates
discover_available_templates(Templates) :-
    findall(TemplateId,
            component(nix(flake(template)), instance, nix(flake(template(TemplateId)))),
            Templates).
