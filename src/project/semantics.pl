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
    :: type_check(list(fs_entry), Structure).

% Verify context outputs is valid
component(_, project_context_outputs, Outputs)
    :: is_list(Outputs).

% === PHASE 3: SPELL REGISTRATIONS WITH INLINE IMPLEMENTATIONS ===

% Create new project from template or basic structure
register_spell(
    conjure(mkproject),
    input(conjure(mkproject(folder_path(FolderPath:stringy), project_name(ProjectName:atom), options(Options:list(term))))),
    output(either(
        ok(project_created(path(ProjectPath:atom), name(ProjectName:atom))),
        error(project_error(Reason:term), Context:term)
    )),
    "Creates a new project directory with full initialization. Options: git(bool), template(TemplateId)",
    [],
    implementation(conjure(mkproject(folder_path(FolderPath), project_name(ProjectName), options(Options))), RetVal, (
        catch(
            (mkproject_impl(FolderPath, ProjectName, Options, ImplResult),
             ImplResult = ok(project_created(path(ProjectPath), name(ProjectName))),
             RetVal = ImplResult),
            error(Reason, Context),
            RetVal = error(project_error(Reason), Context)
        )
    ))
).

% Validate project entity composition
register_spell(
    perceive(project(validate)),
    input(perceive(project(validate(entity(Entity:entity))))),
    output(either(
        ok(valid),
        error(validation_failed(domain(Domain:term), reason(Reason:term)), Context:term)
    )),
    "Validates a project entity against its declared components and cross-domain requirements",
    [],
    implementation(perceive(project(validate(entity(Entity)))), Result, (
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
            error(verification_error(Domain, Reason), Context),
            Result = error(validation_failed(domain(Domain), reason(Reason)), Context)
        )
    ))
).

% Query project structure information
register_spell(
    perceive(project(structure)),
    input(perceive(project(structure(entity(Entity:entity))))),
    output(ok(project_info(
        type(Type:atom),
        sources(Sources:list(term)),
        contexts(Contexts:list(term))
    ))),
    "Queries project structure, including type, source patterns, and available contexts",
    [],
    implementation(perceive(project(structure(entity(Entity)))), Result, (
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

% Initialize Grimoire integration for existing project
register_spell(
    conjure(project(init)),
    input(conjure(project(init(folder(Folder:atom), options(Options:term))))),
    output(either(
        ok(initialized(entity(Entity:entity), path(Path:atom), detected(Features:list(term)), skills(Skills:list(term)))),
        error(init_error(Reason:term), Context:term)
    )),
    "Initialize Grimoire integration for existing project (non-invasive). Creates semantics.pl/.plt, detects git/nix infrastructure, creates 'default' session, and focuses on entity. Options: [force] to overwrite existing semantics.pl",
    [],
    implementation(conjure(project(init(folder(Folder), options(Options)))), Result, (
        catch(
            (project_init_impl(Folder, Options, ImplResult),
             ImplResult = ok(initialized(entity(Entity), path(Path), detected(Features), skills(Skills))),
             Result = ImplResult),
            error(Reason, Context),
            Result = error(init_error(Reason), Context)
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
        throw(error(project_already_exists(ProjectPath), mkproject_impl/4))
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
        throw(error(entity_renaming_failed(RenameResult), create_project_structure/4))
    ).

apply_template_or_basic(ProjectPath, ProjectName, Options, Result) :-
    (member(template(Template), Options) ->
        magic_cast(conjure(nix(flake(new(template_id(Template), dest_path(ProjectPath))))), TemplateResult),
        (TemplateResult = ok(_) ->
            rename_project_entities(ProjectPath, Template, ProjectName, Result)
        ;
            throw(error(template_instantiation_failed(TemplateResult), apply_template_or_basic/4))
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
            throw(error(project_git_init_failed(GitResult), init_git_if_requested/4))
        )
    ).

load_into_session_if_requested(ProjectPath, Options, RetVal, ProjectName) :-
    (member(load_into_session(true), Options) ->
        % Load project semantics into session
        directory_file_path(ProjectPath, 'semantics.pl', SemanticsFile),
        (exists_file(SemanticsFile) ->
            magic_cast(conjure(session(load_entity(semantic(file(SemanticsFile))))), LoadResult),
            (LoadResult = ok(_) ->
                RetVal = ok(project_created(path(ProjectPath), name(ProjectName)))
            ;
                throw(error(failed_to_load_into_session(LoadResult), load_into_session_if_requested/4))
            )
        ;
            % No semantics file to load
            RetVal = ok(project_created(path(ProjectPath), name(ProjectName)))
        )
    ;
        % Not requested, just return success
        RetVal = ok(project_created(path(ProjectPath), name(ProjectName)))
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

% === PROJECT INIT IMPLEMENTATION ===

% Main project init implementation
project_init_impl(Folder, Options, Result) :-
    % Resolve to absolute path
    format('Initializing Grimoire project in folder: ~w with options: ~q~n', [Folder, Options]),
    grimoire_resolve_path(Folder, ResolvedPath),
    format('Resolved path: ~w~n', [ResolvedPath]),
    absolute_file_name(ResolvedPath, AbsPath),
    format('Absolute path: ~w~n', [AbsPath]),

    % Check if directory exists
    (exists_directory(AbsPath) ->
        format('Directory exists: ~w~n', [AbsPath]),
        true
    ;   throw(error(directory_not_found(AbsPath), project_init_impl/3))
    ),

    % Check if semantics.pl already exists
    directory_file_path(AbsPath, 'semantics.pl', SemanticsPath),
    (exists_file(SemanticsPath) ->
        format('semantics.pl already exists at: ~w~n', [SemanticsPath]),
        (member(force, Options) ->
            format('Overwriting existing semantics.pl due to force option.~n', []),
            true  % Overwrite allowed
        ;   throw(error(semantics_already_exists(SemanticsPath), project_init_impl/3))
        )
    ;   true, format('No existing semantics.pl found. Proceeding with initialization.~n', [])
    ),

    % Sanitize directory name to entity name
    file_base_name(AbsPath, DirName),
    sanitize_directory_name(DirName, EntityName),
    format('Sanitized entity name: ~w~n', [EntityName]),

    % Detect infrastructure
    detect_infrastructure(AbsPath, Features),
    format('Detected features: ~q~n', [Features]),

    % Generate semantics.pl
    generate_init_semantics_pl(AbsPath, EntityName, Features),
    format('Generated `~w/semantics.pl`~n', [AbsPath]),

    % Load entity to trigger expansion
    load_entity(semantic(folder(AbsPath))),
    format('Loaded entity: ~w~n', [project(EntityName)]),

    % Query expanded components to get detailed info
    Entity = project(EntityName),
    query_expanded_components(Entity, Features, ExpandedInfo),
    format('Expanded component info: ~q~n', [ExpandedInfo]),

    % Generate semantics.plt with conditional tests
    generate_init_semantics_plt(AbsPath, EntityName, ExpandedInfo),
    format('Generated `~w/semantics.plt`~n', [AbsPath]),

    % If git detected, add files to git and update .gitignore
    (member(git, Features) ->
        format('Adding Grimoire files to git: ~w~n', [AbsPath]),
        add_grimoire_files_to_git(AbsPath)
    ;   true
    ),

    % Create/switch to default session
    ensure_default_session_and_focus(Entity),
    format('Ensured default session and focused on entity: ~w~n', [Entity]),

    % Get available skills
    magic_cast(perceive(skills(entity(Entity))), SkillsResult),
    (SkillsResult = ok(skills_list(Skills)) ->
        true, format('Derived skills: ~q~n', [Skills])
    ;   Skills = []
    ),

    Result = ok(initialized(entity(Entity), path(AbsPath), detected(Features), skills(Skills))).

% Sanitize directory name to valid Prolog atom
sanitize_directory_name(DirName, SanitizedAtom) :-
    atom_string(DirName, DirStr),
    % Convert to lowercase
    string_lower(DirStr, LowerStr),
    % Replace hyphens with underscores (using split/join for global replace)
    split_string(LowerStr, '-', '', Parts1),
    atomic_list_concat(Parts1, '_', Temp1),
    % Replace spaces with underscores
    atom_string(Temp1, Temp1Str),
    split_string(Temp1Str, ' ', '', Parts2),
    atomic_list_concat(Parts2, '_', Temp2),
    % Remove any other non-alphanumeric characters except underscore
    atom_string(Temp2, Temp2Str),
    re_replace('[^a-z0-9_]', '', Temp2Str, SanitizedStr, [g]),
    atom_string(SanitizedAtom, SanitizedStr).

% Detect infrastructure in directory
detect_infrastructure(Path, Features) :-
    findall(Feature, (
        detect_single_feature(Path, Feature)
    ), Features).

detect_single_feature(Path, git) :-
    atomic_list_concat([Path, '/.git'], GitDir),
    exists_directory(GitDir).

detect_single_feature(Path, nix) :-
    atomic_list_concat([Path, '/flake.nix'], FlakePath),
    exists_file(FlakePath).

% Generate semantics.pl for init
generate_init_semantics_pl(Path, EntityName, Features) :-
    directory_file_path(Path, 'semantics.pl', SemanticsPath),
    build_init_semantics_content(EntityName, Features, Content),
    magic_cast(conjure(fs(edit_file(file(SemanticsPath), edits([append(Content)])))), EditResult),
    (EditResult = ok(_) -> true ; throw(EditResult)).

build_init_semantics_content(EntityName, Features, Content) :-
    Entity = project(EntityName),
    EntityLine = {|string(Entity)||
:- self_entity({Entity}).

|},
    findall(FeatureLine, (
        member(Feature, Features),
        build_feature_component_line(Entity, Feature, FeatureLine)
    ), FeatureLines),
    atomic_list_concat(FeatureLines, '', FeatureLinesStr),
    string_concat(EntityLine, FeatureLinesStr, Content).

build_feature_component_line(Entity, git, Line) :-
    Line = {|string(Entity)||
component({Entity}, has(git(repository)), git(repository([]))).
|}.

build_feature_component_line(Entity, nix, Line) :-
    Line = {|string(Entity)||
component({Entity}, has(nix(flake)), nix(flake(ref('.'))).
|}.

% Query expanded components after loading
query_expanded_components(Entity, Features, ExpandedInfo) :-
    findall(info(Feature, Details), (
        member(Feature, Features),
        query_feature_details(Entity, Feature, Details)
    ), ExpandedInfo).

query_feature_details(Entity, git, git_info(Root)) :-
    catch(
        component(Entity, git_repository_root, Root),
        _, Root = unknown
    ).

query_feature_details(Entity, nix, nix_info(Packages, Checks, Apps)) :-
    catch(component(Entity, nix_flake_packages, Packages), _, Packages = []),
    catch(component(Entity, nix_flake_checks, Checks), _, Checks = []),
    catch(component(Entity, nix_flake_apps, Apps), _, Apps = []).

% Generate semantics.plt with conditional tests
generate_init_semantics_plt(Path, EntityName, ExpandedInfo) :-
    directory_file_path(Path, 'semantics.plt', TestPath),
    build_init_tests_content(EntityName, ExpandedInfo, ContentLines),
    atomic_list_concat(ContentLines, '', Content),
    magic_cast(conjure(fs(edit_file(file(TestPath), edits([append(Content)])))), EditResult),
    (EditResult = ok(_) -> true ; throw(EditResult)).

build_init_tests_content(EntityName, ExpandedInfo, Lines) :-
    Entity = project(EntityName),
    format(string(TestSuiteName), '~q', [Entity]),

    % Header and begin_tests
    Header = {|string(TestSuiteName)||
        :- use_module(library(plunit)).
        :- load_entity(semantic(folder('.'))).

        :- begin_tests(TestSuiteName).

    |},

    % Entity exists test
    EntityTest = {|string(Entity)||
        % Test entity exists and self component is valid
        test(entity_exists) :-
            user:entity({Entity}),
            user:please_verify(component({Entity}, self, semantic(folder(_Path)))).

    |},

    % Feature tests
    findall(FeatureTestLines, (
        member(info(Feature, Details), ExpandedInfo),
        build_feature_test_lines(Entity, Feature, Details, FeatureTestLines)
    ), FeatureTestLinesNested),
    flatten(FeatureTestLinesNested, FeatureTestLines),

    % Skills test
    SkillsTest = {|string(Entity)||
        % Test skills are derived
        test(skills_available) :-
            user:magic_cast(perceive(skills(entity({Entity}))), Result),
            Result = ok(skills_list(Skills)),
            assertion(is_list(Skills)),
            assertion(Skills \= []).

    |},

    % Footer
    Footer = {|string(TestSuiteName)||
        :- end_tests(TestSuiteName).
    |},

    append([[Header, EntityTest], FeatureTestLines, [SkillsTest, Footer]], Lines).

build_feature_test_lines(Entity, git, git_info(Root), Lines) :-
    DetectTest = {|string(Entity)||
        % Test git repository detection and status
        test(git_detected) :-
            user:please_verify(component({Entity}, has(git(repository)), _Repo)),
            user:please_verify(component({Entity}, git_repository_root, _Root)).

    |},
    (Root \= unknown ->
        StatusTest = {|string(Entity)||
        test(git_status_works) :-
            user:please_verify(component({Entity}, git_repository_root, Root)),
            user:magic_cast(perceive(git(status(git_root(Root)))), Result),
            assertion(Result = ok(_Result)).

    |},
        Lines = [DetectTest, StatusTest]
    ;   Lines = [DetectTest]
    ).

build_feature_test_lines(Entity, nix, nix_info(Packages, Checks, Apps), Lines) :-
    DetectTest = {|string(Entity)||
        % Test nix flake detection
        test(nix_flake_detected) :-
            user:please_verify(component({Entity}, has(nix(flake)), _Flake)),
            user:please_verify(component({Entity}, nix_flake_ref, _Ref)).

    |},

    findall(PackageTest, (
        member(Package, Packages),
        format(atom(TestName), 'nix_build_~w', [Package]),
        PackageTest = {|string(Package, TestName, Entity)||
        % Test building nix package: {Package}
        test({TestName}) :-
            user:magic_cast(conjure(invoke_skill(entity({Entity}), skill(nix(build({Package}))))), Result),
            assertion(Result = ok(_BuildResult)).

    |}
    ), PackageTests),

    findall(CheckTest, (
        member(Check, Checks),
        format(atom(TestName), 'nix_check_~w', [Check]),
        CheckTest = {|string(Check, TestName, Entity)||
        % Test nix check: {Check}
        test({TestName}) :-
            user:magic_cast(conjure(invoke_skill(entity({Entity}), skill(nix(check({Check}))))), Result),
            assertion(Result = ok(_CheckResult)).

    |}
    ), CheckTests),

    findall(AppTest, (
        member(App, Apps),
        format(atom(TestName), 'nix_run_~w', [App]),
        AppTest = {|string(App, TestName, Entity)||
        % Test nix app (blocked for safety): {App}
        test({TestName}, [blocked(requires_user_review)]) :-
            user:magic_cast(conjure(invoke_skill(entity({Entity}), skill(nix(run({App}))))), Result),
            assertion(Result = ok(_RunResult)).

    |}
    ), AppTests),

    append([[DetectTest], PackageTests, CheckTests, AppTests], Lines).

% Add Grimoire files to git and update .gitignore
add_grimoire_files_to_git(Path) :-
    % Add semantics.pl and semantics.plt
    atomic_list_concat([Path, '/semantics.pl'], SemanticsPath),
    atomic_list_concat([Path, '/semantics.plt'], TestPath),
    magic_cast(conjure(git(add(git_root(Path), paths([SemanticsPath, TestPath])))), AddResult),
    (AddResult = ok(_) -> true ; throw(AddResult)),

    % Add .grimoire directory (session semantics will be tracked)
    atomic_list_concat([Path, '/.grimoire'], GrimoirePath),
    (exists_directory(GrimoirePath) ->
        magic_cast(conjure(git(add(git_root(Path), paths([GrimoirePath])))), GrimoireAddResult),
        (GrimoireAddResult = ok(_) -> true ; throw(GrimoireAddResult))
    ;   true
    ),

    % Update .gitignore to exclude activity logs
    atomic_list_concat([Path, '/.gitignore'], GitignorePath),
    update_gitignore(GitignorePath).

% Update .gitignore to exclude activity_log.jsonl files
update_gitignore(GitignorePath) :-
    IgnorePattern = '.grimoire/**/activity_log.jsonl',

    % Read existing .gitignore if it exists
    (exists_file(GitignorePath) ->
        read_file_to_string(GitignorePath, ExistingContent, [])
    ;   ExistingContent = ''
    ),

    % Check if pattern already exists
    (sub_string(ExistingContent, _, _, _, 'activity_log.jsonl') ->
        true  % Already there, don't add
    ;   % Create newline prefix if file has content
        (ExistingContent = '' ->
            AppendContent = IgnorePattern
        ;   string_concat('\n', IgnorePattern, AppendContent)
        ),
        % Append pattern using edit_file spell
        magic_cast(conjure(fs(edit_file(file(GitignorePath), edits([append(AppendContent)])))), EditResult),
        (EditResult = ok(_) -> true ; throw(EditResult))
    ).

% Ensure default session exists, switch to it, and focus on entity
ensure_default_session_and_focus(Entity) :-
    % Create default session if it doesn't exist
    magic_cast(conjure(session(create(id(default)))), CreateResult),
    % Proceed regardless of result (session might already exist)
    (CreateResult = ok(_) -> true ; CreateResult = error(_) -> true),

    % Switch to default session
    magic_cast(conjure(session(switch(id(default)))), SwitchResult),
    (SwitchResult = ok(_) -> true ; throw(SwitchResult)),

    % Focus on entity (the focused_entity expansion will auto-load it)
    magic_cast(conjure(session(focus_entity(entity(Entity)))), FocusResult),
    (FocusResult = ok(_) -> true ; throw(FocusResult)).
