% Filesystem discovery utilities and file operations
:- use_module(library(filesex)).
:- use_module(library(apply)).
:- use_module(library(strings)).

% Filesystem entity with automatic self-location
:- self_entity(fs).

% === CORE FILESYSTEM ENTITIES ===

% Core filesystem concepts
component(fs, concept, file).
component(fs, concept, directory).
component(fs, concept, glob).
component(fs, concept, pattern).

% Filesystem utility commands
component(fs, utility, discover_fs_components).
component(fs, utility, glob_match).
component(fs, utility, file_exists).
component(fs, utility, directory_exists).

% === DSL ENTITIES FOR USER-FRIENDLY DECLARATIONS ===

% DSL pattern entities
entity(fs(structure)).
entity(fs(permissions)).

docstring(fs(structure),
    {|string(_)||
    Declarative filesystem structure specification.
    Users declare expected project structure in their semantics.pl files.
    Format: component(Entity, has(fs(structure)), fs(structure([Items])))

    Items can be:
    - file(Path) - expect a file at path
    - folder(Path, Contents) - expect a folder with nested contents

    Example:
        component(my_project, has(fs(structure)), fs(structure([
            file("README.md"),
            folder("src", [file("main.js"), file("utils.js")])
        ]))).
    |}).


docstring(fs(permissions),
    {|string(_)||
    Declarative file permission requirements.
    Specify expected file permissions.
    Format: component(Entity, has(fs(permissions)), fs(permissions(Path, PermType)))

    PermType can be:
    - executable - file should be executable
    - readable - file should be readable
    - writable - file should be writable

    Example:
        component(my_project, has(fs(permissions)), fs(permissions(
            "scripts/deploy.sh", executable
        ))).
    |}).

% === DSL SCHEMA REGISTRATIONS ===

% Schema 1: fs(structure) - Declarative filesystem structure
register_dsl_schema(
    fs,
    has(fs(structure)),
    signature(fs(structure(items('Items')))),
    "Declarative filesystem structure specification with files and folders",
    (
        component(Entity, has(fs(structure)), fs(structure(Items)))
            ==> (component(Entity, fs_structure_file, file_spec(Path, Opts)) :-
                    extract_file_specs(Items, FileSpecs),
                    member(file_spec(Path, Opts), FileSpecs)),
                (component(Entity, fs_structure_folder, folder_spec(Path, Contents)) :-
                    extract_folder_specs(Items, FolderSpecs),
                    member(folder_spec(Path, Contents), FolderSpecs))
            ::  is_list(Items)
    )
).

% Schema 2: fs(permissions) - Permission requirements
register_dsl_schema(
    fs,
    has(fs(permissions)),
    signature(fs(permissions(path('Path'), perm_type('PermType')))),
    "Declarative file permission requirements - verify executable, readable, or writable",
    (
        component(Entity, has(fs(permissions)), fs(permissions(Path, PermType)))
            ==> component(Entity, fs_permission_requirement, permission_spec(Path, PermType))
            ::  (atom(Path) ; string(Path)),
                member(PermType, [executable, readable, writable])
    )
).

% === LEAF COMPONENT VERIFICATIONS ===

% Leaf verification: fs_structure_file
component(_, fs_structure_file, file_spec(Path, Opts))
    :: (atom(Path) ; string(Path)),
       is_list(Opts),
       file_exists_and_valid(Path, Opts).

% Leaf verification: fs_structure_folder
component(_, fs_structure_folder, folder_spec(Path, Contents))
    :: (atom(Path) ; string(Path)),
       is_list(Contents),
       folder_exists_and_valid(Path).

% Leaf verification: fs_permission_requirement
component(_, fs_permission_requirement, permission_spec(Path, Type))
    :: (atom(Path) ; string(Path)),
       member(Type, [executable, readable, writable]),
       file_exists_with_permission(Path, Type).

% Helper to extract file specs from structure items
extract_file_specs([], []).
extract_file_specs([file(Path)|Rest], [file_spec(Path, [])|RestSpecs]) :-
    extract_file_specs(Rest, RestSpecs).
extract_file_specs([folder(FolderPath, Contents)|Rest], AllSpecs) :-
    extract_file_specs(Contents, ContentSpecs),
    % Prepend folder path to all nested file paths
    maplist(prepend_folder_path(FolderPath), ContentSpecs, PrefixedSpecs),
    extract_file_specs(Rest, RestSpecs),
    append(PrefixedSpecs, RestSpecs, AllSpecs).
extract_file_specs([_|Rest], Specs) :-
    extract_file_specs(Rest, Specs).

% Helper to prepend folder path to file spec
prepend_folder_path(FolderPath, file_spec(FilePath, Opts), file_spec(FullPath, Opts)) :-
    atomic_list_concat([FolderPath, '/', FilePath], FullPath).

% Helper to extract folder specs from structure items
extract_folder_specs([], []).
extract_folder_specs([folder(Path, Contents)|Rest], [folder_spec(Path, Contents)|RestSpecs]) :-
    extract_folder_specs(Rest, RestSpecs).
extract_folder_specs([file(_)|Rest], Specs) :-
    extract_folder_specs(Rest, Specs).
extract_folder_specs([_|Rest], Specs) :-
    extract_folder_specs(Rest, Specs).

% === OS VERIFICATION HELPERS ===

% Verify file exists and satisfies options
file_exists_and_valid(Path, Opts) :-
    (atom(Path) ; string(Path)),
    (exists_file(Path) -> true
    ; throw(verification_error(fs, missing_file(Path)))),
    verify_file_opts_on_os(Path, Opts).

% Verify folder exists
folder_exists_and_valid(Path) :-
    (atom(Path) ; string(Path)),
    (exists_directory(Path) -> true
    ; throw(verification_error(fs, missing_folder(Path)))).

% Verify file with permission
file_exists_with_permission(Path, Type) :-
    (atom(Path) ; string(Path)),
    (exists_file(Path) -> true
    ; throw(verification_error(fs, file_not_found(Path)))),
    verify_permission_on_os(Path, Type).

% Verify file options against OS
verify_file_opts_on_os(_Path, []).
verify_file_opts_on_os(Path, [executable|Rest]) :-
    (access_file(Path, execute) -> true
    ; throw(verification_error(fs, not_executable(Path)))),
    verify_file_opts_on_os(Path, Rest).
verify_file_opts_on_os(Path, [must_exist|Rest]) :-
    verify_file_opts_on_os(Path, Rest).
verify_file_opts_on_os(Path, [_|Rest]) :-
    verify_file_opts_on_os(Path, Rest).

% Verify permission type on OS
verify_permission_on_os(Path, executable) :-
    (access_file(Path, execute) -> true
    ; throw(verification_error(fs, not_executable(Path)))).
verify_permission_on_os(Path, readable) :-
    (access_file(Path, read) -> true
    ; throw(verification_error(fs, not_readable(Path)))).
verify_permission_on_os(Path, writable) :-
    (access_file(Path, write) -> true
    ; throw(verification_error(fs, not_writable(Path)))).

% === SPELL REGISTRATIONS (PHASE 3) ===

% === PERCEIVE SPELLS ===

register_spell(
    perceive(fs(read_file)),
    input(fs(read_file(path('Path'), start('Start'), end('End')))),
    output(either(
        ok(file_content(lines_with_numbers('ContentWithLineNumbers'))),
        error(fs_error('Reason'))
    )),
    "Read file with line numbers using 1-based indexing. Supports negative line numbers (-1 = last line).",
    [],
    implementation(perceive(fs(read_file(path(Path), start(Start), end(End)))), Result, (
        catch(
            (read_file_to_lines(Path, AllLines),
             length(AllLines, TotalLines),
             % Resolve negative line numbers
             resolve_line_number(Start, TotalLines, StartNum),
             resolve_line_number(End, TotalLines, EndNum),
             % Extract requested lines with numbers
             findall(line(LineNum, Content),
                 (between(StartNum, EndNum, LineNum),
                  nth1(LineNum, AllLines, Content)),
                 ContentWithLineNumbers),
             Result = ok(file_content(ContentWithLineNumbers))),
            Error,
            Result = error(fs_error(Error))
        )
    ))
).

% Resolve negative line numbers (-1 = last line, -2 = second to last, etc.)
resolve_line_number(Num, TotalLines, Resolved) :-
    (Num < 0 ->
        Resolved is TotalLines + Num + 1  % -1 becomes TotalLines, -2 becomes TotalLines-1, etc.
    ;
        Resolved = Num
    ).

register_spell(
    perceive(fs(list_files)),
    input(fs(list_files(directory('Dir')))),
    output(either(ok(files('FileList')), error(fs_error('Reason')))),
    "List all files in a directory (non-recursive)",
    [],
    implementation(perceive(fs(list_files(directory(Dir)))), Result, (
        catch(
            (directory_files(Dir, AllFiles),
             exclude(=('.'), AllFiles, FilteredFiles),
             exclude(=('..'), FilteredFiles, Files),
             Result = ok(files(Files))),
            Error,
            Result = error(fs_error(Error))
        )
    ))
).

register_spell(
    perceive(fs(glob_match)),
    input(fs(glob_match(pattern('Pattern'), base('BaseDir')))),
    output(either(ok(matched_files('FileList')), error(fs_error('Reason')))),
    "Match files using glob patterns (e.g., '**/*.pl')",
    [],
    implementation(perceive(fs(glob_match(pattern(Pattern), base(BaseDir)))), Result, (
        catch(
            (expand_file_name(Pattern, Matches),
             (atom(BaseDir), BaseDir \= '' ->
                 findall(Rel, (member(Abs, Matches), relative_file_name(Rel, BaseDir, Abs)), Files)
             ;
                 Files = Matches
             ),
             Result = ok(matched_files(Files))),
            Error,
            Result = error(fs_error(Error))
        )
    ))
).

register_spell(
    perceive(fs(file_stats)),
    input(fs(file_stats(path('Path')))),
    output(either(ok(stats(size('Size'), modified('Time'), mode('Mode'))), error(fs_error('Reason')))),
    "Get file statistics (size, modified time, permissions)",
    [],
    implementation(perceive(fs(file_stats(path(Path)))), Result, (
        catch(
            (size_file(Path, Size),
             time_file(Path, Time),
             access_file(Path, exist),
             (access_file(Path, execute) -> Mode = executable ; Mode = regular),
             Result = ok(stats(size(Size), modified(Time), mode(Mode)))),
            Error,
            Result = error(fs_error(Error))
        )
    ))
).

% === CONJURE SPELLS ===

register_spell(
    conjure(fs(edit_file)),
    input(fs(edit_file(file('Path'), edits('Edits')))),
    output(either(
        ok(file_modified('Path')),
        error(edit_error('Reason'))
    )),
    "Edit file with specified operations. Supports insert, delete, replace, and append operations.",
    [],
    implementation(conjure(fs(edit_file(file(Path), edits(Edits)))), Result, (
        % Handle both existing and non-existing files
        (exists_file(Path) ->
            read_file_to_lines(Path, Lines)
        ;
            Lines = []
        ),
        maplist(validate_edit, Edits),
        apply_edits(Edits, Lines, NewLines),
        write_lines_to_file(Path, NewLines),
        Result = ok(file_modified(Path))
    ))
).

register_spell(
    conjure(fs(mkdir)),
    input(fs(mkdir(path('Path'), options('Options')))),
    output(either(
        ok(directory_created('Path')),
        error(fs_error('Reason'))
    )),
    "Create directory and initialize with semantics.pl file. Options: [git(auto)] (default) or [git(false)] to disable auto git-add.",
    [],
    implementation(conjure(fs(mkdir(path(Path), options(Options)))), Result, (
        % Create directory using Prolog's make_directory_path
        make_directory_path(Path),
        % Initialize semantics.pl with proper module
        directory_file_path(Path, "semantics.pl", SemanticsFile),
        InitContent = {|string(Path, Parent)||
:- module(semantic_{Path}, [entity/1, component/3]).

% Dynamic declarations for this module
:- dynamic entity/1.
:- dynamic component/3.

% This directory's entity
entity(folder('{Path}')).
|},
        write_file(SemanticsFile, InitContent),
        % Update parent semantics if exists
        directory_file_path(Parent, _, Path),
        directory_file_path(Parent, "semantics.pl", ParentSemantic),
        (exists_file(ParentSemantic) ->
            magic_cast(conjure(fs(edit_file(file(ParentSemantic), [
                append({|string(Path, Parent)||
entity(folder('{Parent}')).
component(folder('{Parent}'), subfolder, folder('{Path}')).
|})
            ]))), _)
        ; true),

        % Conditional git integration (default: auto)
        option(git(GitMode), Options, auto),
        (GitMode = false -> true
        ; is_git_directory(Path) -> magic_cast(conjure(git(add([Path]))), _)
        ; true),

        Result = ok(directory_created(Path))
    ))
).

register_spell(
    conjure(fs(mkfile)),
    input(fs(mkfile(path('Path'), options('Options')))),
    output(either(
        ok(file_created('Path')),
        error(fs_error('Reason'))
    )),
    "Create empty file. Updates parent semantics if exists. Options: [git(auto)] (default) or [git(false)] to disable auto git-add.",
    [],
    implementation(conjure(fs(mkfile(path(Path), options(Options)))), Result, (
        % Create empty file
        write_file(Path, ""),
        % Update parent semantics if exists
        directory_file_path(Parent, Name, Path),
        directory_file_path(Parent, "semantics.pl", ParentSemantic),
        (exists_file(ParentSemantic) ->
            magic_cast(conjure(fs(edit_file(file(ParentSemantic), [
                append({|string(Parent,Name)||
entity(folder('{Parent}')).
component(folder('{Parent}'), file, file('{Name}')).
|})
            ]))), _)
        ; true),

        % Conditional git integration (default: auto)
        option(git(GitMode), Options, auto),
        (GitMode = false -> true
        ; is_git_directory(Path) -> magic_cast(conjure(git(add([Path]))), _)
        ; true),

        Result = ok(file_created(Path))
    ))
).

register_spell(
    conjure(fs(copy_file)),
    input(fs(copy_file(source('Source'), dest('Dest')))),
    output(either(ok(file_copied('Source', 'Dest')), error(fs_error('Reason')))),
    "Copy file from source to destination path",
    [],
    implementation(conjure(fs(copy_file(source(Source), dest(Dest)))), Result, (
        catch(
            (copy_file(Source, Dest), Result = ok(file_copied(Source, Dest))),
            Error,
            Result = error(fs_error(Error))
        )
    ))
).

register_spell(
    conjure(fs(move_file)),
    input(fs(move_file(source('Source'), dest('Dest')))),
    output(either(ok(file_moved('Source', 'Dest')), error(fs_error('Reason')))),
    "Move/rename file from source to destination path",
    [],
    implementation(conjure(fs(move_file(source(Source), dest(Dest)))), Result, (
        catch(
            (rename_file(Source, Dest), Result = ok(file_moved(Source, Dest))),
            Error,
            Result = error(fs_error(Error))
        )
    ))
).

register_spell(
    conjure(fs(delete_file)),
    input(fs(delete_file(path('Path')))),
    output(either(ok(file_deleted('Path')), error(fs_error('Reason')))),
    "Delete file at specified path",
    [],
    implementation(conjure(fs(delete_file(path(Path)))), Result, (
        catch(
            (delete_file(Path), Result = ok(file_deleted(Path))),
            Error,
            Result = error(fs_error(Error))
        )
    ))
).

% === FILE EDITING HELPERS ===

% Edit file constructor validation
validate_edit(Edit) :-
    functor(Edit, Type, _),
    component(edit_file, ctor, Type).

% Apply list of edits sequentially
apply_edits([], Lines, Lines).
apply_edits([Edit|Rest], Lines, Final) :-
    apply_edit(Edit, Lines, Intermediate),
    apply_edits(Rest, Intermediate, Final).

% Apply single edit operation
apply_edit(insert(N, Content), Lines, Result) :-
    length(Lines, Len),
    N > 0, N =< Len + 1,  % Allow insert at end
    % Split lines at insertion point
    NSplit is N - 1,
    split_at(NSplit, Lines, Before, After),
    % Split new content into lines
    string_lines(Content, NewLines),
    % Combine parts
    append([Before, NewLines, After], Result).

apply_edit(delete(Start, End), Lines, Result) :-
    length(Lines, Len),
    Start > 0, Start =< Len,
    End >= Start, End =< Len,
    % Split into before, [to_delete], after
    NSplit is Start - 1,
    split_at(NSplit, Lines, Before, Rest),
    NDel is End - Start + 1,
    split_at(NDel, Rest, _, After),
    % Combine before and after
    append(Before, After, Result).

apply_edit(replace(Start, End, Content), Lines, Result) :-
    length(Lines, Len),
    Start > 0, Start =< Len,
    End >= Start, End =< Len,
    % Split lines into before, to_replace, and after
    NSplit is Start - 1,
    split_at(NSplit, Lines, Before, Rest),
    NDel is End - Start + 1,
    split_at(NDel, Rest, _, After),
    % Split new content into lines
    string_lines(Content, NewLines),
    % Combine parts
    append([Before, NewLines, After], Result).

apply_edit(append(Content), Lines, Result) :-
    string_lines(Content, NewLines),
    append(Lines, NewLines, Result).

% List splitting helper
% split_at(+N, +List, -Before, -After)
% Splits List at position N, Before gets first N elements, After gets rest
split_at(0, List, [], List) :- !.
split_at(N, [H|T], [H|Before], After) :-
    N > 0,
    N1 is N - 1,
    split_at(N1, T, Before, After).

% === FILE I/O HELPERS ===

% Write content to file with directory creation
write_file(Path, Content) :-
    directory_file_path(Dir, _, Path),
    make_directory_path(Dir),
    setup_call_cleanup(
        open(Path, write, Stream),
        write(Stream, Content),
        close(Stream)
    ).

% Read file into list of lines
read_file_to_lines(Path, Lines) :-
    exists_file(Path),
    setup_call_cleanup(
        open(Path, read, Stream),
        read_string(Stream, _, String),
        close(Stream)
    ),
    string_lines(String, Lines).

% Write list of lines to file
write_lines_to_file(Path, Lines) :-
    atomic_list_concat(Lines, '\n', Content),
    write_file(Path, Content).

% === EDIT_FILE ENTITY AND CONSTRUCTORS ===

% Edit file is an entity with subcommands
entity(edit_file).
component(edit_file, ctor, insert).
component(edit_file, ctor, delete).
component(edit_file, ctor, replace).
component(edit_file, ctor, append).

% Edit file constructor entities with docstrings
entity(edit_file(insert)).
docstring(edit_file(insert),
    {|string(_)||
    Insert text at a specific line number.
    Format: insert(LineNumber, Text)
    - LineNumber: Line number where text will be inserted (1-indexed)
    - Text: String to insert as a new line
    Example: insert(5, "new line content")
    |}).

entity(edit_file(delete)).
docstring(edit_file(delete),
    {|string(_)||
    Delete lines from a file.
    Format: delete(StartLine, EndLine)
    - StartLine: First line to delete (1-indexed)
    - EndLine: Last line to delete (inclusive)
    Example: delete(3, 5)  % Deletes lines 3, 4, and 5
    |}).

entity(edit_file(replace)).
docstring(edit_file(replace),
    {|string(_)||
    Replace a range of lines with new text.
    Format: replace(StartLine, EndLine, NewText)
    - StartLine: First line to replace (1-indexed)
    - EndLine: Last line to replace (inclusive)
    - NewText: Text to replace the lines with
    Example: replace(2, 4, "replacement text")
    |}).

entity(edit_file(append)).
docstring(edit_file(append),
    {|string(_)||
    Append text to the end of the file.
    Format: append(Text)
    - Text: String to append as a new line at the end of file
    Example: append("new last line")
    |}).

docstring(edit_file, S) :-
    make_ctors_docstring(edit_file, CtorsDoc),
    S = {|string(CtorsDoc)||
    File editing term structure for specifying file operations.
    Format: edit_file(file(Path), [Edit1, Edit2, ...])
    - Path: File path to edit
    - Edits: List of edit operations

    Available edit operations:
    {CtorsDoc}
    |}.

% === FILESYSTEM PATTERN TYPES (EXISTING) ===

entity(fs(pattern)).
component(fs(pattern), ctor, include).
component(fs(pattern), ctor, exclude).

% Glob pattern entity
entity(fs(glob)).

docstring(fs(glob),
    {|string(_)||
    Filesystem glob pattern matching for file discovery.
    Expands glob patterns to match files and directories.
    Supports standard glob syntax: *, ?, [abc], {a,b,c}, **
    Example: fs(glob("src/**/*.pl", Files))
    Used for pattern-based file system operations.
    |}).

docstring(fs,
    {|string(_)||
    Filesystem domain providing file operations, directory management,
    and declarative filesystem structure verification.

    Provides both:
    - Imperative file operations (read, write, edit, mkdir, mkfile)
    - Declarative structure verification (DSL patterns with verify/1)

    Users can declare expected filesystem structure in their semantics.pl
    and verify it against actual filesystem state.
    |}
).

docstring(fs(pattern),
    {|string(_)||
    Filesystem pattern matching for include/exclude operations.
    Format:
      include(GlobPatterns) - Include files matching these patterns
      exclude(GlobPatterns) - Exclude files matching these patterns
    |}
).

docstring(discover_fs_components,
    {|string(_)||
    Discover filesystem components based on patterns.
    Format: discover_fs_components(BasePath, Patterns, Components)

    Patterns format:
      [include([glob("*.rs"), glob("src/**")]), exclude([glob("target/**")])]

    Returns Components as component/3 facts for discovered files/directories.
    |}
).

% === CORE FILESYSTEM DISCOVERY (EXISTING) ===

% Core filesystem discovery predicate
discover_fs_components(Entity, BasePath, Patterns, Components) :-
    absolute_file_name(BasePath, AbsBasePath),
    extract_patterns(Patterns, IncludePatterns, ExcludePatterns),
    find_matching_files(AbsBasePath, IncludePatterns, ExcludePatterns, Files),
    find_matching_directories(AbsBasePath, IncludePatterns, ExcludePatterns, Dirs),
    append(Files, Dirs, AllPaths),
    maplist(path_to_component(Entity, AbsBasePath), AllPaths, Components).

% Extract include/exclude patterns from pattern list
extract_patterns([], [], []).
extract_patterns([include(Patterns)|Rest], AllIncludes, Excludes) :-
    extract_patterns(Rest, RestIncludes, Excludes),
    append(Patterns, RestIncludes, AllIncludes).
extract_patterns([exclude(Patterns)|Rest], Includes, AllExcludes) :-
    extract_patterns(Rest, Includes, RestExcludes),
    append(Patterns, RestExcludes, AllExcludes).
extract_patterns([_|Rest], Includes, Excludes) :-
    extract_patterns(Rest, Includes, Excludes).

% Find files matching include patterns but not exclude patterns
find_matching_files(BasePath, IncludePatterns, ExcludePatterns, Files) :-
    findall(File,
        (member(glob(Pattern), IncludePatterns),
         glob_files(BasePath, Pattern, CandidateFiles),
         member(File, CandidateFiles),
         exists_file(File),
         \+ matches_exclude_patterns(File, ExcludePatterns)),
        Files).

% Find directories matching include patterns but not exclude patterns
find_matching_directories(BasePath, IncludePatterns, ExcludePatterns, Dirs) :-
    findall(Dir,
        (member(glob(Pattern), IncludePatterns),
         glob_directories(BasePath, Pattern, CandidateDirs),
         member(Dir, CandidateDirs),
         exists_directory(Dir),
         \+ matches_exclude_patterns(Dir, ExcludePatterns)),
        Dirs).

% Glob matching for files
glob_files(BasePath, Pattern, Files) :-
    directory_file_path(BasePath, Pattern, FullPattern),
    expand_file_name(FullPattern, AllPaths),
    include(exists_file, AllPaths, Files).

% Glob matching for directories
glob_directories(BasePath, Pattern, Dirs) :-
    directory_file_path(BasePath, Pattern, FullPattern),
    expand_file_name(FullPattern, AllPaths),
    include(exists_directory, AllPaths, Dirs).

% Check if path matches any exclude pattern
matches_exclude_patterns(Path, ExcludePatterns) :-
    member(glob(ExcludePattern), ExcludePatterns),
    glob_match(Path, ExcludePattern).

% Simple glob matching (can be enhanced)
glob_match(Path, Pattern) :-
    % For now, simple wildcard matching
    atom_codes(Path, PathCodes),
    atom_codes(Pattern, PatternCodes),
    glob_match_codes(PathCodes, PatternCodes).

% Simple code-level glob matching
glob_match_codes([], []).
glob_match_codes(Path, [0'*|PatternRest]) :-
    append(_, Suffix, Path),
    glob_match_codes(Suffix, PatternRest).
glob_match_codes([C|PathRest], [C|PatternRest]) :-
    glob_match_codes(PathRest, PatternRest).

% Convert simple glob patterns to regex - simplified
glob_to_regex(Pattern, Regex) :-
    % For now, just return the pattern as is
    Regex = Pattern.

% Convert filesystem path to component fact
path_to_component(Entity, BasePath, Path, component(Entity, Type, Value)) :-
    make_relative_path(Path, BasePath, RelPath),
    (exists_file(Path) ->
        (Type = file, Value = file(RelPath))
    ; exists_directory(Path) ->
        (Type = subdir, Value = folder(RelPath))
    ;
        fail
    ).

% Helper to get relative path
make_relative_path(AbsPath, BasePath, RelPath) :-
    atom_length(BasePath, BaseLen),
    PrefixLen is BaseLen + 1,  % +1 for the slash
    sub_atom(AbsPath, PrefixLen, _, 0, RelPath).

% Filesystem source location
component(fs, source, file("fs.pl")).
