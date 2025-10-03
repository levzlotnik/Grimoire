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
entity(fs(file_content)).
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

docstring(fs(file_content),
    {|string(_)||
    Declarative file content requirements.
    Specify what content should exist in files.
    Format: component(Entity, has(fs(file_content)), fs(file_content(Path, Requirements)))

    Requirements can be:
    - contains([String1, String2, ...]) - file must contain all these strings

    Example:
        component(my_project, has(fs(file_content)), fs(file_content(
            "package.json", contains(["name", "version"])
        ))).
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

% === DSL EXPANSION RULES (GENERATIVE FLOW) ===

% Expand fs(structure) declarations into detailed components
component(Entity, fs_structure_file, file_spec(Path, Options)) :-
    component(Entity, has(fs(structure)), fs(structure(Items))),
    extract_file_specs(Items, FileSpecs),
    member(file_spec(Path, Options), FileSpecs).

component(Entity, fs_structure_folder, folder_spec(Path, Contents)) :-
    component(Entity, has(fs(structure)), fs(structure(Items))),
    extract_folder_specs(Items, FolderSpecs),
    member(folder_spec(Path, Contents), FolderSpecs).

component(Entity, fs_content_requirement, content_spec(Path, Requirements)) :-
    component(Entity, has(fs(file_content)), fs(file_content(Path, Requirements))).

component(Entity, fs_permission_requirement, permission_spec(Path, PermType)) :-
    component(Entity, has(fs(permissions)), fs(permissions(Path, PermType))).

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

% === SPELL CONSTRUCTOR REGISTRATION ===

% Spell constructors auto-derived from register_spell/4 declarations
% Manual ctors only for commands without register_spell yet:
component(conjure, ctor, fs(copy_file)).
component(conjure, ctor, fs(move_file)).
component(conjure, ctor, fs(delete_file)).

component(perceive, ctor, fs(list_files)).
component(perceive, ctor, fs(glob_match)).
component(perceive, ctor, fs(file_stats)).

% Note: fs(read_file), fs(edit_file), fs(mkdir), and fs(mkfile) constructors
% are auto-derived from their register_spell/4 declarations below

% === SPELL FORMAT REGISTRATIONS ===

% Each register_spell is placed right above its cast implementation

% === PERCEIVE SPELLS (MIGRATED FROM GRIMOIRE.PL) ===

register_spell(
    perceive(fs(read_file)),
    input(fs(read_file(file_path('FilePath'), start('Start'), end('End')))),
    output(either(
        ok(file_content(lines_with_numbers('ContentWithLineNumbers'))),
        error(fs_error('Reason'))
    )),
    docstring("Read file with line numbers using 1-based indexing. Supports negative line numbers (-1 = last line).")
).

% Read file with line numbers using 1-based indexing
cast(perceive(fs(read_file(FilePath, Start, End))), Result) :-
    catch(
        (read_file_to_lines(FilePath, AllLines),
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
    ).

% Resolve negative line numbers (-1 = last line, -2 = second to last, etc.)
resolve_line_number(Num, TotalLines, Resolved) :-
    (Num < 0 ->
        Resolved is TotalLines + Num + 1  % -1 becomes TotalLines, -2 becomes TotalLines-1, etc.
    ;
        Resolved = Num
    ).

% === CONJURE SPELLS (MIGRATED FROM GRIMOIRE.PL) ===

register_spell(
    conjure(fs(edit_file)),
    input(fs(edit_file(file('Path'), edits('Edits')))),
    output(either(
        ok(file_modified('Path')),
        error(edit_error('Reason'))
    )),
    docstring("Edit file with specified operations. Supports insert, delete, replace, and append operations.")
).

% Edit file operation
cast(conjure(fs(edit_file(file(Path), Edits))), RetVal) :-
    % Handle both existing and non-existing files
    (exists_file(Path) ->
        read_file_to_lines(Path, Lines)
    ;
        Lines = []
    ),
    maplist(validate_edit, Edits),
    apply_edits(Edits, Lines, NewLines),
    write_lines_to_file(Path, NewLines),
    RetVal = ok(file_modified(Path)).

register_spell(
    conjure(fs(mkdir)),
    input(fs(mkdir(path('Path')))),
    output(either(
        ok(directory_created('Path')),
        error(fs_error('Reason'))
    )),
    docstring("Create directory and initialize with semantics.pl file. Updates parent semantics if exists.")
).

% Create directory operation
cast(conjure(fs(mkdir(Path))), ok(directory_created(Path))) :-
    % Create directory using Prolog's make_directory_path
    make_directory_path(Path),
    % Initialize semantics.pl with proper module
    directory_file_path(Path, "semantics.pl", SemanticsFile),
    InitContent = {|string(Path)||
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
            append({|string(Path)||
entity(folder('{Parent}')).
component(folder('{Parent}'), subfolder, folder('{Path}')).
|})
        ]))), _)
    ; true).

register_spell(
    conjure(fs(mkfile)),
    input(fs(mkfile(path('Path')))),
    output(either(
        ok(file_created('Path')),
        error(fs_error('Reason'))
    )),
    docstring("Create empty file. Updates parent semantics if exists.")
).

% Create file operation
cast(conjure(fs(mkfile(Path))), RetVal) :-
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
    RetVal = ok(file_created(Path)).

register_spell(
    conjure(fs(mkdir)),
    input(fs(mkdir(path('Path'), options('Options')))),
    output(either(
        ok(directory_created('Path')),
        error(fs_error('Reason'))
    )),
    docstring("Create directory with options. Supports git(false) to disable auto git-add.")
).

% Mkdir with options (auto git-add support)
cast(conjure(fs(mkdir(Path, Options))), RetVal) :-
    magic_cast(conjure(fs(mkdir(Path))), RetVal),
    % Auto git-add if we're in a repo and not disabled
    (option(git(false), Options) ->
        true
    ;
        is_git_directory(Path) ->
            magic_cast(conjure(git(add([Path]))), _)
        ;
        true
    ).

register_spell(
    conjure(fs(mkfile)),
    input(fs(mkfile(path('Path'), options('Options')))),
    output(either(
        ok(file_created('Path')),
        error(fs_error('Reason'))
    )),
    docstring("Create file with options. Supports git(false) to disable auto git-add.")
).

% Mkfile with options (auto git-add support)
cast(conjure(fs(mkfile(Path, Options))), RetVal) :-
    magic_cast(conjure(fs(mkfile(Path))), RetVal),
    % Auto git-add if we're in a repo and not disabled
    (option(git(false), Options) ->
        true
    ;
        is_git_directory(Path) ->
            magic_cast(conjure(git(add([Path]))), _)
        ;
        true
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
