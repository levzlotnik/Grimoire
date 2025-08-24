% Filesystem discovery utilities
:- use_module(library(filesex)).
:- use_module(library(apply)).
:- use_module(library(strings)).

% Allow entity/component declarations to be discontiguous
:- discontiguous entity/1.
:- discontiguous component/3.

% Filesystem entity with automatic self-location  
:- self_entity(fs).

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

% Filesystem pattern types
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
    Filesystem discovery utilities for project introspection.
    Provides glob matching, file/directory discovery, and pattern-based
    filesystem traversal capabilities.
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
