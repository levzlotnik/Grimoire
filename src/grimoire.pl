% Load core rules first - bootstrap with direct ensure_loaded
% We need core_rules.pl to get grimoire_ensure_loaded, so we load it directly
:- (getenv('GRIMOIRE_ROOT', Root) -> 
    atomic_list_concat([Root, '/src/core_rules.pl'], CoreRules),
    ensure_loaded(CoreRules)
   ; 
    ensure_loaded("./core_rules.pl")
   ).
:- use_module(library(filesex)).
:- use_module(library(http/json)).

% Dynamic declarations specific to core_rules
:- dynamic execute/2.   % execute(Transaction, Status)
:- dynamic([run/2], [discontiguous(true), multifile(true)]).       % run(Command, RetVal)
:- dynamic([perceive/1], [discontiguous(true), multifile(true)]).  % perceive(Query)
:- dynamic([cast/2], [discontiguous(true), multifile(true)]).      % cast(Spell, RetVal)

% The most fundamental entity
:- self_entity(system).

docstring(system, S) :-
    grimoire_resolve_path('@/src/GRIMOIRE.md', GrimoirePath),
    read_file_to_string(GrimoirePath, GrimoireDoc, []),
    findall(Line, (
        component(system, concept, Concept),
        docstring(Concept, ConceptDoc),
        format(atom(Line), '  - ~w: ~w', [Concept, ConceptDoc])
    ), Lines),
    atomic_list_concat(Lines, '\n', ConceptsDocs),
    format(string(S), '~w~n~nCore Concepts:~n~w', [GrimoireDoc, ConceptsDocs]).


component(system, root_dir, folder("/home/levz/Projects/Grimoire")).
% Some properties
component(
    system,
    semantic_root,
    folder(AbsSemRoot)
) :-
    RelSemRoot = "src/prolog",
    component(system, root_dir, folder(SysRoot)),
    directory_file_path(SysRoot, RelSemRoot, AbsSemRoot).

system_semantic_root(SemanticRootDir) :-
    component(system, semantic_root, SemanticRootDir).


% Fundamental concepts
component(system, concept, spell).
component(system, concept, transaction).
component(system, concept, hardware).
component(system, concept, execute).
component(system, concept, source).
component(system, concept, project).
component(system, concept, conjure).
component(system, concept, perceive).
component(system, concept, interface).
component(system, concept, git).
component(system, concept, nix).
component(system, concept, session).

component(system, source, source(semantic(file("grimoire.pl")))).

entity(source).
component(source, ctor, semantic).
entity(semantic).
component(semantic, ctor, file).
component(semantic, ctor, folder).

docstring(semantic,
    {|string(_)||
    Semantic knowledge source specification.
    Constructors: file(Path), folder(Path)
    Used to load knowledge from semantics.pl files or folders containing them.
    Example: load_entity(semantic(file("./semantics.pl")))
    Forms the basis of Grimoire's distributed knowledge architecture.
    |}).
docstring(source,S) :-
    make_ctors_docstring(source, SourceTypes),
    S = {|string(SourceTypes)||
    Source of representation for entity.
    Programmers usually refer to it broadly as "source code".
    Typically could be things like source code files, all the way up to
    entire project directories that contain a mix of code, data
    and execution contexts (compilation/runtime flows and scripts).

    Format: source(SourceType)

    Currently defined source types:
    {SourceTypes}
    |}.
docstring(source(semantic(file)), "A source code file for an entity.\nFormat: semantic(file(Path))").
docstring(source(semantic(folder)), "A folder with source code for entity.\nFormat: semantic(folder(Path))").

% System components and their sources - these are just metadata now
% The actual loading is handled by entity/1 rules below
component(system, subsystem, git).
component(system, subsystem, nix).
component(system, subsystem, fs).
component(system, subsystem, project).
component(system, subsystem, session).

entity(project).
component(project, source, source(semantic(folder("project")))).

docstring(project,
    {|string(_)||
    A project represents the organizational unit of a system,
    bundling together source code, configuration files,
    and other resources necessary for building and running an application or library.
    It structures the work into manageable components and defines the
    ecosystem in which the system operates.
    |}
).

% Core subsystem entities - loaded immediately on boot
% Load core system components - immediate loading for core functionality
:- load_entity(semantic(file("@/src/git.pl"))).
:- load_entity(semantic(folder("@/src/nix"))).
:- load_entity(semantic(file("@/src/fs.pl"))).
:- load_entity(semantic(folder("@/src/project"))).
:- load_entity(semantic(file("@/src/session.pl"))).

% Spell system - fantasy-themed query/mutation separation
entity(spell).
component(spell, ctor, conjure).
component(spell, ctor, perceive).

% Conjure entity for mutable operations
entity(conjure).
component(conjure, ctor, shell).
component(conjure, ctor, mkdir).
component(conjure, ctor, mkfile).
component(conjure, ctor, edit_file).
component(conjure, ctor, executable_program).
component(conjure, ctor, session).

% Perceive entity for query operations
entity(perceive).
% Core perceive constructors
component(perceive, ctor, entities).
component(perceive, ctor, read_file).
component(perceive, ctor, search_regex).

% Read file entity and docstring
entity(read_file).
docstring(read_file,
    {|string(_)||
    Read specific lines from a file with line numbers.
    Format: perceive(read_file(FilePath, LineNumbers, ContentWithLineNumbers))
    Parameters:
    - FilePath: Path to the file to read
    - LineNumbers: List of 1-based line numbers to read (negative numbers count from end: -1 = last line)
    - ContentWithLineNumbers: Unifies with list of line(Number, Content) terms
    Examples:
      read_file('file.txt', [1, 2, 3], Content)     % Read first 3 lines
      read_file('file.txt', [1, -1], Content)      % Read first and last line
      read_file('file.txt', [-3, -2, -1], Content) % Read last 3 lines
    |}).

% Generic entity and docstring rules for perceive and conjure constructors
entity(perceive(Ctor)) :- component(perceive, ctor, Ctor), entity(Ctor).
entity(conjure(Ctor)) :- component(conjure, ctor, Ctor), entity(Ctor).
docstring(perceive(Ctor), S) :- entity(perceive(Ctor)), docstring(Ctor, S).
docstring(conjure(Ctor), S) :- entity(conjure(Ctor)), docstring(Ctor, S).

% Spell system docstrings
docstring(spell,
    {|string(_)||
    The fundamental magic system of Grimoire.
    Spells are divided into two categories:
    - conjure: Mutable operations that change system state
    - perceive: Perception operations that query system state

    Format: spell(conjure(...)) or spell(perceive(...))
    |}).

docstring(conjure,
    {|string(_)||
    Conjuration spells that modify system state.
    Must be cast using cast/2 predicate for safety.

    Format: cast(conjure(operation(...)), Result)
    Examples:
      - cast(conjure(git(commit('message'))), Result)
      - cast(conjure(mkdir('path')), Result)
    |}).

docstring(perceive,
    {|string(_)||
    Perception spells that query system state without modification.
    Called directly, with variables unified to results.

    Format: perceive(query(Var1, Var2, ...))
    Examples:
      - perceive(git(status(Branch, Ahead, Files)))
      - perceive(nix(flake(show(Apps, Packages, DevShells))))
    |}).

% Concept docstrings
docstring(transaction,
    {|string(_)||
    Atomic units of change with rollback capability in the Grimoire system.
    Transactions ensure consistency through git-backed session management,
    allowing multiple operations to succeed or fail as a unit.
    Provides the foundation for safe system mutation with undo capabilities.
    |}).

docstring(hardware,
    {|string(_)||
    Hardware abstraction layer and system resource management.
    Represents physical and virtual hardware resources available to the system,
    including CPU, memory, storage, and device interfaces.
    Provides semantic representation of computing resources.
    |}).

docstring(execute,
    {|string(_)||
    Execution contexts and runtime environments within Grimoire.
    Manages the evaluation of operations, command execution, and process lifecycles.
    Bridges the semantic knowledge layer with actual system operations,
    ensuring proper context and environment for code execution.
    |}).

docstring(interface,
    {|string(_)||
    Command interfaces for system interaction.
    Provides structured access points to Grimoire functionality through
    CLI, REST API, MCP protocols, and REPL interfaces.
    Each interface maintains consistency while adapting to its medium's conventions.
    |}).

docstring(git,
    {|string(_)||
    Knowledge evolution tracking and version control subsystem.
    Manages version history, branching, and transactional rollback through git.
    Provides session management where each session is a git branch,
    enabling atomic changes and experimental workflows with easy rollback.
    Core to Grimoire's immutable knowledge architecture.
    |}).

docstring(nix,
    {|string(_)||
    Symbolic configuration, package management, and build subsystem.
    Provides reproducible environments and declarative system configuration through Nix flakes.
    Manages dependencies, build processes, and development shells with perfect reproducibility.
    Enables functional package management where builds are pure functions of their inputs.
    |}).

docstring(session,
    {|string(_)||
    Transaction and workspace management subsystem.
    Coordinates git branches with operational contexts for atomic system changes.
    Each session maintains its own workspace with SQLite command logs and state files.
    Provides commit, rollback, and history tracking for all operations within a session.
    |}).


docstring(conjure(executable_program),
    {|string(_)||
    Executes a program with arguments.
    Format:
      conjure(executable_program(Program, Args))           % Capture output mode
      conjure(executable_program(Program, Args, interactive)) % Interactive mode
    Program is the executable path or name
    Args is a list of arguments
    |}
).

cast(conjure(executable_program(Program, Args)), RetVal) :-
    % Non-interactive mode - capture output and exit code
    setup_call_cleanup(
        process_create(
            path(Program),
            Args,
            [stdout(pipe(Out)), stderr(pipe(Err)), process(PID)]
        ),
        % Read output and wait for process
        (read_string(Out, _, Stdout),
         read_string(Err, _, Stderr),
         process_wait(PID, exit(ExitCode))),
        % Cleanup
        (close(Out), close(Err))
    ),
    % Return structured result based on exit code
    (ExitCode = 0 ->
        RetVal = ok(result(Stdout, Stderr))
    ;
        RetVal = error(process_error(Program, exit(ExitCode), Stdout, Stderr))
    ).

cast(conjure(executable_program(Program, Args, interactive)), RetVal) :-
    % Interactive mode - pass through stdin/stdout
    setup_call_cleanup(
        process_create(
            path(Program),
            Args,
            [stdin(std), stdout(std), stderr(std)]
        ),
        true,  % Process runs interactively
        true   % No cleanup needed
    ),
    RetVal = ok("Interactive program completed").

docstring(conjure(shell),
    {|string(_)||
    Executes a shell command with arguments.
    Format:
      conjure(shell(Args))           % Capture output mode
      conjure(shell(Args, interactive)) % Interactive mode
    Args is a list of strings that will be properly escaped.
    Equivalent to: executable_program(sh, ["-c", JoinedArgs])
    where JoinedArgs is the properly escaped and joined argument list.
    |}
).

cast(conjure(shell(Args)), RetVal) :-
    join_args(Args, JoinedArgs),
    cast(
        conjure(executable_program(sh, ["-c", JoinedArgs])),
        RetVal
    ).

cast(conjure(shell(Args, interactive)), RetVal) :-
    join_args(Args, JoinedArgs),
    cast(
        conjure(executable_program(sh, ["-c", JoinedArgs], interactive)),
        RetVal
    ).

% Helper to join and escape args
join_args(Args, Cmd) :-
    maplist(shell_quote, Args, QuotedArgs),
    atomic_list_concat(QuotedArgs, ' ', Cmd).

shell_quote(Arg, Quoted) :-
    format(string(Quoted), "'~w'", [Arg]).

docstring(conjure(mkdir),
    {|string(_)||
    Creates a directory and initializes its semantic tracking.
    Format: conjure(mkdir(Path)).
    - Creates directory at Path
    - Creates semantics.pl inside it
    - Initializes directory entity in semantics.pl
    - If parent has semantics.pl, adds this dir as a component
    |}
).

cast(conjure(mkdir(Path)), RetVal) :-
    % Create directory
    cast(conjure(shell({|string(Path)||mkdir -p '{Path}'|})), RetVal),
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
    % Rest same as before
    directory_file_path(Parent, _, Path),
    directory_file_path(Parent, "semantics.pl", ParentSemantic),
    (exists_file(ParentSemantic) ->
        cast(conjure(edit_file(file(ParentSemantic), [
            append({|string(Path)||
            entity(folder('{Parent}')).
            component(folder('{Parent}'), subfolder, folder('{Path}')).
            |})
        ])), _)
    ; true).

docstring(conjure(mkfile),
    {|string(_)||
    Creates a file and updates semantic relationships.
    Format: conjure(mkfile(Path)).
    - Creates empty file at Path
    - If parent dir has semantics.pl, adds file as a component
    |}
).

cast(conjure(mkfile(Path)), RetVal) :-
    % Create empty file
    write_file(Path, ""),
    % Update parent semantics if exists
    directory_file_path(Parent, Name, Path),
    directory_file_path(Parent, "semantics.pl", ParentSemantic),
    (exists_file(ParentSemantic) ->
        cast(conjure(edit_file(file(ParentSemantic), [
            append({|string(Parent,Name)||
            entity(folder('{Parent}')).
            component(folder('{Parent}'), file, file('{Name}')).
            |})
        ])), _)
    ; true),
    RetVal = ok("").

docstring(conjure(edit_file), S) :-
    docstring(edit_file, EditFileDoc),
    S = {|string(EditFileDoc)||
    Applies edits to a file.
    Format: cast(conjure(edit_file(file(Path), [Edit1, Edit2, ...])), Result).

    edit_file structure:
    {EditFileDoc}
    |}.

cast(conjure(edit_file(file(Path), Edits)), RetVal) :-
    % Handle both existing and non-existing files
    (exists_file(Path) ->
        read_file_to_lines(Path, Lines)
    ;
        Lines = []
    ),
    maplist(validate_edit, Edits),
    apply_edits(Edits, Lines, NewLines),
    write_lines_to_file(Path, NewLines),
    RetVal = ok("").

validate_edit(Edit) :-
    functor(Edit, Type, _),
    component(edit_file, ctor, Type).

% File editing helpers
apply_edits([], Lines, Lines).
apply_edits([Edit|Rest], Lines, Final) :-
    apply_edit(Edit, Lines, Intermediate),
    apply_edits(Rest, Intermediate, Final).

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

% List splitting helper with proper implementation
% split_at(+N, +List, -Before, -After)
% Splits List at position N, Before gets first N elements, After gets rest
split_at(0, List, [], List) :- !.  % Cut for efficiency when N=0
split_at(N, [H|T], [H|Before], After) :-
    N > 0,
    N1 is N - 1,
    split_at(N1, T, Before, After).

% Transaction execution
execute(transaction(Commands), RetVal) :-
    execute_commands(Commands, Results),
    % If any command failed, return its error
    (member(error(E), Results) ->
        RetVal = error(E)
    ;
        RetVal = ok(Results)
    ).

% Spell casting system - replaces run/2 for mutable operations
docstring(cast,
    {|string(_)||
    Safely cast conjuration spells that modify system state.
    Supports both single spells and ritual (transaction) casting.

    Format:
      cast(conjure(operation(...)), Result)    % Single spell
      cast(ritual([op1, op2, ...]), Result)    % Atomic ritual

    Examples:
      - cast(conjure(git(commit('message'))), Result)
      - cast(ritual([mkdir('dir'), mkfile('dir/file')]), Result)
    |}).

% Remove this dispatcher - let individual files handle cast(conjure(...)) directly

cast(ritual(Operations), RetVal) :-
    % Cast multiple conjuration spells as a ritual (atomic transaction)
    maplist(cast, Operations, Results),
    RetVal = ok(Results).

execute_commands([], []).
execute_commands([Cmd|Rest], [Res|Results]) :-
    format("~w\n", [run(Cmd, Res)]),
    run(Cmd, Res),
    ( Res = error(_) ->
        Results = []
    ;
        execute_commands(Rest, Results)
    ).

docstring(write_file,
    {|string(_)||
    Writes content to a file.
    Format: write_file(Path, Content).
    Creates parent directories if they don't exist.
    |}
).

% Add directory existence checks to file operations
write_file(Path, Content) :-
    directory_file_path(Dir, _, Path),
    make_directory_path(Dir),
    setup_call_cleanup(
        open(Path, write, Stream),
        write(Stream, Content),
        close(Stream)
    ).

docstring(read_file_to_lines,
    {|string(_)||
    Reads a file into a list of lines.
    Format: read_file_to_lines(Path, Lines).
    |}
).

read_file_to_lines(Path, Lines) :-
    exists_file(Path),
    setup_call_cleanup(
        open(Path, read, Stream),
        read_string(Stream, _, String),
        close(Stream)
    ),
    string_lines(String, Lines).

docstring(write_lines_to_file,
    {|string(_)||
    Writes a list of lines to a file.
    Format: write_lines_to_file(Path, Lines).
    |}
).

write_lines_to_file(Path, Lines) :-
    atomic_list_concat(Lines, '\n', Content),
    write_file(Path, Content).

% Add list_mounted_semantics predicate
docstring(list_mounted_semantics,
    {|string(_)||
    Lists all currently mounted semantic modules.
    Format: list_mounted_semantics(Paths).
    Returns list of absolute paths to mounted semantic files.
    |}
).

% Update mkdir/mkfile to support options
cast(conjure(mkdir(Path, Options)), RetVal) :-
    cast(conjure(mkdir(Path)), RetVal),
    % Auto git-add if we're in a repo and not disabled
    (option(git(false), Options) ->
        true
    ;
        is_git_directory(Path) ->
            cast(conjure(git(add([Path]))), _)
        ;
        true
    ).

cast(conjure(mkfile(Path, Options)), RetVal) :-
    cast(conjure(mkfile(Path)), RetVal),
    % Auto git-add if we're in a repo and not disabled
    (option(git(false), Options) ->
        true
    ;
        is_git_directory(Path) ->
            cast(conjure(git(add([Path]))), _)
        ;
        true
    ).

% Edit file is now an entity with subcommands
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

% Agent subsystem
entity(agent).
component(agent, source, source(semantic(file("agent.pl")))).
component(system, subsystem, agent).

docstring(agent,
    {|string(_)||
    Agent subsystem for autonomous task execution.
    Manages agent operations and logging through agent_log constructors.
    Provides infrastructure for LLM-based agents to interact with the system.
    Components: source points to agent.pl implementation.
    |}).

% Define agent logging schema constructors
entity(agent_log).
component(agent_log, ctor, natural_language).
component(agent_log, ctor, tool_call).
component(agent_log, ctor, tool_result).
component(agent_log, ctor, user_input).
component(agent_log, ctor, return_value).

docstring(agent_log,
    {|string(_)||
    Log entry types for agent interactions.
    Matches the Thought hierarchy from tool_calling.py:
    - natural_language: Human-like reasoning steps
    - tool_call: Request to use a specific tool
    - tool_result: Result from tool execution
    - user_input: User's direct prompt.
    - return_value: Final conclusion/answer
    |}
).

% ========================================================================
% CORE PERCEIVE SPELLS
% ========================================================================

% Perceive all entities in the system
perceive(entities(Entities)) :-
    findall(Entity, entity(Entity), Entities).

% Read file with line numbers using 1-based indexing (-1 means from end)
perceive(read_file(FilePath, Lines, ContentWithLineNumbers)) :-
    read_file_to_lines(FilePath, AllLines),
    length(AllLines, TotalLines),
    % Handle different line specification formats
    resolve_lines_spec(Lines, TotalLines, ResolvedLines),
    % Extract requested lines with numbers
    findall(line(LineNum, Content),
        (member(LineNum, ResolvedLines),
         nth1(LineNum, AllLines, Content)),
        ContentWithLineNumbers).

% Resolve different line specification formats
resolve_lines_spec(lines(Start, End), TotalLines, ResolvedLines) :-
    % Handle lines(start, end) format from tests
    resolve_line_bound(Start, TotalLines, StartNum),
    resolve_line_bound(End, TotalLines, EndNum),
    findall(LineNum, between(StartNum, EndNum, LineNum), ResolvedLines).
resolve_lines_spec(Lines, TotalLines, ResolvedLines) :-
    % Handle direct list of line numbers
    is_list(Lines),
    maplist(resolve_line_index(TotalLines), Lines, ResolvedLines).

% Resolve line bounds (start/end atoms or numbers)
resolve_line_bound(start, _, 1) :- !.
resolve_line_bound(end, TotalLines, TotalLines) :- !.
resolve_line_bound(Num, _, Num) :- number(Num).

% Helper to resolve line indices (1-based, -1 from end)
resolve_line_index(Total, Index, Resolved) :-
    (Index < 0 ->
        Resolved is Total + Index + 1  % -1 becomes Total, -2 becomes Total-1, etc.
    ;
        Resolved = Index
    ).

% Helper for extracting lines with numbers
extract_lines_with_numbers([], _, _, _, []) :- !.
extract_lines_with_numbers(_, EndLine, EndLine, Current, []) :-
    Current > EndLine, !.
extract_lines_with_numbers([Line|Rest], StartLine, EndLine, Current, Result) :-
    (Current >= StartLine, Current =< EndLine ->
        Result = [line(Current, Line)|RestResult]
    ;
        Result = RestResult
    ),
    Next is Current + 1,
    extract_lines_with_numbers(Rest, StartLine, EndLine, Next, RestResult).

% Search for regex pattern in content with line numbers
perceive(search_regex(ContentWithLineNumbers, Pattern, FoundContent)) :-
    findall(line(Num, Line),
        (member(line(Num, Line), ContentWithLineNumbers),
         re_match(Pattern, Line)),
        FoundContent).

% ========================================================================
% GRIMOIRE CORE SEMANTIC SYSTEM
% ========================================================================
% This file contains only the core semantic system.
% User interfaces are implemented separately:
% - repl.pl: Interactive REPL
% - api.pl: HTTP API (future)
% - mcp.pl: Model Context Protocol (future)
