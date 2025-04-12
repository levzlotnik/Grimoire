% Load core rules first
:- ensure_loaded("./core_rules.pl").
:- use_module(library(filesex)).

% Dynamic declarations specific to core_rules
:- dynamic execute/2.   % execute(Transaction, Status)
:- dynamic([run/2], [discontiguous(true), multifile(true)]).       % run(Command, RetVal)

% The most fundamental entity
entity(system).

docstring(system, {|string(_)||
    The root entity of the entire system - the system itself.
|}).


component(system, root_dir, folder("/home/nixos/Projects/MyPAOS")) :- !.
% Some properties
component(
    system,
    semantic_root,
    folder(AbsSemRoot)
) :-
    RelSemRoot = "src/prolog",
    component(system, root_dir, folder(SysRoot)),
    directory_file_path(SysRoot, RelSemRoot, AbsSemRoot), !.

system_semantic_root(SemanticRootDir) :-
    component(system, semantic_root, SemanticRootDir).

component(E, semantic_root, Path) :-
    component(E, source, source(semantic(RelPath))),
    system_semantic_root(folder(SysSemRoot)),
    (
        RelPath = file(RawPath) ->
            (directory_file_path(SysSemRoot, RawPath, AbsPath), Path = file(AbsPath)) ;
        RelPath = folder(RawPath) ->
            (directory_file_path(SysSemRoot, RawPath, AbsPath), Path = folder(AbsPath)) ;
        fail
    ).

load_entity_source(Entity) :-
    component(Entity, semantic_root, SemanticPath),
    mount_semantic(SemanticPath).


% Fundamental concepts
component(system, concept, command).
component(system, concept, transaction).
component(system, concept, hardware).
component(system, concept, execute).
component(system, concept, source).
component(system, concept, project).

component(system, source, source(semantic(file("mypaos.pl")))) :- !.

entity(source).
component(source, ctor, semantic).
entity(semantic).
component(semantic, ctor, file).
component(semantic, ctor, folder).
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

% System components and their sources
entity(git).
component(git, source, source(semantic(file("git.pl")))) :- !.
component(system, subsystem, git).

entity(nix).
component(nix, source, source(semantic(folder("nix")))) :- !.
component(system, subsystem, nix).

entity(project).
component(project, source, source(semantic(folder("project")))) :- !.

:- load_entity_source(git).
:- load_entity_source(nix).

docstring(execute,
    {|string(_)||
    Executes a transaction (list of commands) sequentially.
    If any command fails, returns its error.
    Format: execute(transaction([command(...),...]), RetVal).
    RetVal will be either ok([Results]) or error(Error)
    |}
).

docstring(transaction,
    {|string(_)||
    Term structure for atomic operations.
    Format: transaction(CommandList).
    Not a predicate, but a wrapper for a list of commands
    |}
).

docstring(command, S) :-
    make_ctors_docstring(command, CmdsDocs),
    S = {|string(CmdsDocs)||
    Term structure for system operations.
    Available commands:

    {CmdsDocs}
    |}.

% Just commands as a sum type
entity(command).
component(command, ctor, shell).
component(command, ctor, mkdir).
component(command, ctor, mkfile).
component(command, ctor, edit_file).
component(command, ctor, executable_program).

% Dynamic docstring for run based on command type
docstring(run(command(Command)), Doc) :-
    functor(Command, Type, _),
    docstring(Type, CmdDoc),
    indent_lines('  ', CmdDoc, CmdDocN),
    Doc = {|string(Type, CmdDocN)||
    Executes a '{Type}' command and returns its result.

    Command Details:
    {CmdDocN}

    Returns:
      - ok(Output) on success
      - error(Error) on failure
    |}.


docstring(command(executable_program),
    {|string(_)||
    Executes a program with arguments.
    Format:
      command(executable_program(Program, Args))           % Capture output mode
      command(executable_program(Program, Args, interactive)) % Interactive mode
    Program is the executable path or name
    Args is a list of arguments
    |}
).

run(command(executable_program(Program, Args)), RetVal) :-
    % Non-interactive mode - capture output
    setup_call_cleanup(
        process_create(
            Program,
            Args,
            [stdout(pipe(Out)), stderr(pipe(Err))]
        ),
        % Read output
        (read_string(Out, _, Stdout),
         read_string(Err, _, Stderr)),
        % Cleanup
        (close(Out), close(Err))
    ),
    % Return result
    (Stderr = "" ->
        RetVal = ok(Stdout)
    ;
        % RetVal = error(Stderr) % this is wrong...
        true
    ).

run(command(executable_program(Program, Args, interactive)), RetVal) :-
    % Interactive mode - pass through stdin/stdout
    setup_call_cleanup(
        process_create(
            Program,
            Args,
            [stdin(std), stdout(std), stderr(std)]
        ),
        true,  % Process runs interactively
        true   % No cleanup needed
    ),
    RetVal = ok("Interactive program completed").

docstring(command(shell),
    {|string(_)||
    Executes a shell command with arguments.
    Format:
      command(shell(Args))           % Capture output mode
      command(shell(Args, interactive)) % Interactive mode
    Args is a list of strings that will be properly escaped.
    Equivalent to: executable_program(path(sh), ["-c", JoinedArgs])
    where JoinedArgs is the properly escaped and joined argument list.
    |}
).

run(command(shell(Args)), RetVal) :-
    join_args(Args, JoinedArgs),
    run(
        command(executable_program(path(sh), ["-c", JoinedArgs])),
        RetVal
    ).

run(command(shell(Args, interactive)), RetVal) :-
    join_args(Args, JoinedArgs),
    run(
        command(executable_program(path(sh), ["-c", JoinedArgs], interactive)),
        RetVal
    ).

% Helper to join and escape args
join_args(Args, Cmd) :-
    maplist(shell_quote, Args, QuotedArgs),
    atomic_list_concat(QuotedArgs, ' ', Cmd).

shell_quote(Arg, Quoted) :-
    format(string(Quoted), "'~w'", [Arg]).

docstring(command(mkdir),
    {|string(_)||
    Creates a directory and initializes its semantic tracking.
    Format: command(mkdir(Path)).
    - Creates directory at Path
    - Creates semantics.pl inside it
    - Initializes directory entity in semantics.pl
    - If parent has semantics.pl, adds this dir as a component
    |}
).

run(command(mkdir(Path)), RetVal) :-
    % Create directory
    run(command(shell({|string(Path)||mkdir -p '{Path}'|})), RetVal),
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
        run(command(edit_file(file(ParentSemantic), [
            append({|string(Path)||
            entity(folder('{Parent}')).
            component(folder('{Parent}'), subfolder, folder('{Path}')).
            |})
        ])), _)
    ; true).

docstring(command(mkfile),
    {|string(_)||
    Creates a file and updates semantic relationships.
    Format: command(mkfile(Path)).
    - Creates empty file at Path
    - If parent dir has semantics.pl, adds file as a component
    |}
).

run(command(mkfile(Path)), RetVal) :-
    % Create empty file
    write_file(Path, ""),
    % Update parent semantics if exists
    directory_file_path(Parent, Name, Path),
    directory_file_path(Parent, "semantics.pl", ParentSemantic),
    (exists_file(ParentSemantic) ->
        run(command(edit_file(file(ParentSemantic), [
            append({|string(Parent,Name)||
            entity(folder('{Parent}')).
            component(folder('{Parent}'), file, file('{Name}')).
            |})
        ])), _)
    ; true),
    RetVal = ok("").

docstring(command(edit_file), S) :-
    make_ctors_docstring(edit_file, SubCmdsDoc),
    S = {|string(SubCmdsDoc)||
    Applies edits to a file.
    Format: command(edit_file(file(Path), [Edit1, Edit2, ...])).

    Edit terms:
    {SubCmdsDoc}
    |}.

run(command(edit_file(file(Path), Edits)), RetVal) :-
    read_file_to_lines(Path, Lines),
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
run(command(mkdir(Path, Options)), RetVal) :-
    run(command(mkdir(Path)), RetVal),
    % Auto git-add if we're in a repo and not disabled
    (option(git(false), Options) ->
        true
    ;
        is_git_directory(Path) ->
            run(command(git(add([Path]))), _)
        ;
        true
    ).

run(command(mkfile(Path, Options)), RetVal) :-
    run(command(mkfile(Path)), RetVal),
    % Auto git-add if we're in a repo and not disabled
    (option(git(false), Options) ->
        true
    ;
        is_git_directory(Path) ->
            run(command(git(add([Path]))), _)
        ;
        true
    ).

% Edit file is now an entity with subcommands
entity(edit_file).
component(edit_file, ctor, insert).
component(edit_file, ctor, delete).
component(edit_file, ctor, replace).
component(edit_file, ctor, append).

% Agent subsystem
entity(agent).
component(agent, source, source(semantic(file("agent.pl")))) :- !.
component(system, subsystem, agent).

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
