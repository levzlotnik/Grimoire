:- use_module(library(process)).
:- use_module(library(strings)).

% Load ECS core first
:- ensure_loaded("./ecs.pl").

% Dynamic declarations specific to core_rules
:- dynamic execute/2.   % execute(Transaction, Status)
:- dynamic([run/2], [discontiguous(true), multifile(true)]).       % run(Command, RetVal)
:- dynamic mounted_semantic/2.  % mounted_semantic(Path, Module)

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
    fetch_all_commands(CommandTypes),
    maplist(docstring, CommandTypes, Docs),
    format_command_docstrings_together(CommandTypes, Docs, OneDocstring),
    indent_lines('  ', OneDocstring, CmdsDocs),
    S = {|string(CmdsDocs)||
    Term structure for system operations.
    Available commands:

    {CmdsDocs}
    |}.

% Helper predicates for command discovery
fetch_all_commands(CmdTypes) :-
    findall(CmdType, component(command, ctor, CmdType), CmdTypes).

format_command_docstrings_together(Types, Docs, Formatted) :-
    maplist(format_command_entry, Types, Docs, Entries),
    atomic_list_concat(Entries, '\n', Formatted).

format_command_entry(Type, Doc, Entry) :-
    indent_lines('  ', Doc, IDoc),
    format(string(Entry), "~w:\n~w", [Type, IDoc]).

% Just commands as a sum type
entity(command).
component(command, ctor, shell).
component(command, ctor, mkdir).
component(command, ctor, mkfile).
component(command, ctor, edit_file).
component(command, ctor, executable_program).

docstring(executable_program,
    {|string(_)||
    Executes a program with arguments.
    Format: command(executable_program(Program, Args)).
    Program is the executable path or name
    Args is a list of arguments
    |}
).

docstring(shell,
    {|string(_)||
    Executes a shell command with arguments.
    Format: command(shell(Args)).
    Args is a list of strings that will be properly escaped.
    Equivalent to: executable_program(path(sh), ["-c", JoinedArgs])
    where JoinedArgs is the properly escaped and joined argument list.
    |}
).

% Add new command docstrings
docstring(mkdir,
    {|string(_)||
    Creates a directory and initializes its semantic tracking.
    Format: command(mkdir(Path)).
    - Creates directory at Path
    - Creates semantics.pl inside it
    - Initializes directory entity in semantics.pl
    - If parent has semantics.pl, adds this dir as a component
    |}
).

docstring(mkfile,
    {|string(_)||
    Creates a file and updates semantic relationships.
    Format: command(mkfile(Path)).
    - Creates empty file at Path
    - If parent dir has semantics.pl, adds file as a component
    |}
).

docstring(edit_file,
    {|string(_)||
    Applies edits to a file.
    Format: command(edit_file(file(Path), [Edit1, Edit2, ...])).

    Edit terms:
    - insert(Line, Content)          % Insert at line number
    - delete(Start, End)             % Delete range of lines
    - replace(Start, End, Content)    % Replace range of lines with content
    |}
).

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

run(command(executable_program(Program, Args)), RetVal) :-
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
        RetVal = error(Stderr)
    ).

% Shell command now uses executable_program
run(command(shell(Args)), RetVal) :-
    join_args(Args, JoinedArgs),
    run(
        command(executable_program(path(sh), ["-c", JoinedArgs])),
        RetVal
    ).

% Helper to join and escape args
join_args(Args, Cmd) :-
    maplist(shell_quote, Args, QuotedArgs),
    atomic_list_concat(QuotedArgs, ' ', Cmd).

shell_quote(Arg, Quoted) :-
    format(string(Quoted), "'~w'", [Arg]).

% Add command implementations
run(command(mkdir(Path)), RetVal) :-
    % Create directory
    run(command(shell({|string(Path)||mkdir -p '{Path}'|})), RetVal),
    % Initialize semantics.pl with proper module
    atomic_list_concat([Path, '/semantics.pl'], SemanticFile),
    InitContent = {|string(Path)||
    :- module(semantic_{Path}, [entity/1, component/3]).

    % Dynamic declarations for this module
    :- dynamic entity/1.
    :- dynamic component/3.

    % This directory's entity
    entity(folder('{Path}')).
    |},
    write_file(SemanticFile, InitContent),
    % Rest same as before
    directory_file_path(Parent, _, Path),
    atomic_list_concat([Parent, '/semantics.pl'], ParentSemantic),
    (exists_file(ParentSemantic) ->
        consult(ParentSemantic),
        entity(folder(Parent)),
        assert(component(folder(Parent), subfolder, folder(Path)))
    ; true).

run(command(mkfile(Path)), RetVal) :-
    % Create empty file
    write_file(Path, ""),
    % Update parent semantics if exists
    directory_file_path(Parent, Name, Path),
    atomic_list_concat([Parent, '/semantics.pl'], ParentSemantic),
    (exists_file(ParentSemantic) ->
        consult(ParentSemantic),
        entity(folder(Parent)),
        assert(component(folder(Parent), file, file(Name)))
    ; true),
    RetVal = ok("").

run(command(edit_file(file(Path), Edits)), RetVal) :-
    read_file_to_lines(Path, Lines),
    apply_edits(Edits, Lines, NewLines),
    write_lines_to_file(Path, NewLines),
    RetVal = ok("").

% File editing helpers
apply_edits([], Lines, Lines).
apply_edits([Edit|Rest], Lines, Final) :-
    apply_edit(Edit, Lines, Intermediate),
    apply_edits(Rest, Intermediate, Final).

apply_edit(insert(N, Content), Lines, Result) :-
    length(Lines, Len),
    N > 0, N =< Len + 1,  % Allow insert at end
    % Split lines at insertion point
    split_at(N-1, Lines, Before, After),
    % Split new content into lines
    string_lines(Content, NewLines),
    % Combine parts
    append([Before, NewLines, After], Result).

apply_edit(delete(Start, End), Lines, Result) :-
    length(Lines, Len),
    Start > 0, Start =< Len,
    End >= Start, End =< Len,
    % Split into before, [to_delete], after
    split_at(Start-1, Lines, Before, Rest),
    NDel is End - Start + 1,
    split_at(NDel, Rest, _, After),
    % Combine before and after
    append(Before, After, Result).

apply_edit(replace(Start, End, Content), Lines, Result) :-
    length(Lines, Len),
    Start > 0, Start =< Len,
    End >= Start, End =< Len,
    % Split lines into before, to_replace, and after
    split_at(Start-1, Lines, Before, Rest),
    NDel is End - Start + 1,
    split_at(NDel, Rest, _, After),
    % Split new content into lines
    string_lines(Content, NewLines),
    % Combine parts
    append([Before, NewLines, After], Result).

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
    run(Cmd, Res),
    ( Res = error(_) ->
        Results = []
    ;
        execute_commands(Rest, Results)
    ).

docstring(mount_semantic,
    {|string(_)||
    Mounts a semantics.pl file for querying.
    Format: mount_semantic(Path).
    - Loads the module at Path
    - Creates a unique module name based on path
    - Tracks the mounting in mounted_semantic/2
    |}
).

docstring(unmount_semantic,
    {|string(_)||
    Unmounts a previously mounted semantics.pl file.
    Format: unmount_semantic(Path).
    - Unloads the module at Path
    - Removes mounting from mounted_semantic/2
    |}
).

% Add docstrings for query predicates
docstring(find_entity,
    {|string(_)||
    Finds entities across all mounted semantic modules.
    Format: find_entity(Thing).
    Succeeds for each entity found in any mounted semantic module.
    |}
).

docstring(find_component,
    {|string(_)||
    Finds components across all mounted semantic modules.
    Format: find_component(Entity, Name, Value).
    Succeeds for each component relationship found in any mounted module.
    |}
).

% Enhanced semantic mounting predicates
mount_semantic(Path) :-
    % Get absolute path and check file exists
    absolute_file_name(Path, AbsPath),
    (exists_file(AbsPath) ->
        % Only mount if not already mounted
        (\+ mounted_semantic(AbsPath, _) ->
            % Load the semantic file
            catch(
                ensure_loaded(AbsPath),
                Error,
                (print_message(error, Error), fail)
            ),
            % Record the mounting
            assertz(mounted_semantic(AbsPath, AbsPath))
        ;
            true  % Already mounted, silently succeed
        )
    ;
        throw(error(existence_error(source_sink, Path), _))
    ).

unmount_semantic(Path) :-
    absolute_file_name(Path, AbsPath),
    mounted_semantic(AbsPath, _),
    % Unload the file properly
    unload_file(AbsPath),
    % Remove from mounting registry
    retractall(mounted_semantic(AbsPath, _)).

% Query across all mounted semantics
find_entity(Thing) :-
    mounted_semantic(_, Module),
    Module:entity(Thing).

find_component(Entity, Name, Value) :-
    mounted_semantic(_, Module),
    Module:component(Entity, Name, Value).

% Add docstrings for file operations
docstring(write_file,
    {|string(_)||
    Writes content to a file.
    Format: write_file(Path, Content).
    Creates parent directories if they don't exist.
    |}
).

docstring(read_file_to_lines,
    {|string(_)||
    Reads a file into a list of lines.
    Format: read_file_to_lines(Path, Lines).
    |}
).

docstring(write_lines_to_file,
    {|string(_)||
    Writes a list of lines to a file.
    Format: write_lines_to_file(Path, Lines).
    |}
).

% Add list_mounted_semantics predicate
docstring(list_mounted_semantics,
    {|string(_)||
    Lists all currently mounted semantic modules.
    Format: list_mounted_semantics(Paths).
    Returns list of absolute paths to mounted semantic files.
    |}
).

list_mounted_semantics(Paths) :-
    findall(Path, mounted_semantic(Path, _), Paths).

% Add directory existence checks to file operations
write_file(Path, Content) :-
    directory_file_path(Dir, _, Path),
    make_directory_path(Dir),
    setup_call_cleanup(
        open(Path, write, Stream),
        write(Stream, Content),
        close(Stream)
    ).

read_file_to_lines(Path, Lines) :-
    exists_file(Path),
    setup_call_cleanup(
        open(Path, read, Stream),
        read_string(Stream, _, String),
        close(Stream)
    ),
    string_lines(String, Lines).

% Add new command type
component(command, ctor, mkproject).

docstring(mkproject,
    {|string(_)||
    Creates a new project directory with full initialization.
    Format: command(mkproject(+Path, +Options)).
    Options:
    - git(bool)          % Initialize git repo (default: true)
    - template(Template) % Flake template to use (default: none)
    - lang(Language)     % Programming language (affects template)
    |}
).


% Update mkdir/mkfile to support options
run(command(mkdir(Path, Options)), RetVal) :-
    run(command(mkdir(Path)), RetVal),
    % Auto git-add if we're in a repo and not disabled
    (option(git(false), Options) ->
        true
    ;
        in_git_repository(Path) ->
            run(command(git(add(Path))), _)
        ;
        true
    ).

run(command(mkfile(Path, Options)), RetVal) :-
    run(command(mkfile(Path)), RetVal),
    % Auto git-add if we're in a repo and not disabled
    (option(git(false), Options) ->
        true
    ;
        in_git_repository(Path) ->
            run(command(git_add(Path)), _)
        ;
        true
    ).

% Load extensions last
:- ensure_loaded("./git.pl").

