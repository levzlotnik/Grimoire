:- use_module(library(strings)).

% Git entity declaration with automatic self-location
:- self_entity(git).

component(command, ctor, git).
component(git, subcommand, clone).
component(git, subcommand, init).
component(git, subcommand, add).
component(git, subcommand, commit).
component(git, subcommand, push).
component(git, subcommand, pull).
component(git, subcommand, branch).
component(git, subcommand, checkout).
component(git, subcommand, status).
component(git, subcommand, diff).
component(git, subcommand, log).
component(git, subcommand, rev_parse).
component(git, subcommand, reset).
component(git, subcommand, merge).

% Separate mutable vs query operations
% Conjure constructors (state-changing operations)
component(conjure, ctor, git(clone)).
component(conjure, ctor, git(init)).
component(conjure, ctor, git(add)).
component(conjure, ctor, git(commit)).
component(conjure, ctor, git(push)).
component(conjure, ctor, git(pull)).
component(conjure, ctor, git(checkout)).
component(conjure, ctor, git(reset)).
component(conjure, ctor, git(merge)).

% Perceive constructors (query operations)
component(perceive, ctor, git(status)).
component(perceive, ctor, git(diff)).
component(perceive, ctor, git(log)).
component(perceive, ctor, git(branch)).
component(perceive, ctor, git(rev_parse)).

% Legacy support - mark those as ctors:
component(git, ctor, C) :- component(git, subcommand, C).
% Make these available to top level command for backwards compatibility:
component(command, ctor, git(C)) :- component(git, subcommand, C).

% Git command docstrings
docstring(git(clone),
    {|string(_)||
    Clone a repository into a new directory.
    Format: git(clone(Url, Path))
      Url: Remote repository URL
      Path: Local directory to clone into
    |}
).

docstring(git(init),
    {|string(_)||
    Create an empty Git repository.
    Format: git(init(Path))
      Path: Directory to initialize
    |}
).

docstring(git(add),
    {|string(_)||
    Add file(s) to the index.
    Format: git(add(Paths)) or git(add(all_tracked))
      Paths: List of file paths to stage
      all_tracked: Update all tracked files (-u flag)
    |}
).

docstring(git(commit),
    {|string(_)||
    Record changes to the repository.
    Format: git(commit(Message))
      Message: String containing commit message
    |}
).

docstring(git(push),
    {|string(_)||
    Update remote refs along with objects.
    Format: git(push(Remote, Branch))
      Remote: Remote name (e.g. "origin")
      Branch: Branch name to push
    |}
).

docstring(git(pull),
    {|string(_)||
    Fetch and integrate with another repository/branch.
    Format: git(pull(Remote, Branch))
      Remote: Remote name (e.g. "origin")
      Branch: Branch name to pull
    |}
).

docstring(git(branch),
    {|string(_)||
    Manage git branches.
    Format: git(branch(Operation))
      Operations:
        list         - List all branches
        create(Name) - Create new branch named Name
    |}
).

docstring(git(checkout),
    {|string(_)||
    Switch branches or restore working tree files.
    Format: git(checkout(Branch))
      Branch: Name of branch to switch to
    |}
).

docstring(git(status),
    {|string(_)||
    Show the working tree status.
    Format: git(status)
    |}
).

docstring(git(diff),
    {|string(_)||
    Show changes between commits, commit and working tree, etc.
    Format: git(diff)
    |}
).

docstring(git(log),
    {|string(_)||
    Show the commit logs.
    Format: git(log)
    |}
).

docstring(git(rev_parse),
    {|string(_)||
    Parse and verify Git revision/object names.
    Format: git(rev_parse(Args))
    |}
).

docstring(git(reset),
    {|string(_)||
    Reset current HEAD to the specified state.
    Format: git(reset(Args))
    |}
).

docstring(git(merge),
    {|string(_)||
    Join two or more development histories together.
    Format: git(merge(Args))
    |}
).

docstring(git, S) :-
    make_ctors_docstring(git, CtorsDoc),
    S = {|string(CtorsDoc)||
    Git commands suite.
    Format: git(subcommand(...))

    Possible subcommands:
    {CtorsDoc}
    |}.

% Git command implementations
git_args(clone(Url, Path)) --> ["clone", Url, Path].
git_args(init(Path)) --> ["init", Path].
git_args(add(all_tracked)) --> ["add", "-u"].
git_args(add(Paths)) -->
    { is_list(Paths) },  % Validate Paths is a list
    ["add"|Paths].
git_args(commit(Message)) --> ["commit", "-m", Message].
git_args(push(Remote, Branch)) --> ["push", Remote, Branch].
git_args(pull(Remote, Branch)) --> ["pull", Remote, Branch].
git_args(branch(list)) --> ["branch"].
git_args(branch(create(Name))) --> ["branch", Name].
git_args(branch(Args)) --> ["branch" | Args].  % Handle branch with args list
git_args(checkout(Branch)) --> ["checkout", Branch].
git_args(checkout(Args)) --> ["checkout" | Args].  % Handle checkout with args list
git_args(status) --> ["status"].
git_args(status(Args)) --> ["status" | Args].  % Handle status with args list
git_args(diff) --> ["diff"].
git_args(log) --> ["log"].
git_args(log(Args)) --> ["log" | Args].  % Handle log with args list
git_args(rev_parse(Args)) --> ["rev-parse" | Args].
git_args(reset(Args)) --> ["reset" | Args].
git_args(merge(Args)) --> ["merge" | Args].
git_args(commit(Args)) --> ["commit" | Args].  % Handle commit with args list

% Main git command implementation (legacy)
run(command(git(Term)), RetVal) :-
    % Just validate the subcommand type exists
    functor(Term, SubCmdType, _),
    component(git, subcommand, SubCmdType),
    % Convert to shell args
    phrase(git_args(Term), Args),
    % Execute
    run(
        command(executable_program(path(git), Args)),
        RetVal
    ).

% === PERCEIVE PREDICATES - Structured Git Queries ===

% Git status perception - parse status into structured data
perceive(git(status(Branch, WorkingStatus, Files))) :-
    % Get current branch
    run(command(git(branch(['--show-current']))), BranchResult),
    (BranchResult = ok(result(BranchOutput, _)) ->
        string_concat(BranchStr, "\n", BranchOutput),
        atom_string(Branch, BranchStr)
    ;
        Branch = unknown
    ),
    % Get working tree status
    run(command(git(status(['--porcelain']))), StatusResult),
    (StatusResult = ok(result(StatusOutput, _)) ->
        (StatusOutput = "" ->
            WorkingStatus = clean,
            Files = []
        ;
            WorkingStatus = dirty,
            parse_git_status_output(StatusOutput, Files)
        )
    ;
        WorkingStatus = unknown,
        Files = []
    ).

% Parse git status --porcelain output into structured file list
parse_git_status_output("", []).
parse_git_status_output(Output, Files) :-
    string_lines(Output, Lines),
    maplist(parse_status_line, Lines, Files).

% Parse individual status lines like " M file.txt" or "?? newfile.txt"
parse_status_line(Line, FileStatus) :-
    atom_codes(Line, [S1, S2, 32 | FileCodes]),  % 32 is space
    atom_codes(File, FileCodes),
    status_code_to_term([S1, S2], File, FileStatus).

% Map git status codes to clean file status terms
status_code_to_term([32, 77], File, modified(File)).     % " M" - modified in working tree
status_code_to_term([77, 32], File, staged(File)).       % "M " - staged for commit
status_code_to_term([77, 77], File, modified(File)).     % "MM" - modified in both (treat as modified)
status_code_to_term([63, 63], File, created(File)).      % "??" - untracked (new file)
status_code_to_term([65, 32], File, created(File)).      % "A " - added to index (new file)
status_code_to_term([68, 32], File, deleted(File)).      % "D " - deleted from index
status_code_to_term([32, 68], File, deleted(File)).      % " D" - deleted in working tree
status_code_to_term(_, File, unknown(File)).

% Example git subsystem extension for load_entity
% This could handle git repository cloning and then loading semantics
% load_entity(Entity, semantic(git(repo(URL)))) :-
%     % Clone repository to local path
%     format(atom(LocalPath), '/tmp/git_clone_~w', [Entity]),
%     git_clone(URL, LocalPath),
%     % Load the cloned semantics
%     atomic_list_concat([LocalPath, '/semantics.pl'], SemanticFile),
%     load_entity(Entity, semantic(file(SemanticFile))).
