:- use_module(library(strings)).

% Git entity declaration with automatic self-location
:- self_entity(git).

% Git command entities (manual declarations for commands without register_spell/4)
% Note: git(status), git(ls_files), git(current_branch) entities auto-generated from register_spell/4
entity(git(clone)).
entity(git(init)).
entity(git(add)).
entity(git(commit)).
entity(git(push)).
entity(git(pull)).
entity(git(checkout)).
entity(git(reset)).
entity(git(merge)).
entity(git(diff)).
entity(git(log)).
entity(git(branch)).
entity(git(rev_parse)).
entity(git(remote)).

% Removed legacy command ctor - using perceive/conjure above
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
component(git, subcommand, remote).

% Spell constructors (manual declarations for commands without register_spell/4)
% Note: perceive(git(status)), perceive(git(ls_files)), perceive(git(current_branch))
%       are auto-generated from register_spell/4
component(conjure, ctor, git(clone)).
component(conjure, ctor, git(init)).
component(conjure, ctor, git(add)).
component(conjure, ctor, git(commit)).
component(conjure, ctor, git(push)).
component(conjure, ctor, git(pull)).
component(conjure, ctor, git(checkout)).
component(conjure, ctor, git(reset)).
component(conjure, ctor, git(merge)).
component(perceive, ctor, git(diff)).
component(perceive, ctor, git(log)).
component(perceive, ctor, git(branch)).
component(perceive, ctor, git(rev_parse)).

% Legacy support - keep git namespace ctors:
component(git, ctor, C) :- component(git, subcommand, C).
% Removed command ctor compatibility - using perceive/conjure above

% === DSL PATTERNS - has(git(...)) fact schemas ===

% Repository remote expansion
component(Entity, git_repository_remote_name, RemoteName) :-
    component(Entity, has(git(repository)), git(repository(Spec))),
    member(remote(RemoteName, _), Spec).

component(Entity, git_repository_remote_url, RemoteURL) :-
    component(Entity, has(git(repository)), git(repository(Spec))),
    member(remote(_, RemoteURL), Spec).

% Repository branch expansion
component(Entity, git_repository_branch, Branch) :-
    component(Entity, has(git(repository)), git(repository(Spec))),
    member(branch(Branch), Spec).

% Repository working tree status expansion
component(Entity, git_repository_clean, Clean) :-
    component(Entity, has(git(repository)), git(repository(Spec))),
    member(clean(Clean), Spec).

% Repository root path expansion
component(Entity, git_repository_root, Root) :-
    component(Entity, has(git(repository)), _),
    component(Entity, self, semantic(folder(Root))).

component(Entity, git_repository_root, Root) :-
    component(Entity, has(git(repository)), _),
    component(Entity, self, semantic(file(FilePath))),
    file_directory_name(FilePath, Root).

% Auto-detect repository properties from filesystem
component(Entity, git_repository_current_branch, CurrentBranch) :-
    component(Entity, git_repository_root, Root),
    cast(perceive(git(current_branch(Root, CurrentBranch))), ok(_)).

component(Entity, git_repository_working_status, WorkingStatus) :-
    component(Entity, git_repository_root, _Root),
    cast(perceive(git(status(_, WorkingStatus, _))), ok(_)).

% Repository verification flag
component(Entity, git_repository_verified, true) :-
    component(Entity, has(git(repository)), git(repository(_))),
    component(Entity, git_repository_root, Root),
    exists_directory(Root),
    atomic_list_concat([Root, '/.git'], GitDir),
    exists_directory(GitDir).

% Repository state synchronization
component(Entity, git_repository_sync_status, Status) :-
    component(Entity, has(git(repository)), git(repository(_Spec))),
    component(Entity, git_repository_current_branch, CurrentBranch),
    component(Entity, git_repository_branch, ExpectedBranch),
    (CurrentBranch = ExpectedBranch ->
        Status = synchronized
    ;
        Status = desynchronized(expected(ExpectedBranch), actual(CurrentBranch))
    ).

% === SPELL IMPLEMENTATIONS ===
% Each register_spell is placed right above its cast implementation

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

docstring(git(ls_files),
    {|string(_)||
    List all tracked files in a git repository.
    Format: perceive(git(ls_files(Directory, FileList)))
      Directory: Path to the git repository
      FileList: List of all tracked files (relative paths)
    |}
).

docstring(git(remote),
    {|string(_)||
    Manage set of tracked repositories.
    Format: git(remote(Args))
      Args: List of arguments to pass to git remote
      Examples:
        git(remote([]))           - List all remotes
        git(remote(['show', 'origin'])) - Show details for origin
        git(remote(['get-url', 'origin'])) - Get URL for origin
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
git_args(remote(Args)) --> ["remote" | Args].  % Handle remote with args list


% Git conjure implementations
cast(conjure(git(Term)), RetVal) :-
    % Just validate the subcommand type exists
    functor(Term, SubCmdType, _),
    component(git, subcommand, SubCmdType),
    % Convert to shell args
    phrase(git_args(Term), Args),
    % Execute
    cast(conjure(executable_program(git, Args)), RetVal).

% === PERCEIVE PREDICATES - Structured Git Queries ===

% Status perception spell
register_spell(
    perceive(git(status)),
    input(git(status)),
    output(ok(repository_status(branch('Branch'), working_status('Status'), files('Files')))),
    docstring("Query git repository status including current branch, working tree status, and file changes.")
).

cast(perceive(git(status)), Result) :-
    catch(
        (% Get current branch
         cast(conjure(git(branch(['--show-current']))), BranchResult),
         (BranchResult = ok(result(BranchOutput, _)) ->
             string_concat(BranchStr, "\n", BranchOutput),
             atom_string(Branch, BranchStr)
         ;
             Branch = unknown
         ),
         % Get working tree status
         cast(conjure(git(status(['--porcelain']))), StatusResult),
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
         ),
         Result = ok(status_info(branch(Branch), working_status(WorkingStatus), files(Files)))
        ),
        Error,
        Result = error(git_error(Error))
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

% Ls-files perception spell
register_spell(
    perceive(git(ls_files)),
    input(git(ls_files(directory('Directory')))),
    output(ok(tracked_files('Files'))),
    docstring("List all files tracked by git in the specified directory.")
).

cast(perceive(git(ls_files(Directory))), Result) :-
    catch(
        (process_create(path(git), ['ls-files'], [
             stdout(pipe(Out)),
             stderr(null),
             cwd(Directory)
         ]),
         read_lines_from_stream(Out, Files),
         close(Out),
         Result = ok(file_list(Files))
        ),
        Error,
        Result = error(git_error(Error))
    ).

% Current branch perception spell
register_spell(
    perceive(git(current_branch)),
    input(git(current_branch(root('Root')))),
    output(ok(branch('Branch'))),
    docstring("Get the current git branch name for the repository at the specified root.")
).

cast(perceive(git(current_branch(_Root))), Result) :-
    catch(
        (cast(conjure(git(branch(['--show-current']))), BranchResult),
         (BranchResult = ok(result(Output, _)) ->
             string_concat(BranchStr, "\n", Output),
             atom_string(Branch, BranchStr)
         ;
             Branch = unknown
         ),
         Result = ok(current_branch(Branch))
        ),
        Error,
        Result = error(git_error(Error))
    ).

% Use read_lines_from_stream/2 from utils.pl

% Example git subsystem extension for load_entity
% This could handle git repository cloning and then loading semantics
% load_entity(Entity, semantic(git(repo(URL)))) :-
%     % Clone repository to local path
%     format(atom(LocalPath), '/tmp/git_clone_~w', [Entity]),
%     git_clone(URL, LocalPath),
%     % Load the cloned semantics
%     atomic_list_concat([LocalPath, '/semantics.pl'], SemanticFile),
%     load_entity(Entity, semantic(file(SemanticFile))).

