:- use_module(library(strings)).

% Git entity declaration with automatic self-location
:- self_entity(git, {|string(_)||
    Knowledge evolution tracking and version control subsystem.
    Manages version history, branching, and transactional rollback through git.
    Provides session management where each session is a git branch,
    enabling atomic changes and experimental workflows with easy rollback.
    Core to Grimoire's immutable knowledge architecture.
    |}).

% Infer subcommands from spell constructors
component(git, subcommand, SubCmd) :- component(perceive, ctor, git(SubCmd)).
component(git, subcommand, SubCmd) :- component(conjure, ctor, git(SubCmd)).

% Spell constructors are auto-generated from register_spell/4 declarations below

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

% Repository root path expansion (only if entity has self component)
component(Entity, git_repository_root, Root) :-
    component(Entity, has(git(repository)), _),
    component(Entity, self, semantic(folder(Root))),
    !.  % Cut to prevent backtracking to file variant

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

% Conjure spells for git commands
register_spell(
    conjure(git(clone)),
    input(git(clone(url('Url'), path('Path')))),
    output(either(ok(result(stdout('StdOut'), stderr('StdErr'))), error(git_error('Reason')))),
    docstring("Clone a repository into a new directory. Url: Remote repository URL, Path: Local directory to clone into")
).

register_spell(
    conjure(git(init)),
    input(git(init(path('Path')))),
    output(either(ok(result(stdout('StdOut'), stderr('StdErr'))), error(git_error('Reason')))),
    docstring("Create an empty Git repository. Path: Directory to initialize")
).

register_spell(
    conjure(git(add)),
    input(git(add(paths('Paths')))),
    output(either(ok(result(stdout('StdOut'), stderr('StdErr'))), error(git_error('Reason')))),
    docstring("Add file(s) to the index. Paths: List of file paths to stage, or all_tracked to update all tracked files (-u flag)")
).

register_spell(
    conjure(git(commit)),
    input(git(commit(message('Message')))),
    output(either(ok(result(stdout('StdOut'), stderr('StdErr'))), error(git_error('Reason')))),
    docstring("Record changes to the repository. Message: String or list containing commit message and flags")
).

register_spell(
    conjure(git(push)),
    input(git(push(remote('Remote'), branch('Branch')))),
    output(either(ok(result(stdout('StdOut'), stderr('StdErr'))), error(git_error('Reason')))),
    docstring("Update remote refs along with objects. Remote: Remote name (e.g. 'origin'), Branch: Branch name to push")
).

register_spell(
    conjure(git(pull)),
    input(git(pull(remote('Remote'), branch('Branch')))),
    output(either(ok(result(stdout('StdOut'), stderr('StdErr'))), error(git_error('Reason')))),
    docstring("Fetch and integrate with another repository/branch. Remote: Remote name (e.g. 'origin'), Branch: Branch name to pull")
).

register_spell(
    conjure(git(checkout)),
    input(git(checkout(branch('Branch')))),
    output(either(ok(result(stdout('StdOut'), stderr('StdErr'))), error(git_error('Reason')))),
    docstring("Switch branches or restore working tree files. Branch: Name of branch to switch to, or Args list for advanced options")
).

register_spell(
    conjure(git(reset)),
    input(git(reset(args('Args')))),
    output(either(ok(result(stdout('StdOut'), stderr('StdErr'))), error(git_error('Reason')))),
    docstring("Reset current HEAD to the specified state. Args: List of arguments to pass to git reset")
).

register_spell(
    conjure(git(merge)),
    input(git(merge(args('Args')))),
    output(either(ok(result(stdout('StdOut'), stderr('StdErr'))), error(git_error('Reason')))),
    docstring("Join two or more development histories together. Args: List of arguments to pass to git merge")
).

% Perceive spells for git queries
register_spell(
    perceive(git(diff)),
    input(git(diff)),
    output(either(ok(result(stdout('StdOut'), stderr('StdErr'))), error(git_error('Reason')))),
    docstring("Show changes between commits, commit and working tree, etc.")
).

register_spell(
    perceive(git(log)),
    input(git(log)),
    output(either(ok(result(stdout('StdOut'), stderr('StdErr'))), error(git_error('Reason')))),
    docstring("Show the commit logs. Can be called as git(log) or git(log(Args)) with arguments list")
).

register_spell(
    perceive(git(branch)),
    input(git(branch(operation('Operation')))),
    output(either(ok(result(stdout('StdOut'), stderr('StdErr'))), error(git_error('Reason')))),
    docstring("Manage git branches. Operations: list (list all branches), create(Name) (create new branch), or Args list for advanced options")
).

register_spell(
    perceive(git(rev_parse)),
    input(git(rev_parse(args('Args')))),
    output(either(ok(result(stdout('StdOut'), stderr('StdErr'))), error(git_error('Reason')))),
    docstring("Parse and verify Git revision/object names. Args: List of arguments to pass to git rev-parse")
).

register_spell(
    conjure(git(remote)),
    input(git(remote(args('Args')))),
    output(either(ok(result(stdout('StdOut'), stderr('StdErr'))), error(git_error('Reason')))),
    docstring("Manage set of tracked repositories. Args: List of arguments to pass to git remote. Examples: [] (list all), ['show', 'origin'], ['get-url', 'origin']")
).

% Main git entity docstring
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
git_args(config(Args)) --> ["config" | Args].  % Handle config with args list
git_args(remote(Args)) --> ["remote" | Args].  % Handle remote with args list

% Git config spell
register_spell(
    conjure(git(config)),
    input(git(config(args('Args')))),
    output(either(
        ok(result(stdout('StdOut'), stderr('StdErr'))),
        error(git_error(exit('ExitCode'), stderr('StdErr')))
    )),
    docstring("Get and set repository or global git configuration options. Args: list of config command arguments (e.g., ['user.name', 'Value'] or ['--get', 'user.name']).")
).

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

% Example git subsystem extension for load_entity
% This could handle git repository cloning and then loading semantics
% load_entity(Entity, semantic(git(repo(URL)))) :-
%     % Clone repository to local path
%     format(atom(LocalPath), '/tmp/git_clone_~w', [Entity]),
%     git_clone(URL, LocalPath),
%     % Load the cloned semantics
%     atomic_list_concat([LocalPath, '/semantics.pl'], SemanticFile),
%     load_entity(Entity, semantic(file(SemanticFile))).

