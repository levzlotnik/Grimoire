:- use_module(library(strings)).

% Git entity declaration with automatic self-location
:- self_entity(git).

% === ENTITY DECLARATIONS ===

% Git command entities
entity(git(clone)).
entity(git(init)).
entity(git(add)).
entity(git(commit)).
entity(git(push)).
entity(git(pull)).
entity(git(checkout)).
entity(git(reset)).
entity(git(merge)).
entity(git(config)).
entity(git(remote)).
entity(git(diff)).
entity(git(log)).
entity(git(branch)).
entity(git(rev_parse)).
entity(git(ls_files)).
entity(git(status)).
entity(git(current_branch)).

% Git domain entities
entity(git(repository)).

docstring(git,
    {|string(_)||
    Knowledge evolution tracking and version control subsystem.
    Manages version history, branching, and transactional rollback through git.
    Provides session management where each session is a git branch,
    enabling atomic changes and experimental workflows with easy rollback.
    Core to Grimoire's immutable knowledge architecture.
    |}).

% === COMMAND SYSTEM ===

% Git subcommands (inferred from spell constructors)
component(git, subcommand, SubCmd) :- component(perceive, ctor, git(SubCmd)).
component(git, subcommand, SubCmd) :- component(conjure, ctor, git(SubCmd)).

% Legacy support - keep git namespace ctors
component(git, ctor, C) :- component(git, subcommand, C).

% === COMPONENT EXPANSION USING ==> OPERATOR ===

% High-level has(git(repository)) pattern expands to queryable properties
% The ==> operator automatically generates BOTH:
% 1. Component expansion rules (generative)
% 2. Verify clause with please_verify calls (discriminative)
component(E, has(git(repository)), git(repository(Spec)))
    ==> (component(E, git_repository_remote_name, Name) :- member(remote(Name, _), Spec)),
        (component(E, git_repository_remote_url, URL) :- member(remote(_, URL), Spec)),
        (component(E, git_repository_branch, Branch) :- member(branch(Branch), Spec)),
        (component(E, git_repository_clean, Clean) :- member(clean(Clean), Spec)),
        % Skills derived from git repository
        (component(E, skill(git(push)), SpellTerm) :-
            member(remote(_, _), Spec),  % Only if remote exists
            component(E, git_repository_root, Root),
            SpellTerm = conjure(git(push(git_root(Root))))),
        (component(E, skill(git(pull)), SpellTerm) :-
            member(remote(_, _), Spec),  % Only if remote exists
            component(E, git_repository_root, Root),
            SpellTerm = conjure(git(pull(git_root(Root))))),
        (component(E, skill(git(status)), SpellTerm) :-
            component(E, git_repository_root, Root),
            SpellTerm = perceive(git(status(git_root(Root))))).

% === EXTERNAL ACTIVATION PATTERNS (Pattern 1: OS Reality → KB) ===
% ALL component queries use magic_cast(perceive(git(...))) to query git state

% Git repository root derived from entity's self component
component(E, git_repository_root, Root) :-
    component(E, has(git(repository)), git(repository(_))),
    (component(E, self, semantic(folder(Root))) ;
     (component(E, self, semantic(file(FilePath))),
      file_directory_name(FilePath, Root))).

% Auto-detect current branch from git
component(Entity, git_repository_current_branch, CurrentBranch) :-
    component(Entity, git_repository_root, Root),
    git_root_ready(Root),
    magic_cast(perceive(git(current_branch(git_root(Root)))), Result),
    Result = ok(current_branch(CurrentBranch)).

% Auto-detect working status from git
component(Entity, git_repository_working_status, WorkingStatus) :-
    component(Entity, git_repository_root, Root),
    git_root_ready(Root),
    magic_cast(perceive(git(status(git_root(Root)))), Result),
    Result = ok(status_info(branch(_), working_status(WorkingStatus), files(_))).

% === LEAF COMPONENT VERIFICATIONS USING :: OPERATOR ===
% Verification checks actual OS reality

% Verify root exists and is actually a git repository
component(_, git_repository_root, Root)
    :: exists_directory(Root),
       atomic_list_concat([Root, '/.git'], GitDir),
       exists_directory(GitDir).

git_root_ready(Root) :-
    exists_directory(Root),
    atomic_list_concat([Root, '/.git'], GitDir),
    exists_directory(GitDir).

% === PARSING HELPERS FOR STRUCTURED OUTPUTS ===

% Extract commit hash from git commit output
% Git commit output: "[branch commit_hash] message"
parse_commit_hash(Output, Hash) :-
    (re_matchsub("\\[\\w+ ([0-9a-f]+)\\]", Output, Sub, []) ->
        get_dict(1, Sub, Hash)
    ;
        Hash = unknown
    ).

% Parse branch list output into structured list
parse_branch_list(Output, Branches) :-
    string_lines(Output, Lines),
    maplist(parse_branch_line, Lines, Branches).

parse_branch_line(Line, Branch) :-
    string_trim(Line, Trimmed),
    (string_concat("* ", BranchName, Trimmed) ->
        atom_string(Branch, BranchName)
    ;
        atom_string(Branch, Trimmed)
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

% === DCG FOR GIT ARGUMENTS ===

% Git command implementations using DCG
% Commands that don't use -C (they create/target directories)
git_args(clone(Url, Path)) --> ["clone", Url, Path].
git_args(init(Path)) --> ["init", Path].

% All other commands use -C flag to specify repository
% Unwrap git_root functor to get actual path
git_args(in(git_root(Root), add(all_tracked))) --> ["-C", Root, "add", "-u"].
git_args(in(git_root(Root), add(Paths))) -->
    { is_list(Paths) },
    ["-C", Root, "add" | Paths].
git_args(in(git_root(Root), commit(Message))) --> ["-C", Root, "commit", "-m", Message].
git_args(in(git_root(Root), push(Remote, Branch))) --> ["-C", Root, "push", Remote, Branch].
git_args(in(git_root(Root), pull(Remote, Branch))) --> ["-C", Root, "pull", Remote, Branch].
git_args(in(git_root(Root), branch(list))) --> ["-C", Root, "branch"].
git_args(in(git_root(Root), branch(create(Name)))) --> ["-C", Root, "branch", Name].
git_args(in(git_root(Root), branch(Args))) --> { is_list(Args) }, ["-C", Root, "branch" | Args].
git_args(in(git_root(Root), checkout(Branch))) --> ["-C", Root, "checkout", Branch].
git_args(in(git_root(Root), checkout(Args))) --> { is_list(Args) }, ["-C", Root, "checkout" | Args].
git_args(in(git_root(Root), status)) --> ["-C", Root, "status"].
git_args(in(git_root(Root), status(Args))) --> { is_list(Args) }, ["-C", Root, "status" | Args].
git_args(in(git_root(Root), diff)) --> ["-C", Root, "diff"].
git_args(in(git_root(Root), log)) --> ["-C", Root, "log"].
git_args(in(git_root(Root), log(Args))) --> { is_list(Args) }, ["-C", Root, "log" | Args].
git_args(in(git_root(Root), rev_parse(Args))) --> { is_list(Args) }, ["-C", Root, "rev-parse" | Args].
git_args(in(git_root(Root), reset(Args))) --> { is_list(Args) }, ["-C", Root, "reset" | Args].
git_args(in(git_root(Root), merge(Args))) --> { is_list(Args) }, ["-C", Root, "merge" | Args].
git_args(in(git_root(Root), config(Args))) --> { is_list(Args) }, ["-C", Root, "config" | Args].
git_args(in(git_root(Root), remote(Args))) --> { is_list(Args) }, ["-C", Root, "remote" | Args].
git_args(in(git_root(Root), ls_files)) --> ["-C", Root, "ls-files"].

% === PHASE 3: SPELL REGISTRATIONS ===

% Clone repository
register_spell(
    conjure(git(clone)),
    input(conjure(git(clone(url(Url:string), path(Path:string))))),
    output(either(ok(cloned(url(Url:string), path(Path:string))), error(git_error(Reason:term), Context:term))),
    "Clone a repository into a new directory",
    [],
    implementation(conjure(git(clone(url(Url), path(Path)))), Result, (
        phrase(git_args(clone(Url, Path)), Args),
        magic_cast(conjure(executable_program(program(git), args(Args))), ExecResult),
        (ExecResult = ok(result(stdout(_Stdout), stderr(_Stderr))) ->
            Result = ok(cloned(url(Url), path(Path)))
        ; ExecResult = error(Reason, Context) ->
            Result = error(git_error(Reason), Context)
        )
    ))
).

% Initialize repository
register_spell(
    conjure(git(init)),
    input(conjure(git(init(path(Path:existing_folder))))),
    output(either(ok(initialized(path(Path:existing_folder))), error(git_error(Reason:term), Context:term))),
    "Create an empty Git repository at the specified path",
    [],
    implementation(conjure(git(init(path(Path)))), Result, (
        phrase(git_args(init(Path)), Args),
        magic_cast(conjure(executable_program(program(git), args(Args))), ExecResult),
        (ExecResult = ok(result(stdout(_Stdout), stderr(_Stderr))) ->
            Result = ok(initialized(path(Path)))
        ; ExecResult = error(Reason, Context) ->
            Result = error(git_error(Reason), Context)
        )
    ))
).

% Add files to staging area
register_spell(
    conjure(git(add)),
    input(conjure(git(add(git_root(Root:existing_folder), paths(Paths:term))))),
    output(either(ok(staged(files(Files:term))), error(git_error(Reason:term), Context:term))),
    "Add file(s) to the staging area in the given repository. Paths can be a list of files or all_tracked",
    [],
    implementation(conjure(git(add(git_root(Root), paths(Paths)))), Result, (
        phrase(git_args(in(git_root(Root), add(Paths))), Args),
        magic_cast(conjure(executable_program(program(git), args(Args))), ExecResult),
        (ExecResult = ok(result(stdout(_Stdout), stderr(_Stderr))) ->
            (is_list(Paths) -> Files = Paths ; Files = all_tracked),
            Result = ok(staged(files(Files)))
        ; ExecResult = error(Reason, Context) ->
            Result = error(git_error(Reason), Context)
        )
    ))
).

% Commit changes
register_spell(
    conjure(git(commit)),
    input(conjure(git(commit(git_root(Root:existing_folder), message(Message:string))))),
    output(either(ok(committed(hash(Hash:string))), error(git_error(Reason:term), Context:term))),
    "Record changes to the repository with the given commit message",
    [],
    implementation(conjure(git(commit(git_root(Root), message(Message)))), Result, (
        phrase(git_args(in(git_root(Root), commit(Message))), Args),
        magic_cast(conjure(executable_program(program(git), args(Args))), ExecResult),
        (ExecResult = ok(result(stdout(Stdout), stderr(_Stderr))) ->
            parse_commit_hash(Stdout, Hash),
            Result = ok(committed(hash(Hash)))
        ; ExecResult = error(Reason, Context) ->
            Result = error(git_error(Reason), Context)
        )
    ))
).

% Push to remote
register_spell(
    conjure(git(push)),
    input(conjure(git(push(remote(Remote:atom), branch(Branch:atom))))),
    output(either(ok(pushed(remote(Remote:atom), branch(Branch:atom))), error(git_error(Reason:term), Context:term))),
    "Update remote refs along with associated objects",
    [],
    implementation(conjure(git(push(remote(Remote), branch(Branch)))), Result, (
        phrase(git_args(push(Remote, Branch)), Args),
        magic_cast(conjure(executable_program(program(git), args(Args))), ExecResult),
        (ExecResult = ok(result(stdout(_Stdout), stderr(_Stderr))) ->
            Result = ok(pushed(remote(Remote), branch(Branch)))
        ; ExecResult = error(Reason, Context) ->
            Result = error(git_error(Reason), Context)
        )
    ))
).

% Pull from remote
register_spell(
    conjure(git(pull)),
    input(conjure(git(pull(remote(Remote:atom), branch(Branch:atom))))),
    output(either(ok(pulled(remote(Remote:atom), branch(Branch:atom))), error(git_error(Reason:term), Context:term))),
    "Fetch from and integrate with another repository or branch",
    [],
    implementation(conjure(git(pull(remote(Remote), branch(Branch)))), Result, (
        phrase(git_args(pull(Remote, Branch)), Args),
        magic_cast(conjure(executable_program(program(git), args(Args))), ExecResult),
        (ExecResult = ok(result(stdout(_Stdout), stderr(_Stderr))) ->
            Result = ok(pulled(remote(Remote), branch(Branch)))
        ; ExecResult = error(Reason, Context) ->
            Result = error(git_error(Reason), Context)
        )
    ))
).

% Checkout branch
register_spell(
    conjure(git(checkout)),
    input(conjure(git(checkout(branch(Branch:atom))))),
    output(either(ok(checked_out(branch(Branch:atom))), error(git_error(Reason:term), Context:term))),
    "Switch branches or restore working tree files",
    [],
    implementation(conjure(git(checkout(branch(Branch)))), Result, (
        phrase(git_args(checkout(Branch)), Args),
        magic_cast(conjure(executable_program(program(git), args(Args))), ExecResult),
        (ExecResult = ok(result(stdout(_Stdout), stderr(_Stderr))) ->
            Result = ok(checked_out(branch(Branch)))
        ; ExecResult = error(Reason, Context) ->
            Result = error(git_error(Reason), Context)
        )
    ))
).

% Reset HEAD
register_spell(
    conjure(git(reset)),
    input(conjure(git(reset(args(Args:term))))),
    output(either(ok(reset(args(Args:term))), error(git_error(Reason:term), Context:term))),
    "Reset current HEAD to the specified state",
    [],
    implementation(conjure(git(reset(args(Args)))), Result, (
        phrase(git_args(reset(Args)), GitArgs),
        magic_cast(conjure(executable_program(program(git), args(GitArgs))), ExecResult),
        (ExecResult = ok(result(stdout(_Stdout), stderr(_Stderr))) ->
            Result = ok(reset(args(Args)))
        ; ExecResult = error(Reason, Context) ->
            Result = error(git_error(Reason), Context)
        )
    ))
).

% Merge branches
register_spell(
    conjure(git(merge)),
    input(conjure(git(merge(args(Args:term))))),
    output(either(ok(merged(args(Args:term))), error(git_error(Reason:term), Context:term))),
    "Join two or more development histories together",
    [],
    implementation(conjure(git(merge(args(Args)))), Result, (
        phrase(git_args(merge(Args)), GitArgs),
        magic_cast(conjure(executable_program(program(git), args(GitArgs))), ExecResult),
        (ExecResult = ok(result(stdout(_Stdout), stderr(_Stderr))) ->
            Result = ok(merged(args(Args)))
        ; ExecResult = error(Reason, Context) ->
            Result = error(git_error(Reason), Context)
        )
    ))
).

% Configure git
register_spell(
    conjure(git(config)),
    input(conjure(git(config(git_root(Root:existing_folder), args(Args:term))))),
    output(either(ok(config_output(Output:string)), error(git_error(Reason:term), Context:term))),
    "Get and set repository or global git configuration options",
    [],
    implementation(conjure(git(config(git_root(Root), args(Args)))), Result, (
        phrase(git_args(in(git_root(Root), config(Args))), GitArgs),
        magic_cast(conjure(executable_program(program(git), args(GitArgs))), ExecResult),
        (ExecResult = ok(result(stdout(Output), stderr(_Stderr))) ->
            Result = ok(config_output(Output))
        ; ExecResult = error(Reason, Context) ->
            Result = error(git_error(Reason), Context)
        )
    ))
).

% Manage remotes
register_spell(
    conjure(git(remote)),
    input(conjure(git(remote(args(Args:term))))),
    output(either(ok(remote_output(Output:string)), error(git_error(Reason:term), Context:term))),
    "Manage set of tracked repositories",
    [],
    implementation(conjure(git(remote(args(Args)))), Result, (
        phrase(git_args(remote(Args)), GitArgs),
        magic_cast(conjure(executable_program(program(git), args(GitArgs))), ExecResult),
        (ExecResult = ok(result(stdout(Output), stderr(_Stderr))) ->
            Result = ok(remote_output(Output))
        ; ExecResult = error(Reason, Context) ->
            Result = error(git_error(Reason), Context)
        )
    ))
).

% === PERCEIVE SPELLS ===

% Show diff
register_spell(
    perceive(git(diff)),
    input(perceive(git(diff(git_root(Root:existing_folder))))),
    output(either(ok(diff_output(Output:string)), error(git_error(Reason:term), Context:term))),
    "Show changes between commits, commit and working tree, etc.",
    [],
    implementation(perceive(git(diff(git_root(Root)))), Result, (
        phrase(git_args(in(git_root(Root), diff)), Args),
        magic_cast(conjure(executable_program(program(git), args(Args))), ExecResult),
        (ExecResult = ok(result(stdout(Output), stderr(_Stderr))) ->
            Result = ok(diff_output(Output))
        ; ExecResult = error(Reason, Context) ->
            Result = error(git_error(Reason), Context)
        )
    ))
).

% Show log
register_spell(
    perceive(git(log)),
    input(perceive(git(log(git_root(Root:existing_folder))))),
    output(either(ok(log_output(Output:string)), error(git_error(Reason:term), Context:term))),
    "Show the commit logs",
    [],
    implementation(perceive(git(log(git_root(Root)))), Result, (
        phrase(git_args(in(git_root(Root), log)), Args),
        magic_cast(conjure(executable_program(program(git), args(Args))), ExecResult),
        (ExecResult = ok(result(stdout(Output), stderr(_Stderr))) ->
            Result = ok(log_output(Output))
        ; ExecResult = error(Reason, Context) ->
            Result = error(git_error(Reason), Context)
        )
    ))
).

% List branches
register_spell(
    perceive(git(branch)),
    input(perceive(git(branch(git_root(Root:existing_folder), operation(Operation:term))))),
    output(either(ok(branches(Branches:term)), error(git_error(Reason:term), Context:term))),
    "List or manage git branches",
    [],
    implementation(perceive(git(branch(git_root(Root), operation(Operation)))), Result, (
        phrase(git_args(in(git_root(Root), branch(Operation))), Args),
        magic_cast(conjure(executable_program(program(git), args(Args))), ExecResult),
        (ExecResult = ok(result(stdout(Output), stderr(_Stderr))) ->
            (Operation = list ->
                parse_branch_list(Output, Branches),
                Result = ok(branches(Branches))
            ;
                Result = ok(branches(Output))
            )
        ; ExecResult = error(Reason, Context) ->
            Result = error(git_error(Reason), Context)
        )
    ))
).

% Parse revision
register_spell(
    perceive(git(rev_parse)),
    input(perceive(git(rev_parse(args(Args:term))))),
    output(either(ok(rev_parse_output(Output:string)), error(git_error(Reason:term), Context:term))),
    "Parse and verify Git revision/object names",
    [],
    implementation(perceive(git(rev_parse(args(Args)))), Result, (
        phrase(git_args(rev_parse(Args)), GitArgs),
        magic_cast(conjure(executable_program(program(git), args(GitArgs))), ExecResult),
        (ExecResult = ok(result(stdout(Output), stderr(_Stderr))) ->
            Result = ok(rev_parse_output(Output))
        ; ExecResult = error(Reason, Context) ->
            Result = error(git_error(Reason), Context)
        )
    ))
).

% List tracked files
register_spell(
    perceive(git(ls_files)),
    input(perceive(git(ls_files(git_root(Root:existing_folder))))),
    output(either(ok(tracked_files(Lines:list(string))), error(git_error(Reason:term), Context:term))),
    "List all files tracked by git in the specified directory",
    [],
    implementation(perceive(git(ls_files(git_root(Root)))), Result, (
        phrase(git_args(in(git_root(Root), ls_files)), Args),
        magic_cast(conjure(executable_program(program(git), args(Args))), ExecResult),
        (ExecResult = ok(result(stdout(Stdout), stderr(_Stderr))) ->
            string_lines(Stdout, Lines),
            Result = ok(tracked_files(Lines))
        ; ExecResult = error(Reason, Context) ->
            Result = error(git_error(Reason), Context)
        )
    ))
).

% Get repository status
register_spell(
    perceive(git(status)),
    input(perceive(git(status(git_root(Root:existing_folder))))),
    output(either(ok(status_info(branch(Branch:atom), working_status(WorkingStatus:atom), files(Files:list(term)))), error(git_error(Reason:term), Context:term))),
    "Query git repository status including current branch, working tree status, and file changes",
    [],
    implementation(perceive(git(status(git_root(Root)))), Result, (
        catch(
            (% Get current branch
             magic_cast(perceive(git(branch(git_root(Root), operation(['--show-current'])))), BranchResult),
             (BranchResult = ok(branches(BranchOutput)) ->
                 (is_list(BranchOutput), BranchOutput = [Branch|_] -> true
                 ; atom_string(Branch, BranchOutput))
             ; BranchResult = ok(remote_output(BOut)) ->
                 string_concat(BranchStr, "\n", BOut),
                 atom_string(Branch, BranchStr)
             ;
                 Branch = unknown
             ),
             % Get working tree status - need a separate status spell for porcelain
             phrase(git_args(in(git_root(Root), status(['--porcelain']))), StatusArgs),
             magic_cast(conjure(executable_program(program(git), args(StatusArgs))), StatusExecResult),
             (StatusExecResult = ok(result(stdout(StatusOutput), stderr(_Stderr))) ->
                 (StatusOutput = "" ->
                     WorkingStatus = clean,
                     Files = []
                 ;
                     WorkingStatus = dirty,
                     parse_git_status_output(StatusOutput, Files)
                 )
             ; StatusExecResult = error(_Reason, _Context) ->
                 WorkingStatus = unknown,
                 Files = []
             ),
             Result = ok(status_info(branch(Branch), working_status(WorkingStatus), files(Files)))
            ),
            error(Reason, Context),
            Result = error(git_error(Reason), Context)
        )
    ))
).

% Get current branch
register_spell(
    perceive(git(current_branch)),
    input(perceive(git(current_branch(git_root(Root:existing_folder))))),
    output(either(ok(current_branch(Branch:atom)), error(git_error(Reason:term), Context:term))),
    "Get the current git branch name for the repository at the specified root",
    [],
    implementation(perceive(git(current_branch(git_root(Root)))), Result, (
        catch(
            (magic_cast(perceive(git(branch(git_root(Root), operation(['--show-current'])))), BranchResult),
             (BranchResult = ok(branches(BranchOutput)) ->
                 (is_list(BranchOutput), BranchOutput = [Branch|_] -> true
                 ; atom_string(Branch, BranchOutput))
             ; BranchResult = ok(remote_output(Output)) ->
                 string_concat(BranchStr, "\n", Output),
                 atom_string(Branch, BranchStr)
             ;
                 Branch = unknown
             ),
             Result = ok(current_branch(Branch))
            ),
            error(Reason, Context),
            Result = error(git_error(Reason), Context)
        )
    ))
).
