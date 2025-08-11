:- use_module(library(strings)).

% Git source location
component(git, source, file("git.pl")).

% Git entities and components
entity(git).
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

% Might as well mark those as ctors:
component(git, ctor, C) :- component(git, subcommand, C).
% Make these available to top level command:
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

% Main git command implementation
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

% Example git subsystem extension for load_entity
% This could handle git repository cloning and then loading semantics
% load_entity(Entity, semantic(git(repo(URL)))) :-
%     % Clone repository to local path
%     format(atom(LocalPath), '/tmp/git_clone_~w', [Entity]),
%     git_clone(URL, LocalPath),
%     % Load the cloned semantics
%     atomic_list_concat([LocalPath, '/semantics.pl'], SemanticFile),
%     load_entity(Entity, semantic(file(SemanticFile))).
