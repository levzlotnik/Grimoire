:- use_module(library(strings)).
:- ensure_loaded("./ecs.pl").

% Git command entity and registration
entity(git).
component(command, ctor, git).

% Define all supported git subcommands
component(git, subcommand, clone).
component(git, subcommand, init).
component(git, subcommand, add).
component(git, subcommand, commit).
component(git, subcommand, push).
component(git, subcommand, pull).
component(git, subcommand, branch).
component(git, subcommand, checkout).

% Term structure docstrings
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

docstring(git, S) :-
    findall(
        SubCmdDoc,
        (
            component(git, subcommand, SubCmdType),
            docstring(git(SubCmdType), SubCmd),
            indent_lines('  ', SubCmd, SubCmdDoc)
        ),
        SubCmdDocs
    ),
    atomic_list_concat(SubCmdDocs, "\n\n", Joined),
    S = {|string(Joined)||
    Git commands suite.
    Format: git(subcommand(...))

    Possible subcommands:
    {Joined}
    |}.

% Convert git terms to shell arguments
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
git_args(checkout(Branch)) --> ["checkout", Branch].

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
