:- use_module(library(plunit)).
:- ensure_loaded('git.pl').

:- begin_tests(git_semantics).

% Test basic Git entity existence
test(git_entity_exists, [true]) :-
    entity(git).

% Test Git command constructors
test(git_command_constructors, [true]) :-
    component(command, ctor, git(clone)),
    component(command, ctor, git(init)),
    component(command, ctor, git(add)),
    component(command, ctor, git(commit)),
    component(command, ctor, git(status)).

% Test Git subcommand declarations
test(git_subcommands, [true]) :-
    component(git, subcommand, clone),
    component(git, subcommand, status),
    component(git, subcommand, diff),
    component(git, subcommand, log).

% Test Git docstrings exist
test(git_docstrings_exist, [
    forall(component(git, subcommand, Cmd))
]) :-
    docstring(git(Cmd), _).

% Test Git argument parsing (DCG)
test(git_args_parsing, [true]) :-
    phrase(git_args(clone("https://github.com/user/repo", "/tmp/repo")),
           ["clone", "https://github.com/user/repo", "/tmp/repo"]),
    phrase(git_args(status), ["status"]),
    phrase(git_args(diff), ["diff"]).

% Test Git command validation
test(git_command_validation, [true]) :-
    % Test that we can validate git commands exist
    functor(clone("url", "path"), clone, 2),
    component(git, subcommand, clone),
    functor(status, status, 0),
    component(git, subcommand, status).

:- end_tests(git_semantics).
