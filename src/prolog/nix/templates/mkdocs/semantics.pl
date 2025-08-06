% MkDocs project semantics - Nix-centric approach
% Uses Nix flake apps rather than direct mkdocs commands
% Note: MkDocs is documentation generation, no traditional testing needed

entity(mkdocs_template).

% Main docstring for the project
docstring(mkdocs_template, "An MkDocs documentation template using Nix flake apps for canonical documentation operations. Focuses on static site generation with Material theme.").

% Project type identification
component(mkdocs_template, project_type, mkdocs).
component(mkdocs_template, build_system, nix).  % Changed from mkdocs to nix
component(mkdocs_template, language, markdown).
component(mkdocs_template, paradigm, documentation).

% Make this entity available as a command
component(command, ctor, mkdocs_template).

% Nix-provided subcommands (from flake apps)
component(mkdocs_template, subcommand, build).
component(mkdocs_template, subcommand, serve).
component(mkdocs_template, subcommand, deploy).
component(mkdocs_template, subcommand, new).

% Docstrings for Nix-based subcommands
docstring(mkdocs_template(build), "Build the documentation site using 'nix run .#build'").
docstring(mkdocs_template(serve), "Serve the documentation locally using 'nix run .#serve'").
docstring(mkdocs_template(deploy), "Deploy documentation to GitHub Pages using 'nix run .#deploy'").
docstring(mkdocs_template(new), "Create new documentation pages using 'nix run .#new'").

% Make subcommands available as ctors too
component(mkdocs_template, ctor, C) :- component(mkdocs_template, subcommand, C).

% Command implementations using Nix
run(command(mkdocs_template(build)), RetVal) :-
    run(command(nix(run(['.#build']))), RetVal).

run(command(mkdocs_template(serve)), RetVal) :-
    run(command(nix(run(['.#serve']))), RetVal).

run(command(mkdocs_template(deploy)), RetVal) :-
    run(command(nix(run(['.#deploy']))), RetVal).

run(command(mkdocs_template(new)), RetVal) :-
    run(command(nix(run(['.#new']))), RetVal).

% Filesystem pattern matching for MkDocs projects
component(mkdocs_template, file_pattern, glob("mkdocs.yml")).
component(mkdocs_template, file_pattern, glob("docs/**/*.md")).
component(mkdocs_template, file_pattern, glob("docs/**/*.html")).
component(mkdocs_template, file_pattern, glob("overrides/**")).

% Discovery integration - can be used to identify MkDocs projects
component(mkdocs_template, discovery_pattern,
    include([glob("mkdocs.yml"), glob("docs/**/*.md")])).
component(mkdocs_template, discovery_pattern,
    exclude([glob("site/**"), glob("*.tmp")])).

% Source file location
component(mkdocs_template, source, source(semantic(file("semantics.pl")))).
