:- module(project_semantics, [entity/1, component/3]).

% Dynamic declarations
:- dynamic entity/1.
:- dynamic component/3.

% Base project entity and components
entity(project).
component(project, source, source(folder("project"))).
component(project, ctor, empty).
component(project, ctor, template).

% Project metadata components
component(project, option, git).          % git(bool) - initialize git repo
component(project, option, template).     % template(Template) - project template to use
component(project, option, language).     % language(Lang) - primary language

% Document the entities
docstring(project,
    {|string(_)||
    Project entity representing a software project.
    A project is a directory with specific structure and tooling.
    Projects can be created from templates or as empty directories.
    |}).

docstring(project(empty),
    {|string(_)||
    Empty project constructor.
    Creates a bare project directory with only basic initialization.
    |}).

docstring(project(template),
    {|string(_)||
    Template-based project constructor.
    Creates a project from a predefined template with standard structure.
    |}).

% Project template entity
entity(project_template).
component(project_template, ctor, nix_flake).
component(project_template, ctor, python).
component(project_template, ctor, rust).

docstring(project_template,
    {|string(_)||
    Project template entity defining standard project structures.
    Templates provide consistent starting points for new projects.
    |}).
