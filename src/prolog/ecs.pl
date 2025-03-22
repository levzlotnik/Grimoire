:- use_module(library(strings)).

% Core ECS predicates - allow extension across files
:- dynamic([
    entity/1,
    component/3,
    docstring/2
], [
    discontiguous(true),
    multifile(true)
]).

% Base docstrings for ECS
docstring(entity,
    {|string(_)||
    Declares something as an entity within the system.
    Format: entity(Thing).
    Examples:
      entity(folder("/home/user/docs"))
      entity(file("document.txt"))
    |}
).

docstring(component,
    {|string(_)||
    Defines a hierarchical relationship between entities, where one entity is a
    component of another. ComponentName must be an atom for efficient querying.
    Format: component(Entity, ComponentName, Value)
    |}
).
