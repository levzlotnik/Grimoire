% Python REST API template semantics - FastAPI-based HTTP interface
% Provides RESTful API endpoints for accessing functionality over HTTP

:- self_entity(interface_api).

% Main docstring for the project
docstring(interface_api, "FastAPI-based HTTP interface for Grimoire. Provides RESTful endpoints that mirror all interface functionality. Note: This API provides HTTP access to the same interface predicates available through the CLI - it's a recursive relationship where API endpoints call interface operations.").

% Project type identification
component(interface_api, project_type, python).
component(interface_api, build_system, nix).
component(interface_api, language, python).
component(interface_api, framework, fastapi).

% Infer subcommands from spell constructors
component(interface_api, subcommand, SubCmd) :- component(conjure, ctor, interface_api(SubCmd)).

% Make subcommands available as ctors too
component(interface_api, ctor, C) :- component(interface_api, subcommand, C).

% Command implementations using Nix with register_spell/4

register_spell(
    conjure(interface_api(run)),
    input(interface_api(run)),
    output(either(
        ok(result(stdout('StdOut'), stderr('StdErr'))),
        error(nix_error('Reason'))
    )),
    docstring("Run the FastAPI server using 'nix run .#run'")
).
cast(conjure(interface_api(run)), RetVal) :-
    cast(conjure(nix(run(['.#run']))), RetVal).

register_spell(
    conjure(interface_api(test)),
    input(interface_api(test)),
    output(either(
        ok(result(stdout('StdOut'), stderr('StdErr'))),
        error(nix_error('Reason'))
    )),
    docstring("Run API tests using 'nix run .#test'")
).
cast(conjure(interface_api(test)), RetVal) :-
    cast(conjure(nix(run(['.#test']))), RetVal).

register_spell(
    conjure(interface_api(develop)),
    input(interface_api(develop)),
    output(either(
        ok(result(stdout('StdOut'), stderr('StdErr'))),
        error(nix_error('Reason'))
    )),
    docstring("Enter development shell using 'nix develop'")
).
cast(conjure(interface_api(develop)), RetVal) :-
    cast(conjure(nix(develop(['.']))), RetVal).

% API endpoint definitions as entities and components
entity(api_endpoint).

% Define the API endpoints as components - these mirror interface operations
component(interface_api, api_endpoint, endpoint(get, "/", "Root endpoint")).
component(interface_api, api_endpoint, endpoint(get, "/compt", "List component types → interface(compt)")).
component(interface_api, api_endpoint, endpoint(get, "/compt/{entity}", "List component types for entity → interface(compt(Entity))")).
component(interface_api, api_endpoint, endpoint(get, "/comp/{entity}/{comp_type}", "List components → interface(comp(Entity, Type))")).
component(interface_api, api_endpoint, endpoint(get, "/doc", "Show documentation → interface(doc)")).
component(interface_api, api_endpoint, endpoint(get, "/doc/{entity}", "Show entity documentation → interface(doc(Entity))")).
component(interface_api, api_endpoint, endpoint(get, "/perceive", "Perception query endpoint → interface(perceive(Query))")).
component(interface_api, api_endpoint, endpoint(post, "/conjure", "Conjuration spell endpoint → interface(conjure(Spell))")).
component(interface_api, api_endpoint, endpoint(get, "/entities", "List all entities → interface(entities)")).
component(interface_api, api_endpoint, endpoint(get, "/test", "Run tests → interface(test)")).
component(interface_api, api_endpoint, endpoint(get, "/health", "Health check endpoint")).

% Endpoint documentation
docstring(endpoint(get, "/", _), "Returns basic API information and available endpoints").
docstring(endpoint(get, "/compt", _), "Lists all component types in the system").
docstring(endpoint(get, "/compt/{entity}", _), "Lists component types for a specific entity").
docstring(endpoint(get, "/comp/{entity}/{comp_type}", _), "Lists components of a specific type for an entity").
docstring(endpoint(get, "/doc", _), "Shows system documentation").
docstring(endpoint(get, "/doc/{entity}", _), "Shows documentation for a specific entity").
docstring(endpoint(get, "/perceive", _), "Executes perception queries").
docstring(endpoint(post, "/conjure", _), "Executes conjuration spells").
docstring(endpoint(get, "/entities", _), "Lists all entities in the system").
docstring(endpoint(get, "/test", _), "Runs the test suite").
docstring(endpoint(get, "/health", _), "Returns API health status").

% Project structure expectations
component(interface_api, expected_file, file("pyproject.toml")).
component(interface_api, expected_file, file("main.py")).
component(interface_api, main_source, file("main.py")).

% FastAPI-specific patterns
component(interface_api, framework_pattern, "FastAPI application with Pydantic models").
component(interface_api, server_pattern, "Uvicorn ASGI server").
component(interface_api, api_pattern, "RESTful endpoints with JSON request/response").

% Session management pattern
component(interface_api, session_pattern, "Optional session_id in request/response").
component(interface_api, response_pattern, "Standardized APIResponse model").

% Interface integration pattern
component(interface_api, integration_pattern, "API endpoints call interface predicates").
component(interface_api, recursive_pattern, "HTTP access to CLI functionality").

% Common Python build artifacts patterns  
component(interface_api, build_artifact_pattern, glob("dist/**/*")).
component(interface_api, build_artifact_pattern, glob("__pycache__/**/*")).
component(interface_api, build_artifact_pattern, glob("*.egg-info/**/*")).
component(interface_api, build_artifact_pattern, glob(".pytest_cache/**/*")).