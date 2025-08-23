% Python REST API template semantics - FastAPI-based HTTP interface
% Provides RESTful API endpoints for accessing functionality over HTTP

:- self_entity(python_rest_api_template).

% Main docstring for the project
docstring(python_rest_api_template, "A Python REST API template using FastAPI for HTTP-based interfaces. Provides RESTful endpoints for data processing, perception queries, and conjuration spells.").

% Project type identification
component(python_rest_api_template, project_type, python).
component(python_rest_api_template, build_system, nix).
component(python_rest_api_template, language, python).
component(python_rest_api_template, framework, fastapi).

% Make this entity available as a command
component(conjure, ctor, python_rest_api_template).

% Nix-provided subcommands (from flake apps)
component(python_rest_api_template, subcommand, run).
component(python_rest_api_template, subcommand, test).
component(python_rest_api_template, subcommand, develop).

% Docstrings for Nix-based subcommands
docstring(python_rest_api_template(run), "Run the FastAPI server using 'nix run .#run'").
docstring(python_rest_api_template(test), "Run API tests using 'nix run .#test'").
docstring(python_rest_api_template(develop), "Enter development shell using 'nix develop'").

% Make subcommands available as ctors too
component(python_rest_api_template, ctor, C) :- component(python_rest_api_template, subcommand, C).

% Command implementations using Nix
cast(conjure(python_rest_api_template(run)), RetVal) :-
    cast(conjure(nix(run(['.#run']))), RetVal).

cast(conjure(python_rest_api_template(test)), RetVal) :-
    cast(conjure(nix(run(['.#test']))), RetVal).

cast(conjure(python_rest_api_template(develop)), RetVal) :-
    cast(conjure(nix(develop(['.']))), RetVal).

% API endpoint definitions as entities and components
entity(api_endpoint).

% Define the API endpoints as components
component(python_rest_api_template, api_endpoint, endpoint(get, "/", "Root endpoint")).
component(python_rest_api_template, api_endpoint, endpoint(get, "/hello", "GET endpoint example")).
component(python_rest_api_template, api_endpoint, endpoint(post, "/data", "POST endpoint for data processing")).
component(python_rest_api_template, api_endpoint, endpoint(get, "/perceive", "Perception query endpoint")).
component(python_rest_api_template, api_endpoint, endpoint(post, "/conjure", "Conjuration spell endpoint")).
component(python_rest_api_template, api_endpoint, endpoint(get, "/health", "Health check endpoint")).

% Endpoint documentation
docstring(endpoint(get, "/", _), "Returns basic API information").
docstring(endpoint(get, "/hello", _), "Returns a greeting message").
docstring(endpoint(post, "/data", _), "Processes arbitrary JSON data and returns transformation").
docstring(endpoint(get, "/perceive", _), "Executes perception queries with optional session management").
docstring(endpoint(post, "/conjure", _), "Executes conjuration spells with session tracking").
docstring(endpoint(get, "/health", _), "Returns API health status").

% Project structure expectations
component(python_rest_api_template, expected_file, file("pyproject.toml")).
component(python_rest_api_template, expected_file, file("main.py")).
component(python_rest_api_template, main_source, file("main.py")).

% FastAPI-specific patterns
component(python_rest_api_template, framework_pattern, "FastAPI application with Pydantic models").
component(python_rest_api_template, server_pattern, "Uvicorn ASGI server").
component(python_rest_api_template, api_pattern, "RESTful endpoints with JSON request/response").

% Session management pattern
component(python_rest_api_template, session_pattern, "Optional session_id in request/response").
component(python_rest_api_template, response_pattern, "Standardized APIResponse model").

% Common Python build artifacts patterns  
component(python_rest_api_template, build_artifact_pattern, glob("dist/**/*")).
component(python_rest_api_template, build_artifact_pattern, glob("__pycache__/**/*")).
component(python_rest_api_template, build_artifact_pattern, glob("*.egg-info/**/*")).
component(python_rest_api_template, build_artifact_pattern, glob(".pytest_cache/**/*")).