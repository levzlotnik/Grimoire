% React + FastAPI Fullstack Template Semantics
% A simple fullstack web application with React frontend and FastAPI backend

:- self_entity(react_fastapi_template).

% Main docstring for the project
docstring(react_fastapi_template, "A simple fullstack web application template using React (TypeScript) frontend and FastAPI (Python) backend with Nix build system.").

% Project type identification
component(react_fastapi_template, project_type, fullstack_web).
component(react_fastapi_template, build_system, nix).
component(react_fastapi_template, frontend_language, typescript).
component(react_fastapi_template, backend_language, python).
component(react_fastapi_template, frontend_framework, react).
component(react_fastapi_template, backend_framework, fastapi).
component(react_fastapi_template, database, sqlite).

% Make this entity available as a command
component(conjure, ctor, react_fastapi_template).

% Nix-provided subcommands (from flake apps)
component(react_fastapi_template, subcommand, run).
component(react_fastapi_template, subcommand, dev).
component(react_fastapi_template, subcommand, test).
component(react_fastapi_template, subcommand, test_backend).
component(react_fastapi_template, subcommand, test_frontend).

% Docstrings for Nix-based subcommands
docstring(react_fastapi_template(run), "Start both frontend and backend services using 'nix run'").
docstring(react_fastapi_template(dev), "Start development servers with hot reload using 'nix run .#dev'").
docstring(react_fastapi_template(test), "Run all tests (frontend and backend) using 'nix run .#test'").
docstring(react_fastapi_template(test_backend), "Run backend tests only using 'nix run .#test-backend'").
docstring(react_fastapi_template(test_frontend), "Run frontend tests only using 'nix run .#test-frontend'").

% Make subcommands available as ctors too
component(react_fastapi_template, ctor, C) :- component(react_fastapi_template, subcommand, C).

% Command implementations using Nix
cast(conjure(react_fastapi_template(run)), RetVal) :-
    cast(conjure(nix(run(['.']))), RetVal).

cast(conjure(react_fastapi_template(dev)), RetVal) :-
    cast(conjure(nix(run(['.#dev']))), RetVal).

cast(conjure(react_fastapi_template(test)), RetVal) :-
    cast(conjure(nix(run(['.#test']))), RetVal).

cast(conjure(react_fastapi_template(test_backend)), RetVal) :-
    cast(conjure(nix(run(['.#test-backend']))), RetVal).

cast(conjure(react_fastapi_template(test_frontend)), RetVal) :-
    cast(conjure(nix(run(['.#test-frontend']))), RetVal).

% Subscribe subcommands as children for entity hierarchy
component(react_fastapi_template, child, react_fastapi_template(Cmd)) :-
    component(react_fastapi_template, subcommand, Cmd).

% Main project structure - define child components
component(react_fastapi_template, child, react_fastapi_template(frontend)).
component(react_fastapi_template, child, react_fastapi_template(backend)).
component(react_fastapi_template, child, react_fastapi_template(tests)).

% Source file entities with docstrings for key files
entity(react_fastapi_template(source(file('./flake.nix')))).
docstring(react_fastapi_template(source(file('./flake.nix'))), "Nix flake configuration for reproducible builds with buildNpmPackage and multi-service management").

entity(react_fastapi_template(source(file('./docker-compose.yml')))).
docstring(react_fastapi_template(source(file('./docker-compose.yml'))), "Docker Compose configuration for containerized development environment").

entity(react_fastapi_template(source(file('./README.md')))).
docstring(react_fastapi_template(source(file('./README.md'))), "Project documentation and setup instructions").

% Make source files children for hierarchy
component(react_fastapi_template, child, react_fastapi_template(source(file('./flake.nix')))).
component(react_fastapi_template, child, react_fastapi_template(source(file('./docker-compose.yml')))).
component(react_fastapi_template, child, react_fastapi_template(source(file('./README.md')))).

% Load semantic knowledge from subcomponents first
:- load_entity(semantic(folder('./frontend'))).
:- load_entity(semantic(folder('./backend'))).
:- load_entity(semantic(folder('./tests'))).

% Then discover semantic artifacts to build hierarchy
:- discover_semantic_artifacts(react_fastapi_template, '.').
