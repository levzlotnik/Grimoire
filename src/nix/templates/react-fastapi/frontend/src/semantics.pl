% Frontend src directory semantics
% Main source code structure for React application

:- self_entity(react_fastapi_template(frontend(src))).

% Src directory docstring
docstring(react_fastapi_template(frontend(src)), "Main source directory containing React application components, pages, services, and types").

% Src child components
component(react_fastapi_template(frontend(src)), child, react_fastapi_template(frontend(src(components)))).
component(react_fastapi_template(frontend(src)), child, react_fastapi_template(frontend(src(pages)))).
component(react_fastapi_template(frontend(src)), child, react_fastapi_template(frontend(src(services)))).
component(react_fastapi_template(frontend(src)), child, react_fastapi_template(frontend(src(types)))).
component(react_fastapi_template(frontend(src)), child, react_fastapi_template(frontend(src(assets)))).
component(react_fastapi_template(frontend(src)), child, react_fastapi_template(frontend(src(utils)))).

% Root src files
component(react_fastapi_template(frontend(src)), child, react_fastapi_template(frontend(src(source(file('./App.tsx')))))).
component(react_fastapi_template(frontend(src)), child, react_fastapi_template(frontend(src(source(file('./main.tsx')))))).
component(react_fastapi_template(frontend(src)), child, react_fastapi_template(frontend(src(source(file('./App.css')))))).
component(react_fastapi_template(frontend(src)), child, react_fastapi_template(frontend(src(source(file('./index.css')))))).
component(react_fastapi_template(frontend(src)), child, react_fastapi_template(frontend(src(source(file('./vite-env.d.ts')))))).

% Source file entities with docstrings
entity(react_fastapi_template(frontend(src(source(file('./App.tsx')))))).
docstring(react_fastapi_template(frontend(src(source(file('./App.tsx'))))), "Main React application component with routing setup for Landing, Dashboard, Documentation, and Showcase pages").

entity(react_fastapi_template(frontend(src(source(file('./main.tsx')))))).
docstring(react_fastapi_template(frontend(src(source(file('./main.tsx'))))), "React application entry point with root DOM mounting").

entity(react_fastapi_template(frontend(src(source(file('./App.css')))))).
docstring(react_fastapi_template(frontend(src(source(file('./App.css'))))), "Application-specific CSS styles").

entity(react_fastapi_template(frontend(src(source(file('./index.css')))))).
docstring(react_fastapi_template(frontend(src(source(file('./index.css'))))), "Global CSS styles and Tailwind imports").

entity(react_fastapi_template(frontend(src(source(file('./vite-env.d.ts')))))).
docstring(react_fastapi_template(frontend(src(source(file('./vite-env.d.ts'))))), "TypeScript definitions for Vite environment").

% Load subdirectory semantics
:- load_entity(semantic(folder('./components'))).
:- load_entity(semantic(folder('./pages'))).
:- load_entity(semantic(folder('./services'))).
:- load_entity(semantic(folder('./types'))).
:- load_entity(semantic(folder('./assets'))).
:- load_entity(semantic(folder('./utils'))).