% Frontend semantics for React FastAPI Template
% React application with TypeScript, Vite, and Tailwind CSS

:- self_entity(react_fastapi_template(frontend)).

% Frontend docstring
docstring(react_fastapi_template(frontend), "React frontend application with TypeScript, Vite build system, Tailwind CSS, and dashboard components").

% Frontend child components
component(react_fastapi_template(frontend), child, react_fastapi_template(frontend(src))).
component(react_fastapi_template(frontend), child, react_fastapi_template(frontend(source(file('./package.json'))))).
component(react_fastapi_template(frontend), child, react_fastapi_template(frontend(source(file('./vite.config.ts'))))).
component(react_fastapi_template(frontend), child, react_fastapi_template(frontend(source(file('./tsconfig.json'))))).
component(react_fastapi_template(frontend), child, react_fastapi_template(frontend(source(file('./index.html'))))).
component(react_fastapi_template(frontend), child, react_fastapi_template(frontend(source(file('./Dockerfile'))))).

% Configuration file entities with docstrings
entity(react_fastapi_template(frontend(source(file('./package.json'))))).
docstring(react_fastapi_template(frontend(source(file('./package.json')))), "Node.js package configuration with React 19, TypeScript, Vite, and Tailwind dependencies").

entity(react_fastapi_template(frontend(source(file('./vite.config.ts'))))).
docstring(react_fastapi_template(frontend(source(file('./vite.config.ts')))), "Vite build configuration with proxy setup for backend API").

entity(react_fastapi_template(frontend(source(file('./tsconfig.json'))))).
docstring(react_fastapi_template(frontend(source(file('./tsconfig.json')))), "TypeScript configuration for React application").

entity(react_fastapi_template(frontend(source(file('./index.html'))))).
docstring(react_fastapi_template(frontend(source(file('./index.html')))), "Main HTML template with React mount point").

entity(react_fastapi_template(frontend(source(file('./Dockerfile'))))).
docstring(react_fastapi_template(frontend(source(file('./Dockerfile')))), "Docker container configuration for frontend service").

% Frontend technology stack
component(react_fastapi_template(frontend), framework, react).
component(react_fastapi_template(frontend), language, typescript).
component(react_fastapi_template(frontend), build_tool, vite).
component(react_fastapi_template(frontend), css_framework, tailwind).
component(react_fastapi_template(frontend), router, react_router).
component(react_fastapi_template(frontend), charts, recharts).
component(react_fastapi_template(frontend), icons, lucide_react).
component(react_fastapi_template(frontend), http_client, fetch).

% Load subdirectory semantics
:- load_entity(semantic(folder('./src'))).