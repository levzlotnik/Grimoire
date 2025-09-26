% Frontend pages directory semantics
% Main application pages and nested page directories

:- self_entity(react_fastapi_template(frontend(src(pages)))).

% Pages directory docstring
docstring(react_fastapi_template(frontend(src(pages))), "Application pages including Landing, Dashboard, and nested Documentation and Showcase sections").

% Page child components
component(react_fastapi_template(frontend(src(pages))), child, react_fastapi_template(frontend(src(pages(documentation))))).
component(react_fastapi_template(frontend(src(pages))), child, react_fastapi_template(frontend(src(pages(showcase))))).

% Root page files
component(react_fastapi_template(frontend(src(pages))), child, react_fastapi_template(frontend(src(pages(source(file('./Landing.tsx'))))))).
component(react_fastapi_template(frontend(src(pages))), child, react_fastapi_template(frontend(src(pages(source(file('./Dashboard.tsx'))))))).

% Root page entities with docstrings
entity(react_fastapi_template(frontend(src(pages(source(file('./Landing.tsx'))))))).
docstring(react_fastapi_template(frontend(src(pages(source(file('./Landing.tsx')))))), "Landing page component with hero section, features overview, and tech stack information").

entity(react_fastapi_template(frontend(src(pages(source(file('./Dashboard.tsx'))))))).
docstring(react_fastapi_template(frontend(src(pages(source(file('./Dashboard.tsx')))))), "Main dashboard page with charts, statistics, and data visualization using Recharts").

% Load subdirectory semantics
:- load_entity(semantic(folder('./Documentation'))).
:- load_entity(semantic(folder('./Showcase'))).