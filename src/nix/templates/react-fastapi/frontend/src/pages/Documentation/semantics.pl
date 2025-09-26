% Documentation pages directory semantics
% Documentation system with multiple sections

:- self_entity(react_fastapi_template(frontend(src(pages(documentation))))).

% Documentation directory docstring
docstring(react_fastapi_template(frontend(src(pages(documentation)))), "Documentation system with Getting Started, API Reference, Code Examples, and Deployment guides").

% Documentation sections
component(react_fastapi_template(frontend(src(pages(documentation)))), section, getting_started).
component(react_fastapi_template(frontend(src(pages(documentation)))), section, api_reference).
component(react_fastapi_template(frontend(src(pages(documentation)))), section, code_examples).
component(react_fastapi_template(frontend(src(pages(documentation)))), section, deployment).