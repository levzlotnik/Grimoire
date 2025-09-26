% Frontend assets directory semantics
% Static assets like images and icons

:- self_entity(react_fastapi_template(frontend(src(assets)))).

% Assets directory docstring
docstring(react_fastapi_template(frontend(src(assets))), "Static assets including React logo and other images").

% Asset files as children
component(react_fastapi_template(frontend(src(assets))), child, react_fastapi_template(frontend(src(assets(source(file('./react.svg'))))))).

% Asset file entities with docstrings
entity(react_fastapi_template(frontend(src(assets(source(file('./react.svg'))))))).
docstring(react_fastapi_template(frontend(src(assets(source(file('./react.svg')))))), "React framework logo SVG file").

% Asset types
component(react_fastapi_template(frontend(src(assets))), type, logos).
component(react_fastapi_template(frontend(src(assets))), type, icons).