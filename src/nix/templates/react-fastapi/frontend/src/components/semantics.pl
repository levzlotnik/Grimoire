% Frontend components directory semantics
% Reusable React components for the application

:- self_entity(react_fastapi_template(frontend(src(components)))).

% Components directory docstring
docstring(react_fastapi_template(frontend(src(components))), "Reusable React components including navigation, charts, code blocks, and UI elements").

% Component files as children
component(react_fastapi_template(frontend(src(components))), child, react_fastapi_template(frontend(src(components(source(file('./Navigation.tsx'))))))).
component(react_fastapi_template(frontend(src(components))), child, react_fastapi_template(frontend(src(components(source(file('./DeviceDistributionPieChart.tsx'))))))).
component(react_fastapi_template(frontend(src(components))), child, react_fastapi_template(frontend(src(components(source(file('./HourlyActivityLineChart.tsx'))))))).
component(react_fastapi_template(frontend(src(components))), child, react_fastapi_template(frontend(src(components(source(file('./MonthlyFinanceBarChart.tsx'))))))).
component(react_fastapi_template(frontend(src(components))), child, react_fastapi_template(frontend(src(components(source(file('./StatCard.tsx'))))))).
component(react_fastapi_template(frontend(src(components))), child, react_fastapi_template(frontend(src(components(source(file('./TransactionTable.tsx'))))))).
component(react_fastapi_template(frontend(src(components))), child, react_fastapi_template(frontend(src(components(source(file('./CodeBlock.tsx'))))))).
component(react_fastapi_template(frontend(src(components))), child, react_fastapi_template(frontend(src(components(source(file('./CodeBlockWithOutput.tsx'))))))).
component(react_fastapi_template(frontend(src(components))), child, react_fastapi_template(frontend(src(components(source(file('./CodeWithOutputBlock.tsx'))))))).
component(react_fastapi_template(frontend(src(components))), child, react_fastapi_template(frontend(src(components(source(file('./CommandWithOutputBlock.tsx'))))))).
component(react_fastapi_template(frontend(src(components))), child, react_fastapi_template(frontend(src(components(source(file('./EndpointBlock.tsx'))))))).

% Component entities with docstrings
entity(react_fastapi_template(frontend(src(components(source(file('./Navigation.tsx'))))))).
docstring(react_fastapi_template(frontend(src(components(source(file('./Navigation.tsx')))))), "Main navigation component with sticky positioning and route links").

entity(react_fastapi_template(frontend(src(components(source(file('./DeviceDistributionPieChart.tsx'))))))).
docstring(react_fastapi_template(frontend(src(components(source(file('./DeviceDistributionPieChart.tsx')))))), "Recharts pie chart component for device usage distribution data").

entity(react_fastapi_template(frontend(src(components(source(file('./HourlyActivityLineChart.tsx'))))))).
docstring(react_fastapi_template(frontend(src(components(source(file('./HourlyActivityLineChart.tsx')))))), "Recharts line chart component for hourly activity data").

entity(react_fastapi_template(frontend(src(components(source(file('./MonthlyFinanceBarChart.tsx'))))))).
docstring(react_fastapi_template(frontend(src(components(source(file('./MonthlyFinanceBarChart.tsx')))))), "Recharts bar chart component for monthly revenue and expense data").

entity(react_fastapi_template(frontend(src(components(source(file('./StatCard.tsx'))))))).
docstring(react_fastapi_template(frontend(src(components(source(file('./StatCard.tsx')))))), "Dashboard statistic card component with icon, title, and value").

entity(react_fastapi_template(frontend(src(components(source(file('./TransactionTable.tsx'))))))).
docstring(react_fastapi_template(frontend(src(components(source(file('./TransactionTable.tsx')))))), "Data table component for displaying recent transactions").

entity(react_fastapi_template(frontend(src(components(source(file('./CodeBlock.tsx'))))))).
docstring(react_fastapi_template(frontend(src(components(source(file('./CodeBlock.tsx')))))), "Syntax-highlighted code block component with copy functionality").

entity(react_fastapi_template(frontend(src(components(source(file('./CodeBlockWithOutput.tsx'))))))).
docstring(react_fastapi_template(frontend(src(components(source(file('./CodeBlockWithOutput.tsx')))))), "Code block component with execution output display").

entity(react_fastapi_template(frontend(src(components(source(file('./CodeWithOutputBlock.tsx'))))))).
docstring(react_fastapi_template(frontend(src(components(source(file('./CodeWithOutputBlock.tsx')))))), "Alternative code block component with output and success/error states").

entity(react_fastapi_template(frontend(src(components(source(file('./CommandWithOutputBlock.tsx'))))))).
docstring(react_fastapi_template(frontend(src(components(source(file('./CommandWithOutputBlock.tsx')))))), "Shell command display component with output").

entity(react_fastapi_template(frontend(src(components(source(file('./EndpointBlock.tsx'))))))).
docstring(react_fastapi_template(frontend(src(components(source(file('./EndpointBlock.tsx')))))), "API endpoint documentation component with method, path, and examples").

% Component categories
component(react_fastapi_template(frontend(src(components))), category, navigation).
component(react_fastapi_template(frontend(src(components))), category, charts).
component(react_fastapi_template(frontend(src(components))), category, code_display).
component(react_fastapi_template(frontend(src(components))), category, dashboard).
component(react_fastapi_template(frontend(src(components))), category, documentation).