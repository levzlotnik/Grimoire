% Frontend React Application Semantics
% Knowledge representation of the React TypeScript frontend component

% Self-entity declaration pattern
:- self_entity(react_fastapi_template(frontend)).

docstring(react_fastapi_template(frontend), "React TypeScript frontend with Vite build system, TailwindCSS styling, comprehensive documentation system with multi-language code examples, and interactive component showcases.").

% Technology stack
component(react_fastapi_template(frontend), framework, react).
component(react_fastapi_template(frontend), language, typescript).
component(react_fastapi_template(frontend), build_tool, vite).
component(react_fastapi_template(frontend), styling, tailwindcss).
component(react_fastapi_template(frontend), syntax_highlighting, react_syntax_highlighter).
component(react_fastapi_template(frontend), syntax_themes, [oneDark, vscDarkPlus]).

% Main application structure
entity(app_component).
component(app_component, file_path, "src/App.tsx").
component(app_component, main_routes, [landing, dashboard, documentation, showcase]).
component(app_component, navigation_component, navigation).

% Navigation system
entity(navigation_component).
component(navigation_component, file_path, "src/components/Navigation.tsx").
component(navigation_component, nav_items, [home, dashboard, documentation, showcase]).
component(navigation_component, styling, "sticky top-0 bg-white shadow-sm").

% Page components
entity(landing_page).
component(landing_page, file_path, "src/pages/Landing.tsx").
component(landing_page, sections, [hero, features, quick_start, tech_stack]).

entity(dashboard_page).
component(dashboard_page, file_path, "src/pages/Dashboard.tsx").
component(dashboard_page, charts, [device_distribution_pie, hourly_activity_line, monthly_finance_bar]).
component(dashboard_page, data_display, [stat_cards, transaction_table]).

% Documentation system
entity(documentation_system).
component(documentation_system, file_path, "src/pages/Documentation/index.tsx").
component(documentation_system, tab_structure, [getting_started, api_reference, code_examples, deployment]).
component(documentation_system, styling, "p-6 padding with tab content").

entity(getting_started_docs).
component(getting_started_docs, file_path, "src/pages/Documentation/GettingStarted.tsx").
component(getting_started_docs, sections, [overview, installation, quick_start, environment_setup]).

entity(api_reference_docs).
component(api_reference_docs, file_path, "src/pages/Documentation/ApiReference.tsx").
component(api_reference_docs, features, [endpoint_filtering, interactive_examples, openapi_integration]).
component(api_reference_docs, components, [endpoint_block]).

entity(code_examples_docs).
component(code_examples_docs, file_path, "src/pages/Documentation/CodeExamples.tsx").
component(code_examples_docs, language_sections, [javascript_typescript, python, cpp, rust, haskell, prolog]).
component(code_examples_docs, example_types, [success_examples, error_examples]).
component(code_examples_docs, features, [syntax_highlighting, copy_functionality, output_display]).

entity(deployment_docs).
component(deployment_docs, file_path, "src/pages/Documentation/Deployment.tsx").
component(deployment_docs, deployment_methods, [docker, nix, manual]).

% Code display components
entity(code_with_output_block).
component(code_with_output_block, file_path, "src/components/CodeWithOutputBlock.tsx").
component(code_with_output_block, features, [syntax_highlighting, line_numbers, copy_button, output_display]).
component(code_with_output_block, width_constraint, "120ch max-width").
component(code_with_output_block, success_states, [success, error]).
component(code_with_output_block, output_coloring, [error_red, warning_yellow, normal_white]).

entity(command_with_output_block).
component(command_with_output_block, file_path, "src/components/CommandWithOutputBlock.tsx").
component(command_with_output_block, shell_types, [bash, shell, zsh, fish, powershell, cmd]).
component(command_with_output_block, features, [syntax_highlighting, copy_button, output_display]).

entity(endpoint_block).
component(endpoint_block, file_path, "src/components/EndpointBlock.tsx").
component(endpoint_block, features, [method_badges, example_requests, example_responses]).
component(endpoint_block, http_methods, [get, post, put, delete, patch]).

% Legacy code block component (alternative implementation)
entity(code_block_with_output).
component(code_block_with_output, file_path, "src/components/CodeBlockWithOutput.tsx").
component(code_block_with_output, code_types, [shell, repl, programming]).
component(code_block_with_output, themes, [oneDark, dark]).

% Showcase system
entity(showcase_system).
component(showcase_system, file_path, "src/pages/Showcase/index.tsx").
component(showcase_system, showcase_categories, [alerts, buttons, cards, feedback, forms, modals, navigation, tables]).

entity(alerts_showcase).
component(alerts_showcase, file_path, "src/pages/Showcase/AlertsShowcase.tsx").

entity(buttons_showcase).
component(buttons_showcase, file_path, "src/pages/Showcase/ButtonsShowcase.tsx").

entity(cards_showcase).
component(cards_showcase, file_path, "src/pages/Showcase/CardsShowcase.tsx").

entity(feedback_showcase).
component(feedback_showcase, file_path, "src/pages/Showcase/FeedbackShowcase.tsx").

entity(forms_showcase).
component(forms_showcase, file_path, "src/pages/Showcase/FormsShowcase.tsx").

entity(modals_showcase).
component(modals_showcase, file_path, "src/pages/Showcase/ModalsShowcase.tsx").

entity(navigation_showcase).
component(navigation_showcase, file_path, "src/pages/Showcase/NavigationShowcase.tsx").

entity(tables_showcase).
component(tables_showcase, file_path, "src/pages/Showcase/TablesShowcase.tsx").

% Chart components for dashboard
entity(device_distribution_pie_chart).
component(device_distribution_pie_chart, file_path, "src/components/DeviceDistributionPieChart.tsx").
component(device_distribution_pie_chart, chart_type, pie).
component(device_distribution_pie_chart, data_categories, [desktop, mobile, tablet]).

entity(hourly_activity_line_chart).
component(hourly_activity_line_chart, file_path, "src/components/HourlyActivityLineChart.tsx").
component(hourly_activity_line_chart, chart_type, line).
component(hourly_activity_line_chart, time_range, "24 hours").

entity(monthly_finance_bar_chart).
component(monthly_finance_bar_chart, file_path, "src/components/MonthlyFinanceBarChart.tsx").
component(monthly_finance_bar_chart, chart_type, bar).
component(monthly_finance_bar_chart, time_range, "12 months").

% Utility components
entity(stat_card).
component(stat_card, file_path, "src/components/StatCard.tsx").
component(stat_card, display_elements, [icon, title, value, trend]).

entity(transaction_table).
component(transaction_table, file_path, "src/components/TransactionTable.tsx").
component(transaction_table, columns, [date, description, amount, category, status]).

% API client service
entity(api_client_service).
component(api_client_service, file_path, "src/services/apiClient.ts").
component(api_client_service, endpoints, [get_api_endpoints]).
component(api_client_service, base_url, "http://localhost:8000").

% Type definitions
entity(api_types).
component(api_types, file_path, "src/types/api.ts").
component(api_types, type_definitions, [api_endpoint, api_docs_response, http_method, parameter, response]).

% Build and configuration
component(react_fastapi_template(frontend), package_manager, npm).
component(react_fastapi_template(frontend), main_dependencies, [react, typescript, vite, tailwindcss]).
component(react_fastapi_template(frontend), syntax_highlighting_deps, [react_syntax_highlighter, prismjs]).
component(react_fastapi_template(frontend), dev_dependencies, [eslint, typescript_eslint]).

% File structure
component(react_fastapi_template(frontend), entry_point, "src/main.tsx").
component(react_fastapi_template(frontend), index_html, "index.html").
component(react_fastapi_template(frontend), config_files, ["package.json", "tsconfig.json", "vite.config.ts", "tailwind.config.js"]).

% Styling system
component(react_fastapi_template(frontend), css_framework, tailwindcss).
component(react_fastapi_template(frontend), main_stylesheet, "src/index.css").
component(react_fastapi_template(frontend), component_stylesheet, "src/App.css").

% Code examples language support
component(code_examples_docs, supported_languages, [javascript, typescript, python, cpp, rust, haskell, prolog]).
component(code_examples_docs, syntax_highlighter_languages, [javascript, python, cpp, rust, haskell, prolog]).
component(code_examples_docs, example_categories, [api_client, error_handling, authentication]).

% Width constraints and styling patterns
component(code_with_output_block, width_constraint_pattern, "width: fit-content, maxWidth: 120ch").
component(code_with_output_block, horizontal_scrolling, enabled).
component(code_with_output_block, line_wrapping, disabled).

% Output coloring patterns
component(code_with_output_block, error_keywords, [error, fatal, critical]).
component(code_with_output_block, warning_keywords, [warning]).
component(code_with_output_block, success_color, "text-green-400").
component(code_with_output_block, error_color, "text-red-400").
component(code_with_output_block, warning_color, "text-yellow-400").

% Tab system styling
component(documentation_system, tab_padding, "p-6").
component(documentation_system, tab_background, "bg-white rounded-lg shadow-sm border border-gray-200").