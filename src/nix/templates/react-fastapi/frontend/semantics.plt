% PLUnit tests for Frontend React Application Semantics
% Tests the completeness and consistency of frontend ECS declarations

:- begin_tests(react_fastapi_frontend).

% Test self-entity declaration
test(frontend_entity_declared) :-
    entity(react_fastapi_template(frontend)).

% Test main docstring exists
test(frontend_has_docstring) :-
    docstring(react_fastapi_template(frontend), _).

% Test technology stack components
test(frontend_technology_stack) :-
    component(react_fastapi_template(frontend), framework, react),
    component(react_fastapi_template(frontend), language, typescript),
    component(react_fastapi_template(frontend), build_tool, vite),
    component(react_fastapi_template(frontend), styling, tailwindcss).

% Test syntax highlighting components
test(syntax_highlighting_configured) :-
    component(react_fastapi_template(frontend), syntax_highlighting, react_syntax_highlighter),
    component(react_fastapi_template(frontend), syntax_themes, Themes),
    is_list(Themes),
    member(oneDark, Themes),
    member(vscDarkPlus, Themes).

% Test main application structure entities
test(main_app_entities_declared) :-
    entity(app_component),
    entity(navigation_component).

% Test main app components
test(app_component_structure) :-
    component(app_component, file_path, "src/App.tsx"),
    component(app_component, main_routes, Routes),
    is_list(Routes),
    member(landing, Routes),
    member(dashboard, Routes),
    member(documentation, Routes),
    member(showcase, Routes).

% Test navigation component
test(navigation_component_structure) :-
    component(navigation_component, file_path, "src/components/Navigation.tsx"),
    component(navigation_component, nav_items, NavItems),
    is_list(NavItems),
    member(home, NavItems),
    member(documentation, NavItems).

% Test page entities are declared
test(page_entities_declared) :-
    entity(landing_page),
    entity(dashboard_page).

% Test landing page structure
test(landing_page_structure) :-
    component(landing_page, file_path, "src/pages/Landing.tsx"),
    component(landing_page, sections, Sections),
    is_list(Sections),
    member(hero, Sections),
    member(features, Sections).

% Test dashboard page structure
test(dashboard_page_structure) :-
    component(dashboard_page, file_path, "src/pages/Dashboard.tsx"),
    component(dashboard_page, charts, Charts),
    is_list(Charts),
    member(device_distribution_pie, Charts),
    component(dashboard_page, data_display, DataDisplay),
    is_list(DataDisplay),
    member(stat_cards, DataDisplay).

% Test documentation system entities
test(documentation_entities_declared) :-
    entity(documentation_system),
    entity(getting_started_docs),
    entity(api_reference_docs),
    entity(code_examples_docs),
    entity(deployment_docs).

% Test documentation system structure
test(documentation_system_structure) :-
    component(documentation_system, file_path, "src/pages/Documentation/index.tsx"),
    component(documentation_system, tab_structure, Tabs),
    is_list(Tabs),
    member(getting_started, Tabs),
    member(api_reference, Tabs),
    member(code_examples, Tabs),
    member(deployment, Tabs).

% Test code examples documentation
test(code_examples_structure) :-
    component(code_examples_docs, file_path, "src/pages/Documentation/CodeExamples.tsx"),
    component(code_examples_docs, language_sections, Languages),
    is_list(Languages),
    member(javascript_typescript, Languages),
    member(python, Languages),
    member(cpp, Languages),
    member(rust, Languages),
    member(haskell, Languages),
    member(prolog, Languages).

% Test code display component entities
test(code_display_entities_declared) :-
    entity(code_with_output_block),
    entity(command_with_output_block),
    entity(endpoint_block),
    entity(code_block_with_output).

% Test code with output block structure
test(code_with_output_block_structure) :-
    component(code_with_output_block, file_path, "src/components/CodeWithOutputBlock.tsx"),
    component(code_with_output_block, features, Features),
    is_list(Features),
    member(syntax_highlighting, Features),
    member(copy_button, Features),
    component(code_with_output_block, width_constraint, "120ch max-width").

% Test success states and coloring
test(output_coloring_configured) :-
    component(code_with_output_block, success_states, States),
    is_list(States),
    member(success, States),
    member(error, States),
    component(code_with_output_block, error_keywords, ErrorKeywords),
    is_list(ErrorKeywords),
    member(error, ErrorKeywords),
    member(fatal, ErrorKeywords),
    member(critical, ErrorKeywords).

% Test command with output block
test(command_with_output_block_structure) :-
    component(command_with_output_block, file_path, "src/components/CommandWithOutputBlock.tsx"),
    component(command_with_output_block, shell_types, ShellTypes),
    is_list(ShellTypes),
    member(bash, ShellTypes),
    member(shell, ShellTypes),
    member(zsh, ShellTypes).

% Test showcase system entities
test(showcase_entities_declared) :-
    entity(showcase_system),
    entity(alerts_showcase),
    entity(buttons_showcase),
    entity(cards_showcase),
    entity(feedback_showcase),
    entity(forms_showcase),
    entity(modals_showcase),
    entity(navigation_showcase),
    entity(tables_showcase).

% Test showcase system structure
test(showcase_system_structure) :-
    component(showcase_system, file_path, "src/pages/Showcase/index.tsx"),
    component(showcase_system, showcase_categories, Categories),
    is_list(Categories),
    member(alerts, Categories),
    member(buttons, Categories),
    member(cards, Categories).

% Test chart component entities
test(chart_entities_declared) :-
    entity(device_distribution_pie_chart),
    entity(hourly_activity_line_chart),
    entity(monthly_finance_bar_chart).

% Test chart component structure
test(chart_components_structure) :-
    component(device_distribution_pie_chart, chart_type, pie),
    component(hourly_activity_line_chart, chart_type, line),
    component(monthly_finance_bar_chart, chart_type, bar),
    component(hourly_activity_line_chart, time_range, "24 hours"),
    component(monthly_finance_bar_chart, time_range, "12 months").

% Test utility component entities
test(utility_entities_declared) :-
    entity(stat_card),
    entity(transaction_table).

% Test utility component structure
test(utility_components_structure) :-
    component(stat_card, file_path, "src/components/StatCard.tsx"),
    component(stat_card, display_elements, Elements),
    is_list(Elements),
    member(icon, Elements),
    member(title, Elements),
    component(transaction_table, columns, Columns),
    is_list(Columns),
    member(date, Columns),
    member(amount, Columns).

% Test service entities
test(service_entities_declared) :-
    entity(api_client_service),
    entity(api_types).

% Test service structure
test(service_structure) :-
    component(api_client_service, file_path, "src/services/apiClient.ts"),
    component(api_client_service, base_url, "http://localhost:8000"),
    component(api_types, file_path, "src/types/api.ts"),
    component(api_types, type_definitions, Types),
    is_list(Types),
    member(api_endpoint, Types).

% Test build configuration
test(build_configuration) :-
    component(react_fastapi_template(frontend), package_manager, npm),
    component(react_fastapi_template(frontend), main_dependencies, MainDeps),
    is_list(MainDeps),
    member(react, MainDeps),
    member(typescript, MainDeps),
    member(vite, MainDeps),
    member(tailwindcss, MainDeps).

% Test syntax highlighting dependencies
test(syntax_highlighting_dependencies) :-
    component(react_fastapi_template(frontend), syntax_highlighting_deps, SyntaxDeps),
    is_list(SyntaxDeps),
    member(react_syntax_highlighter, SyntaxDeps),
    member(prismjs, SyntaxDeps).

% Test file structure
test(file_structure_complete) :-
    component(react_fastapi_template(frontend), entry_point, "src/main.tsx"),
    component(react_fastapi_template(frontend), index_html, "index.html"),
    component(react_fastapi_template(frontend), config_files, ConfigFiles),
    is_list(ConfigFiles),
    member("package.json", ConfigFiles),
    member("tsconfig.json", ConfigFiles).

% Test styling system
test(styling_system_complete) :-
    component(react_fastapi_template(frontend), css_framework, tailwindcss),
    component(react_fastapi_template(frontend), main_stylesheet, "src/index.css"),
    component(react_fastapi_template(frontend), component_stylesheet, "src/App.css").

% Test language support
test(language_support_complete) :-
    component(code_examples_docs, supported_languages, SupportedLangs),
    is_list(SupportedLangs),
    member(javascript, SupportedLangs),
    member(python, SupportedLangs),
    member(cpp, SupportedLangs),
    member(rust, SupportedLangs),
    member(haskell, SupportedLangs),
    member(prolog, SupportedLangs),
    component(code_examples_docs, syntax_highlighter_languages, HighlighterLangs),
    is_list(HighlighterLangs).

% Test width constraints and styling patterns
test(width_constraints_configured) :-
    component(code_with_output_block, width_constraint_pattern, _),
    component(code_with_output_block, horizontal_scrolling, enabled),
    component(code_with_output_block, line_wrapping, disabled).

% Test tab system styling
test(tab_system_styling) :-
    component(documentation_system, tab_padding, "p-6"),
    component(documentation_system, tab_background, _).

% Test color scheme completeness
test(color_scheme_complete) :-
    component(code_with_output_block, success_color, "text-green-400"),
    component(code_with_output_block, error_color, "text-red-400"),
    component(code_with_output_block, warning_color, "text-yellow-400").

:- end_tests(react_fastapi_frontend).