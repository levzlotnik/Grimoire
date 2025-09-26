% PLUnit tests for React + FastAPI Template Semantics

:- begin_tests(react_fastapi_template).

% Load the template entity (which will load its children)
:- load_entity(semantic(folder('.'))).

% Test basic entity declaration
test(entity_declared) :-
    entity(react_fastapi_template).

% Test project type identification
test(project_type_fullstack) :-
    component(react_fastapi_template, project_type, fullstack_web).

test(build_system_nix) :-
    component(react_fastapi_template, build_system, nix).

test(frontend_framework_react) :-
    component(react_fastapi_template, frontend_framework, react).

test(backend_framework_fastapi) :-
    component(react_fastapi_template, backend_framework, fastapi).

% Test available subcommands
test(has_run_subcommand) :-
    component(react_fastapi_template, subcommand, run).

test(has_dev_subcommand) :-
    component(react_fastapi_template, subcommand, dev).

test(has_test_subcommands) :-
    component(react_fastapi_template, subcommand, test),
    component(react_fastapi_template, subcommand, test_backend),
    component(react_fastapi_template, subcommand, test_frontend).

% Test that subcommands are available as ctors
test(subcommands_as_ctors) :-
    component(react_fastapi_template, ctor, run),
    component(react_fastapi_template, ctor, dev),
    component(react_fastapi_template, ctor, test).

% Test docstring existence
test(has_main_docstring) :-
    docstring(react_fastapi_template, _).

test(has_subcommand_docstrings) :-
    docstring(react_fastapi_template(run), _),
    docstring(react_fastapi_template(dev), _),
    docstring(react_fastapi_template(test), _).

% Test main project structure
test(has_main_children) :-
    component(react_fastapi_template, child, react_fastapi_template(frontend)),
    component(react_fastapi_template, child, react_fastapi_template(backend)),
    component(react_fastapi_template, child, react_fastapi_template(tests)).

% Test configuration files
test(has_config_files) :-
    entity(react_fastapi_template(source(file('./flake.nix')))),
    entity(react_fastapi_template(source(file('./docker-compose.yml')))),
    entity(react_fastapi_template(source(file('./README.md')))).

test(config_files_have_docstrings) :-
    docstring(react_fastapi_template(source(file('./flake.nix'))), _),
    docstring(react_fastapi_template(source(file('./docker-compose.yml'))), _),
    docstring(react_fastapi_template(source(file('./README.md'))), _).

test(config_files_are_children) :-
    component(react_fastapi_template, child, react_fastapi_template(source(file('./flake.nix')))),
    component(react_fastapi_template, child, react_fastapi_template(source(file('./docker-compose.yml')))),
    component(react_fastapi_template, child, react_fastapi_template(source(file('./README.md')))).

% Test that the entity is available as a conjure target
test(conjure_target_available) :-
    component(conjure, ctor, react_fastapi_template).

% Test command implementations - verify spell clauses are defined
test(conjure_spells_defined) :-
    clause(cast(conjure(react_fastapi_template(run)), _), _),
    clause(cast(conjure(react_fastapi_template(dev)), _), _),
    clause(cast(conjure(react_fastapi_template(test)), _), _),
    clause(cast(conjure(react_fastapi_template(test_backend)), _), _),
    clause(cast(conjure(react_fastapi_template(test_frontend)), _), _).

% Test that conjure spells delegate to nix(run(...))
test(conjure_spells_delegate_to_nix) :-
    clause(cast(conjure(react_fastapi_template(run)), _), Body),
    contains_nix_run_call(Body),
    clause(cast(conjure(react_fastapi_template(dev)), _), Body2),
    contains_nix_run_call(Body2).

% Helper to check if clause body contains nix(run(...)) call
contains_nix_run_call(cast(conjure(nix(run(_))), _)) :- !.
contains_nix_run_call((A, _)) :- contains_nix_run_call(A), !.
contains_nix_run_call((_, B)) :- contains_nix_run_call(B), !.

% Test actual files exist on filesystem
test(filesystem_files_exist) :-
    exists_file('flake.nix'),
    exists_file('docker-compose.yml'),
    exists_file('README.md'),
    exists_directory('frontend'),
    exists_directory('backend'),
    exists_directory('tests').

% Test all key semantics.pl files exist
test(semantics_files_exist) :-
    exists_file('semantics.pl'),
    exists_file('semantics.plt'),
    exists_file('frontend/semantics.pl'),
    exists_file('frontend/semantics.plt'),
    exists_file('backend/semantics.pl'),
    exists_file('backend/semantics.plt'),
    exists_file('tests/semantics.pl'),
    exists_file('tests/semantics.plt').

% Test that child entities are properly defined
test(child_entities_exist) :-
    entity(react_fastapi_template(frontend)),
    entity(react_fastapi_template(backend)),
    entity(react_fastapi_template(tests)).

% Test entity hierarchy contains expected structure
test(entity_hierarchy_structure) :-
    perceive(entity_hierarchy(react_fastapi_template, Tree)),
    Tree = tree(react_fastapi_template, Children),
    % Verify main children are included
    member(tree(react_fastapi_template(frontend), _), Children),
    member(tree(react_fastapi_template(backend), _), Children),
    member(tree(react_fastapi_template(tests), _), Children),
    % Verify source files are included
    member(tree(react_fastapi_template(source(file('./flake.nix'))), _), Children),
    member(tree(react_fastapi_template(source(file('./docker-compose.yml'))), _), Children),
    member(tree(react_fastapi_template(source(file('./README.md'))), _), Children),
    % Verify subcommands are included (via subscription)
    member(tree(react_fastapi_template(run), _), Children),
    member(tree(react_fastapi_template(dev), _), Children),
    member(tree(react_fastapi_template(test), _), Children).

% Test that all tracked files can be discovered via git
test(git_ls_files_works) :-
    perceive(git(ls_files('.', Files))),
    length(Files, Count),
    Count > 50,  % We know there are many files in this template
    % Check some key files are in the list
    member('flake.nix', Files),
    member('README.md', Files),
    member('frontend/package.json', Files),
    member('backend/main.py', Files).

% Test that semantics files are discoverable
test(all_semantics_files_discovered) :-
    perceive(git(ls_files('.', AllFiles))),
    include(test_is_semantics_file, AllFiles, SemanticFiles),
    length(SemanticFiles, Count),
    % Debug: print what we found
    format('Found ~w semantics files~n', [Count]),
    Count >= 16,  % We have at least 8 pairs of .pl/.plt files
    % Verify specific semantics files are found
    member('semantics.pl', SemanticFiles),
    member('frontend/src/components/semantics.pl', SemanticFiles),
    member('frontend/src/pages/Documentation/semantics.pl', SemanticFiles).

% Helper predicate for test
test_is_semantics_file(Path) :-
    (sub_atom(Path, _, _, 0, 'semantics.pl') ; sub_atom(Path, _, _, 0, 'semantics.plt')).

:- end_tests(react_fastapi_template).