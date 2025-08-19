:- use_module(library(plunit)).

% Test suite for self_entity introspection mechanism
% Ensure relevant semantics are loaded for the tests
:- begin_tests(self_entity).

:- load_entity(semantic(file('src/grimoire.pl'))).
:- load_entity(semantic(file('src/project/semantics.pl'))).
:- load_entity(semantic(file('src/db/semantics.pl'))).
:- load_entity(semantic(file('src/nix/templates/python/semantics.pl'))).
:- load_entity(semantic(file('src/nix/templates/cpp/semantics.pl'))).
:- load_entity(semantic(file('src/nix/templates/mkdocs/semantics.pl'))).
:- load_entity(semantic(file('src/nix/templates/lean4/semantics.pl'))).
:- load_entity(semantic(file('src/nix/templates/haskell/semantics.pl'))).
:- load_entity(semantic(file('src/nix/templates/rust/semantics.pl'))).

% === SELF ENTITY MECHANISM TESTS ===

test(git_has_self_component) :-
        % Test that git entity has self component pointing to its file
        once(( component(git, self, Self),
                     Self = semantic(file(FilePath)),
                     ( string(FilePath) -> sub_string(FilePath, _, _, 0, "git.pl")
                     ; atom(FilePath) -> sub_atom(FilePath, _, _, 0, 'git.pl')
                     )
                 )).

test(session_has_self_component) :-
        % Test that session entity has self component pointing to its file
        once(( component(session, self, Self),
                     Self = semantic(file(FilePath)),
                     ( string(FilePath) -> sub_string(FilePath, _, _, 0, "session.pl")
                     ; atom(FilePath) -> sub_atom(FilePath, _, _, 0, 'session.pl')
                     )
                 )).

test(nix_has_self_folder_component) :-
        % Test that nix entity has self component pointing to its folder (semantics.pl pattern)
        once(( component(nix, self, Self),
                     Self = semantic(folder(FolderPath)),
                     ( string(FolderPath) -> sub_string(FolderPath, _, _, 0, "/nix")
                     ; atom(FolderPath) -> sub_atom(FolderPath, _, _, 0, '/nix')
                     )
                 )).

test(fs_has_self_component) :-
        % Test that fs entity has self component pointing to its file
        once(( component(fs, self, Self),
                     Self = semantic(file(FilePath)),
                     ( string(FilePath) -> sub_string(FilePath, _, _, 0, "fs.pl")
                     ; atom(FilePath) -> sub_atom(FilePath, _, _, 0, 'fs.pl')
                     )
                 )).

test(self_entity_creates_entity) :-
    % Test that self_entity/1 properly creates the entity
    entity(git),
    entity(session),
    entity(nix),
    entity(fs).

test(self_components_are_values) :-
    % Test that self components are marked as values (not entities)
    once(( component(git, self, GitSelf), \+ entity(GitSelf) )),
    once(( component(nix, self, NixSelf), \+ entity(NixSelf) )).

% Additional tests for other semantics files
test(system_has_self_component) :-
    component(system, self, Self),
    Self = semantic(folder(_)).

test(project_has_self_component) :-
    component(project, self, Self),
    ( Self = semantic(folder(_)) ; Self = semantic(file(_)) ).

test(db_has_self_component) :-
    component(db, self, Self),
    Self = semantic(folder(_)).

test(python_template_self) :-
    component(python_template, self, Self),
    ( Self = semantic(folder(_)) ; Self = semantic(file(_)) ).

test(cpp_template_self) :-
    component(cpp_template, self, Self),
    ( Self = semantic(folder(_)) ; Self = semantic(file(_)) ).

test(mkdocs_template_self) :-
    component(mkdocs_template, self, Self),
    ( Self = semantic(folder(_)) ; Self = semantic(file(_)) ).

test(lean4_template_self) :-
    component(lean4_template, self, Self),
    ( Self = semantic(folder(_)) ; Self = semantic(file(_)) ).

test(haskell_template_self) :-
    component(haskell_template, self, Self),
    ( Self = semantic(folder(_)) ; Self = semantic(file(_)) ).

test(rust_template_self) :-
    component(rust_template, self, Self),
    ( Self = semantic(folder(_)) ; Self = semantic(file(_)) ).

:- end_tests(self_entity).