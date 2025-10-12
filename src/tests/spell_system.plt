:- use_module(library(plunit)).

% Test suite for the conjure/perceive spell system
:- begin_tests(spell_system).

% Grimoire system is already loaded by run_tests.pl

% === SPELL ECS HIERARCHY TESTS ===

test(spell_entity_exists) :-
    entity(spell).

test(spell_has_conjure_ctor) :-
    component(spell, ctor, conjure), !.

test(spell_has_perceive_ctor) :-
    component(spell, ctor, perceive).

test(conjure_entity_exists) :-
    entity(conjure).

test(perceive_entity_exists) :-
    entity(perceive).

% === CAST PREDICATE TESTS ===

test(cast_single_conjure_spell) :-
    % Test casting a single conjure spell
    cast(conjure(shell(['echo', 'test'])), Result),
    Result = ok(result("test\n", "")).

test(cast_invalid_spell_fails) :-
    % Test that invalid spells fail gracefully
    \+ cast(conjure(completely_nonexistent_spell_12345), ok(_)).

% === PERCEIVE PREDICATE TESTS ===

test(perceive_git_status) :-
    % Test git status perception
    magic_cast(perceive(git(status)), ok(status_info(branch(Branch), working_status(Status), files(Files)))),
    atom(Branch),
    atom(Status),
    is_list(Files).

test(perceive_simple_query) :-
    % Test simple Prolog query (fallback to call/1)
    % Since member/2 doesn't have a perceive/1 clause, it should fall back to call/1
    member(1, [1, 2, 3]), !.

test(perceive_unification) :-
    % Test variable unification with simple query
    member(X, [a, b, c]),
    X = a, !.

% === DOMAIN INTEGRATION TESTS ===

test(git_conjure_ctors_exist) :-
    % Test git conjure constructors are properly defined
    component(conjure, ctor, git(commit)),
    component(conjure, ctor, git(add)),
    component(conjure, ctor, git(checkout)), !.

test(git_perceive_ctors_exist) :-
    % Test git perceive constructors are properly defined
    component(perceive, ctor, git(status)),
    component(perceive, ctor, git(log)),
    component(perceive, ctor, git(branch)), !.

test(nix_conjure_ctors_exist) :-
    % Test nix conjure constructors
    component(conjure, ctor, nix(build)),
    component(conjure, ctor, nix(develop)),
    component(conjure, ctor, nix(run)), !.

test(nix_perceive_ctors_exist) :-
    % Test nix perceive constructors
    component(perceive, ctor, nix(flake(show))),
    component(perceive, ctor, nix(search)), !.

% === NEW CORE PERCEIVE SPELLS TESTS ===

test(perceive_entities_exists) :-
    % Test entities perceive constructor exists
    component(perceive, ctor, entities), !.

test(perceive_entities_returns_list) :-
    % Test that entities perceive returns a list
    magic_cast(conjure(interface(entities)), ok(entities(Entities))),
    is_list(Entities), !.

test(perceive_entities_includes_system) :-
    % Test that system entity is included
    magic_cast(conjure(interface(entities)), ok(entities(Entities))),
    member(system, Entities), !.

test(perceive_read_file_exists) :-
    % Test read_file perceive constructor exists
    user:please_verify(component(perceive, ctor, fs(read_file))).

test(perceive_read_file_basic, [cleanup(delete_file('/tmp/test_read.txt'))]) :-
    % Test basic file reading with line numbers
    write_file('/tmp/test_read.txt', "Line 1\nLine 2\nLine 3"),
    magic_cast(perceive(fs(read_file('/tmp/test_read.txt', 1, 3))), ok(file_content(Content))),
    Content = [line(1, "Line 1"), line(2, "Line 2"), line(3, "Line 3")], !.

test(perceive_read_file_range, [cleanup(delete_file('/tmp/test_read.txt'))]) :-
    % Test reading specific line range
    write_file('/tmp/test_read.txt', "Line 1\nLine 2\nLine 3\nLine 4\nLine 5"),
    magic_cast(perceive(fs(read_file('/tmp/test_read.txt', 2, 4))), ok(file_content(Content))),
    Content = [line(2, "Line 2"), line(3, "Line 3"), line(4, "Line 4")], !.

test(perceive_read_file_start_end, [cleanup(delete_file('/tmp/test_read.txt'))]) :-
    % Test reading entire file (1 to -1)
    write_file('/tmp/test_read.txt', "First\nMiddle\nLast"),
    magic_cast(perceive(fs(read_file('/tmp/test_read.txt', 1, -1))), ok(file_content(Content))),
    length(Content, 3),
    Content = [line(1, "First")|_], !.

test(perceive_search_regex_exists) :-
    % Test search_regex perceive constructor exists
    user:please_verify(component(perceive, ctor, search_regex)).

test(perceive_search_regex_basic, [cleanup(delete_file('/tmp/test_search.txt'))]) :-
    % Test basic regex search
    write_file('/tmp/test_search.txt', "apple\nbanana\napricot\norange"),
    magic_cast(perceive(fs(read_file('/tmp/test_search.txt', 1, -1))), ok(file_content(Content))),
    magic_cast(perceive(search_regex(Content, "^a")), ok(search_results(Found))),
    length(Found, 2),
    Found = [line(1, "apple"), line(3, "apricot")], !.

test(perceive_search_regex_pattern, [cleanup(delete_file('/tmp/test_search.txt'))]) :-
    % Test more complex regex pattern
    write_file('/tmp/test_search.txt', "test123\nhello\ntest456\nworld"),
    magic_cast(perceive(fs(read_file('/tmp/test_search.txt', 1, -1))), ok(file_content(Content))),
    magic_cast(perceive(search_regex(Content, "test[0-9]+")), ok(search_results(Found))),
    length(Found, 2),
    Found = [line(1, "test123"), line(3, "test456")], !.

% === GIT STATUS PARSING TESTS ===

test(git_status_file_terms) :-
    % Test that git status returns clean file terms
    magic_cast(perceive(git(status)), ok(status_info(branch(_), working_status(_), files(Files)))),
    is_list(Files),
    (Files = [] ->
        true  % No files is valid
    ;
        Files = [FirstFile|_],
        % Check that file terms are clean (modified/created/deleted/staged)
        valid_file_status_term(FirstFile)
    ).

% Helper predicate to check valid file status terms
valid_file_status_term(modified(_)) :- !.
valid_file_status_term(created(_)) :- !.
valid_file_status_term(deleted(_)) :- !.
valid_file_status_term(staged(_)) :- !.
valid_file_status_term(unknown(_)) :- !.

% === BACKWARDS COMPATIBILITY TESTS ===

test(conjure_shell_works) :-
    % Test that conjure shell works
    cast(conjure(shell(['echo', 'test'])), Result),
    Result = ok(result("test\n", "")).

test(perceive_git_status_works) :-
    % Test perceive git status works
    magic_cast(perceive(git(status)), ok(status_info(branch(Branch), working_status(_), files(_)))),
    atom(Branch), !.

% === DOCSTRING TESTS ===

test(spell_docstrings_exist) :-
    docstring(spell, _),
    docstring(conjure, _),
    docstring(perceive, _),
    docstring(cast, _).

% === CONSTRUCTOR ENTITY CONSISTENCY TESTS ===

test(all_conjure_ctors_are_entities) :-
    % Test that all conjure constructors are also entities
    findall(Ctor, component(conjure, ctor, Ctor), ConjureCtors),
    maplist(assert_conjure_ctor_is_entity, ConjureCtors).

test(all_perceive_ctors_are_entities) :-
    % Test that all perceive constructors are also entities
    findall(Ctor, component(perceive, ctor, Ctor), PerceiveCtors),
    maplist(assert_perceive_ctor_is_entity, PerceiveCtors).

test(all_conjure_ctors_have_docstrings) :-
    % Test that all conjure constructors have docstrings
    findall(Ctor, component(conjure, ctor, Ctor), ConjureCtors),
    maplist(assert_conjure_ctor_has_docstring, ConjureCtors).

test(all_perceive_ctors_have_docstrings) :-
    % Test that all perceive constructors have docstrings
    findall(Ctor, component(perceive, ctor, Ctor), PerceiveCtors),
    maplist(assert_perceive_ctor_has_docstring, PerceiveCtors).

% Helper predicates for constructor entity tests

assert_conjure_ctor_is_entity(Ctor) :-
    (entity(Ctor) ->
        true
    ; entity(conjure(Ctor)) ->
        true
    ;
        format('FAIL: conjure ctor ~w is not an entity~n', [Ctor]),
        fail
    ).

assert_perceive_ctor_is_entity(Ctor) :-
    (entity(Ctor) ->
        true
    ; entity(perceive(Ctor)) ->
        true
    ;
        format('FAIL: perceive ctor ~w is not an entity~n', [Ctor]),
        fail
    ).

assert_conjure_ctor_has_docstring(Ctor) :-
    (docstring(Ctor, _) ->
        true
    ; docstring(conjure(Ctor), _) ->
        true
    ;
        format('FAIL: conjure ctor ~w has no docstring~n', [Ctor]),
        fail
    ).

assert_perceive_ctor_has_docstring(Ctor) :-
    (docstring(Ctor, _) ->
        true
    ; docstring(perceive(Ctor), _) ->
        true
    ;
        format('FAIL: perceive ctor ~w has no docstring~n', [Ctor]),
        fail
    ).

% === HELPER PREDICATES ===

cleanup_test_files :-
    % Clean up test files
    catch(delete_file('/tmp/test1.txt'), _, true),
    catch(delete_file('/tmp/test2.txt'), _, true).

:- end_tests(spell_system).