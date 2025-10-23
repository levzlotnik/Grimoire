:- use_module(library(plunit)).

%% ============================================================================
%% DELEGATION TESTING HOOKS
%% ============================================================================

:- dynamic hit/2.

% Hook for session import delegation
cast_post_hook(conjure(session(import(A))), Result, assertz(hit(conjure(session(import(A))), Result))).

% Hook for session create delegation
cast_post_hook(conjure(session(create(id(SessionId)))), Result, assertz(hit(conjure(session(create(id(SessionId)))), Result))).

% Hook for session export delegation
cast_post_hook(conjure(session(export(id(SessionId), destination(Dest)))), Result, assertz(hit(conjure(session(export(id(SessionId), destination(Dest)))), Result))).

% Hook for prove_it delegation
cast_post_hook(perceive(prove_it(component(E, T, V))), Result, assertz(hit(perceive(prove_it(component(E, T, V))), Result))).

% Hook for sauce_me delegation
cast_post_hook(perceive(sauce_me(spell(Ctor))), Result, assertz(hit(perceive(sauce_me(spell(Ctor))), Result))).

:- begin_tests(interface_spells).

% === PERCEIVE SPELLS ===

test(component_types_spell) :-
    user:magic_cast(perceive(interface(component_types(entity(system)))), Result),
    Result = ok(types(Types)),
    is_list(Types), !.

test(components_spell_unique) :-
    % Test with a component that should have exactly one value
    user:magic_cast(perceive(interface(components(entity(system), type(self)))), Result),
    Result = ok(unique(semantic(_))), !.

test(components_spell_set) :-
    % Test with a component that has multiple values
    user:magic_cast(perceive(interface(components(entity(system), type(subsystem)))), Result),
    Result = ok(set(Values)),
    is_list(Values),
    length(Values, Len),
    Len > 1, !.

test(components_spell_empty) :-
    % Test with a component that doesn't exist - should error
    catch(
        user:magic_cast(perceive(interface(components(entity(nonexistent_entity_xyz), type(foo)))), _),
        error(existence_error(component, _), _),
        true
    ), !.

test(docstring_spell) :-
    user:magic_cast(perceive(interface(docstring(entity(system)))), Result),
    Result = ok(doc(Doc)),
    (atom(Doc) ; string(Doc)), !.

test(entities_spell) :-
    user:magic_cast(perceive(interface(entities)), Result),
    Result = ok(entities(Entities)),
    is_list(Entities),
    member(system, Entities), !.

test(prove_it_spell, [cleanup(retractall(hit(perceive(prove_it(component(_, _, _))), _)))]) :-
    % Test prove_it delegation with subsystem (self is asserted, not provable)
    user:magic_cast(perceive(interface(prove_it(entity(system), type(subsystem), value(git)))), ExternalResult),
    % Verify internal spell called with correct transformed args
    hit(perceive(prove_it(component(Entity, Type, HitValue))), InternalResult),
    assertion(Entity == system),
    assertion(Type == subsystem),
    assertion(HitValue == git),
    % Verify result passed through
    assertion(ExternalResult == InternalResult), !.

test(sauce_me_spell, [cleanup(retractall(hit(perceive(sauce_me(spell(_))), _)))]) :-
    % Test sauce_me delegation with a known spell
    user:magic_cast(perceive(interface(sauce_me(spell_ctor(interface(component_types))))), ExternalResult),
    % Verify internal spell called with correct transformed args
    hit(perceive(sauce_me(spell(SpellCtor))), InternalResult),
    assertion(SpellCtor == interface(component_types)),
    % Verify result passed through
    assertion(ExternalResult == InternalResult), !.

test(system_instructions_spell) :-
    user:magic_cast(perceive(interface(system_instructions)), Result),
    Result = ok(instructions(Instructions)),
    (atom(Instructions) ; string(Instructions)), !.

% === CONJURE SPELLS ===

% Session tests - these delegate to session domain
test(session_create_spell, [
    setup((
        get_time(T),
        format(atom(SessionId), 'test_session_~w', [T]),
        nb_setval(test_session_id, SessionId)
    )),
    cleanup((
        nb_getval(test_session_id, SessionId),
        retractall(hit(conjure(session(create(id(_)))), _)),
        expand_file_name('~/.grimoire', [HomeGrimoire]),
        format(atom(SessionPath), '~w/session-~w', [HomeGrimoire, SessionId]),
        (exists_directory(SessionPath) -> delete_directory_and_contents(SessionPath) ; true)
    ))
]) :-
    nb_getval(test_session_id, SessionId),
    user:magic_cast(conjure(interface(session_create(session_id(SessionId)))), ExternalResult),
    % Verify internal spell called with correct transformed args
    hit(conjure(session(create(id(HitSessionId)))), InternalResult),
    assertion(HitSessionId == SessionId),
    % Verify result passed through
    assertion(ExternalResult == InternalResult), !.

test(session_export_import_delegation, [
    setup((
        get_time(T),
        % Use integer timestamp to avoid dots in session ID
        TInt is floor(T * 1000000),
        format(atom(SessionId), 'test_exp_imp_~w', [TInt]),
        nb_setval(test_session_id, SessionId),
        % Export creates the archive name automatically as /tmp/session-{ID}.tar.gz
        format(atom(ArchivePath), '/tmp/session-~w.tar.gz', [SessionId]),
        nb_setval(test_archive_path, ArchivePath)
    )),
    cleanup((
        nb_getval(test_session_id, SessionId),
        nb_getval(test_archive_path, ArchivePath),
        retractall(hit(conjure(session(export(_, _))), _)),
        retractall(hit(conjure(session(import(_))), _)),
        expand_file_name('~/.grimoire', [HomeGrimoire]),
        format(atom(SessionPath), '~w/session-~w', [HomeGrimoire, SessionId]),
        (exists_directory(SessionPath) -> delete_directory_and_contents(SessionPath) ; true),
        (exists_file(ArchivePath) -> delete_file(ArchivePath) ; true)
    ))
]) :-
    nb_getval(test_session_id, SessionId),
    nb_getval(test_archive_path, ArchivePath),

    % Create a real session
    user:magic_cast(conjure(interface(session_create(session_id(SessionId)))), _),

    % Export it and verify delegation (destination is directory, not full path)
    user:magic_cast(conjure(interface(session_export(session_id(SessionId), destination('/tmp')))), ExportExternalResult),
    hit(conjure(session(export(id(ExportSessionId), destination(ExportDest)))), ExportInternalResult),
    assertion(ExportSessionId == SessionId),
    assertion(ExportDest == '/tmp'),
    assertion(ExportExternalResult == ExportInternalResult),

    % Import the created archive and verify delegation
    user:magic_cast(conjure(interface(session_import(archive(ArchivePath)))), ImportExternalResult),
    hit(conjure(session(import(archive(ImportPath)))), ImportInternalResult),
    assertion(ImportPath == ArchivePath),
    assertion(ImportExternalResult == ImportInternalResult), !.

% === PYTHON_MAGIC_CAST TESTS ===

test(python_magic_cast_component_types) :-
    user:python_magic_cast('perceive(interface(component_types))', _{'Entity': system}, PyResult),
    PyResult.type = "term_struct",
    PyResult.functor = ok,
    PyResult.args = [TypesResult],
    TypesResult.functor = types, !.

test(python_magic_cast_entities) :-
    user:python_magic_cast('perceive(interface(entities))', _{}, PyResult),
    PyResult.type = "term_struct",
    PyResult.functor = ok,
    PyResult.args = [EntitiesResult],
    EntitiesResult.functor = entities,
    EntitiesResult.args = [ListResult],
    ListResult.type = "list", !.

test(python_magic_cast_components_empty) :-
    % Should return error when component doesn't exist
    user:python_magic_cast('perceive(interface(components))', _{'Entity': nonexistent_xyz, 'Type': foo}, PyResult),
    PyResult.functor = error, !.

test(python_magic_cast_docstring) :-
    user:python_magic_cast('perceive(interface(docstring))', _{'Entity': system}, PyResult),
    PyResult.functor = ok,
    PyResult.args = [DocResult],
    DocResult.functor = doc, !.

test(python_magic_cast_error_missing_arg) :-
    % Test error handling when template arg is missing
    user:python_magic_cast('perceive(interface(component_types))', _{}, PyResult),
    PyResult.functor = error, !.

test(python_magic_cast_term_conversion_atom) :-
    % Test that atoms are properly converted
    user:python_magic_cast('perceive(interface(entities))', _{}, PyResult),
    PyResult.args = [EntitiesResult],
    EntitiesResult.args = [ListResult],
    ListResult.elements = [FirstEntity|_],
    FirstEntity.type = "atom", !.

test(python_magic_cast_term_conversion_empty_list) :-
    % Test that empty lists are properly converted - use component_types which can return []
    user:python_magic_cast('perceive(interface(component_types))', _{'Entity': xyz}, PyResult),
    PyResult.functor = ok,
    PyResult.args = [TypesResult],
    TypesResult.functor = types,
    TypesResult.args = [EmptyList],
    EmptyList.type = "list",
    EmptyList.elements = [], !.

:- end_tests(interface_spells).
