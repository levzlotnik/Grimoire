:- use_module(library(plunit)).

%% ============================================================================
%% DELEGATION TESTING HOOKS
%% ============================================================================

:- dynamic hit/2.
:- multifile cast_post_hook/3.

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

% Hook for session context delegation
cast_post_hook(perceive(session(context)), Result, assertz(hit(perceive(session(context)), Result))).

cleanup_test_session(SessionId) :-
    catch(user:magic_cast(conjure(session(delete(id(SessionId)))), _), _, true),
    (catch(user:grimoire_data_path(DataPath), _, fail)
    -> atomic_list_concat([DataPath, '/sessions/', SessionId], SessionDir),
       catch(user:unload_entity(semantic(folder(SessionDir))), _, true),
       (exists_directory(SessionDir) -> delete_directory_and_contents(SessionDir) ; true)
    ; true).

:- begin_tests(interface_spells).

% === PERCEIVE SPELLS ===

test(component_types_spell) :-
    user:magic_cast(perceive(interface(component_types(entity(system)))), Result),
    Result = ok(types(Types)),
    is_list(Types), !.

test(components_spell_unique) :-
    % Test with a component that should have exactly one value (and no broken)
    user:magic_cast(perceive(interface(components(entity(system), type(self)))), Result),
    Result = ok(components(verified(unique(semantic(_))), broken([]))), !.

test(components_spell_set) :-
    % Test with a component that has multiple values (and no broken)
    user:magic_cast(perceive(interface(components(entity(system), type(subsystem)))), Result),
    Result = ok(components(verified(set(Values)), broken([]))),
    is_list(Values),
    length(Values, Len),
    Len > 1, !.

test(components_spell_empty) :-
    % Test with a component that doesn't exist - should throw type error
    catch(
        user:magic_cast(perceive(interface(components(entity(nonexistent_entity_xyz), type(foo)))), _),
        error(type_error(entity, nonexistent_entity_xyz), _),
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

test(prove_it_spell, [
    setup(retractall(user:hit(perceive(prove_it(component(_, _, _))), _))),
    cleanup(retractall(user:hit(perceive(prove_it(component(_, _, _))), _)))
]) :-
    % Test prove_it delegation with subsystem (self is asserted, not provable)
    user:magic_cast(perceive(interface(prove_it(entity(system), type(subsystem), value(git)))), ExternalResult),
    % Verify internal spell called with correct transformed args
    user:hit(perceive(prove_it(component(Entity, Type, HitValue))), InternalResult),
    assertion(Entity == system),
    assertion(Type == subsystem),
    assertion(HitValue == git),
    % Verify result passed through
    assertion(ExternalResult == InternalResult), !.

test(sauce_me_spell, [
    setup(retractall(user:hit(perceive(sauce_me(spell(_))), _))),
    cleanup(retractall(user:hit(perceive(sauce_me(spell(_))), _)))
]) :-
    % Test sauce_me delegation with a known spell
    user:magic_cast(perceive(interface(sauce_me(spell_ctor(perceive(interface(component_types)))))), ExternalResult),
    % Verify internal spell called with correct transformed args
    user:hit(perceive(sauce_me(spell(SpellCtor))), InternalResult),
    assertion(SpellCtor == perceive(interface(component_types))),
    % Verify result passed through
    assertion(ExternalResult == InternalResult), !.

test(system_instructions_spell) :-
    user:magic_cast(perceive(interface(system_instructions)), Result),
    Result = ok(instructions(Instructions)),
    (atom(Instructions) ; string(Instructions)), !.

% === CONJURE SPELLS ===

% Session tests - these delegate to session domain
test(session_create_spell, [
    cleanup(retractall(user:hit(conjure(session(create(id(_)))), _)))
]) :-
    get_time(T),
    format(atom(SessionId), 'test_session_~w', [T]),

    % Create session via interface
    user:magic_cast(conjure(interface(session_create(session_id(SessionId)))), ExternalResult),

    % Verify internal spell called with correct transformed args
    user:hit(conjure(session(create(id(HitSessionId)))), InternalResult),
    assertion(HitSessionId == SessionId),
    % Verify result passed through
    assertion(ExternalResult == InternalResult),

    % Clean up using session spell
    cleanup_test_session(SessionId), !.

test(session_export_import_delegation, [
    cleanup((
        retractall(user:hit(conjure(session(export(_, _))), _)),
        retractall(user:hit(conjure(session(import(_))), _))
    ))
]) :-
    get_time(T),
    % Use integer timestamp to avoid dots in session ID
    TInt is floor(T * 1000000),
    format(atom(SessionId), 'test_exp_imp_~w', [TInt]),
    % Export creates the archive name automatically as /tmp/session-{ID}.tar.gz
    format(atom(ArchivePath), '/tmp/session-~w.tar.gz', [SessionId]),

    % Create a real session
    user:magic_cast(conjure(interface(session_create(session_id(SessionId)))), CreateResult),
    assertion(CreateResult = ok(_)),

    % Export it and verify delegation (destination is directory, not full path)
    user:magic_cast(conjure(interface(session_export(session_id(SessionId), destination('/tmp')))), ExportExternalResult),
    assertion(ExportExternalResult = ok(_)),
    user:hit(conjure(session(export(id(ExportSessionId), destination(ExportDest)))), ExportInternalResult),
    assertion(ExportSessionId == SessionId),
    assertion(ExportDest == '/tmp'),
    assertion(ExportExternalResult == ExportInternalResult),

    % Delete the session (so we can re-import it)
    user:magic_cast(conjure(session(delete(id(SessionId)))), DeleteResult),
    assertion(DeleteResult = ok(_)),

    % Import the created archive and verify delegation
    user:magic_cast(conjure(interface(session_import(archive(ArchivePath)))), ImportExternalResult),
    assertion(ImportExternalResult = ok(_)),
    user:hit(conjure(session(import(archive(ImportPath)))), ImportInternalResult),
    assertion(ImportPath == ArchivePath),
    assertion(ImportExternalResult == ImportInternalResult),

    % Clean up: delete session and archive
    user:magic_cast(conjure(session(delete(id(SessionId)))), FinalDeleteResult),
    assertion(FinalDeleteResult = ok(_)),
    delete_file(ArchivePath),
    cleanup_test_session(SessionId), !.

test(session_context_delegation, [
    cleanup((
        retractall(user:hit(perceive(session(context)), _))
    ))
]) :-
    get_time(T),
    TInt is floor(T * 1000000),
    format(atom(SessionId), 'test_ctx_~w', [TInt]),

    % Create and switch to session
    user:magic_cast(conjure(interface(session_create(session_id(SessionId)))), CreateResult),
    assertion(CreateResult = ok(_)),
    user:magic_cast(conjure(interface(session_switch(session_id(SessionId)))), SwitchResult),
    assertion(SwitchResult = ok(_)),

    % Get context and verify delegation
    user:magic_cast(perceive(interface(session_context)), ExternalResult),
    user:hit(perceive(session(context)), InternalResult),
    assertion(ExternalResult == InternalResult),
    assertion(ExternalResult = ok(session(SessionId), _, _)),

    % Cleanup
    user:magic_cast(conjure(session(delete(id(SessionId)))), DeleteResult),
    assertion(DeleteResult = ok(_)),
    cleanup_test_session(SessionId), !.

% === PYTHON_MAGIC_CAST TESTS ===

test(python_magic_cast_component_types) :-
    user:python_magic_cast(perceive(interface(component_types(entity(system)))), PyResult),
    assertion(PyResult.type = "compound"),
    assertion(PyResult.functor = ok),
    PyResult.args = [TypesResult],
    assertion(TypesResult.type = "compound"),
    assertion(TypesResult.functor = types).

test(python_magic_cast_entities) :-
    user:python_magic_cast(perceive(interface(entities)), PyResult),
    assertion(PyResult.type = "compound"),
    assertion(PyResult.functor = ok),
    PyResult.args = [EntitiesResult],
    assertion(EntitiesResult.type = "compound"),
    assertion(EntitiesResult.functor = entities),
    EntitiesResult.args = [ListResult],
    assertion(is_list(ListResult)).

test(python_magic_cast_components_empty) :-
    % Should return error when component doesn't exist
    user:python_magic_cast(perceive(interface(components(entity(nonexistent_xyz), type(foo)))), PyResult),
    assertion(PyResult.functor = error).

test(python_magic_cast_docstring) :-
    user:python_magic_cast(perceive(interface(docstring(entity(system)))), PyResult),
    assertion(PyResult.functor = ok),
    PyResult.args = [DocResult],
    assertion(DocResult.functor = doc).

test(python_magic_cast_ungrounded_variable) :-
    % Test that ungrounded variables in spell terms cause proper error
    % This tests the spell implementation's validation, not template filling
    catch(
        user:python_magic_cast(perceive(interface(component_types(entity(_)))), _PyResult),
        Error,
        true
    ),
    assertion(nonvar(Error)).

test(python_magic_cast_term_conversion_atom) :-
    % Test that atoms are properly converted
    user:python_magic_cast(perceive(interface(entities)), PyResult),
    PyResult.args = [EntitiesResult],
    EntitiesResult.args = [ListResult],
    assertion(is_list(ListResult)),
    ListResult = [FirstEntity|_],
    assertion(FirstEntity.type = "atom").

test(python_magic_cast_term_conversion_empty_list) :-
    % Test that empty lists are properly converted - use component_types which can return []
    user:python_magic_cast(perceive(interface(component_types(entity(xyz)))), PyResult),
    assertion(PyResult.functor = ok),
    PyResult.args = [TypesResult],
    assertion(TypesResult.functor = types),
    TypesResult.args = [EmptyList],
    assertion(is_list(EmptyList)),
    assertion(EmptyList = []).

% === CRUD OPERATION TESTS ===

test(interface_add_component_explicit_entity) :-
    % Create test entity file
    TestEntityContent = {|string||
        :- self_entity(interface_test_add).
    |},
    user:magic_cast(conjure(fs(edit_file(file('/tmp/interface_test_add.pl'), edits([append(TestEntityContent)])))), WriteResult),
    assertion(WriteResult = ok(_)),
    load_entity(semantic(file('/tmp/interface_test_add.pl'))),

    % Add component using interface with explicit entity
    user:magic_cast(conjure(interface(add_component(entity(interface_test_add), component_type(test_comp), value(test_val)))), AddResult),
    assertion(AddResult = ok(component_added(component_type(test_comp), value(test_val)))),
    user:please_verify(component(interface_test_add, test_comp, test_val)),

    % Cleanup
    (exists_file('/tmp/interface_test_add.pl') -> delete_file('/tmp/interface_test_add.pl') ; true), !.

test(interface_add_component_focused_entity) :-
    % Create test entity and session
    TestEntityContent = {|string||
        :- self_entity(interface_test_focused).
    |},
    user:magic_cast(conjure(fs(edit_file(file('/tmp/interface_test_focused.pl'), edits([append(TestEntityContent)])))), WriteResult),
    assertion(WriteResult = ok(_)),
    load_entity(semantic(file('/tmp/interface_test_focused.pl'))),

    % Create session and focus on entity
    user:magic_cast(conjure(session(create(id(test_interface_focused)))), CreateResult),
    assertion(CreateResult = ok(_)),
    user:magic_cast(conjure(session(switch(id(test_interface_focused)))), SwitchResult),
    assertion(SwitchResult = ok(_)),
    user:magic_cast(conjure(session(focus_entity(entity(interface_test_focused)))), FocusResult),
    assertion(FocusResult = ok(_)),

    % Add component using interface with focused_entity
    user:magic_cast(conjure(interface(add_component(entity(focused_entity), component_type(focused_comp), value(focused_val)))), AddResult),
    assertion(AddResult = ok(component_added(component_type(focused_comp), value(focused_val)))),
    user:please_verify(component(interface_test_focused, focused_comp, focused_val)),

    % Cleanup
    cleanup_test_session(test_interface_focused),
    (exists_file('/tmp/interface_test_focused.pl') -> delete_file('/tmp/interface_test_focused.pl') ; true), !.

test(interface_add_component_no_focus_error) :-
    % Create session without focusing
    user:magic_cast(conjure(session(delete(id(test_interface_nofocus)))), DeleteResult),
    assertion((
        DeleteResult = ok(_)
    ;   DeleteResult = error(session_error(session_not_found(_)), _)
    )),
    user:magic_cast(conjure(session(create(id(test_interface_nofocus)))), CreateResult),
    assertion(CreateResult = ok(_)),
    user:magic_cast(conjure(session(switch(id(test_interface_nofocus)))), SwitchResult),
    assertion(SwitchResult = ok(_)),

    % Try to add with focused_entity when nothing is focused
    user:magic_cast(conjure(interface(add_component(entity(focused_entity), component_type(test), value(val)))), AddResult),
    assertion(AddResult = error(add_error(no_focused_entity), context(interface(add_component), _))),

    % Cleanup
    cleanup_test_session(test_interface_nofocus), !.

test(interface_remove_component_explicit_entity) :-
    % Create test entity file
    TestEntityContent = {|string||
        :- self_entity(interface_test_remove).
    |},
    user:magic_cast(conjure(fs(edit_file(file('/tmp/interface_test_remove.pl'), edits([append(TestEntityContent)])))), WriteResult),
    assertion(WriteResult = ok(_)),
    load_entity(semantic(file('/tmp/interface_test_remove.pl'))),

    % Add then remove using interface
    user:magic_cast(conjure(interface(add_component(entity(interface_test_remove), component_type(temp_comp), value(temp_val)))), AddResult),
    assertion(AddResult = ok(_)),
    user:magic_cast(conjure(interface(remove_component(entity(interface_test_remove), component_type(temp_comp), value(temp_val)))), RemoveResult),
    assertion(RemoveResult = ok(component_removed(component_type(temp_comp), value(temp_val)))),

    % Cleanup
    (exists_file('/tmp/interface_test_remove.pl') -> delete_file('/tmp/interface_test_remove.pl') ; true), !.

:- end_tests(interface_spells).
