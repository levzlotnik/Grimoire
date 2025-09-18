% PLUnit tests for Prolog-C++ binding domain
% Comprehensive test suite covering all C++ objects and operations

:- begin_tests(cpp_bridge_domain).
:- use_module('semantics.pl').
:- use_module('auxiliary.pl').

%% Basic Object Management Tests

test(cpp_bridge_system_exists) :-
    entity(cpp_bridge_system).

test(object_creation_basic) :-
    point_new(3.0, 4.0, Handle),
    cpp_exists(Handle),
    cpp_type(Handle, point),
    cpp_destroy(Handle).

test(object_destruction) :-
    point_new(1.0, 2.0, Handle),
    cpp_exists(Handle),
    cpp_destroy(Handle),
    \+ cpp_exists(Handle).

test(object_type_identification) :-
    point_new(0, 0, PHandle),
    array_new(5, AHandle),
    rectangle_new("test", 10, 5, RHandle),
    circle_new("test_circle", 3, CHandle),
    graph_node_new("node1", GHandle),
    
    cpp_type(PHandle, point),
    cpp_type(AHandle, dynamic_array),
    cpp_type(RHandle, rectangle),
    cpp_type(CHandle, circle),
    cpp_type(GHandle, graph_node),
    
    maplist(cpp_destroy, [PHandle, AHandle, RHandle, CHandle, GHandle]).

test(object_string_representation) :-
    point_new(3, 4, Handle),
    cpp_string(Handle, String),
    atom_string(String, StringAtom),
    sub_atom(StringAtom, _, _, _, 'point'),
    cpp_destroy(Handle).

test(object_listing) :-
    point_new(1, 1, P1),
    point_new(2, 2, P2),
    array_new(0, A1),
    
    cpp_list_all(AllHandles),
    length(AllHandles, Len),
    Len >= 3,
    
    cpp_list_by_type(point, PointHandles),
    length(PointHandles, 2),
    
    cpp_list_by_type(dynamic_array, ArrayHandles),
    length(ArrayHandles, 1),
    
    maplist(cpp_destroy, [P1, P2, A1]).

test(object_statistics) :-
    point_new(1, 1, P1),
    array_new(5, A1),
    
    cpp_stats(Stats),
    Stats.total >= 2,
    
    maplist(cpp_destroy, [P1, A1]).

test(garbage_collection) :-
    cpp_gc(Removed),
    integer(Removed),
    Removed >= 0.

%% Point Operations Tests

test(point_creation_and_access) :-
    point_new(3.5, 4.2, Handle),
    point_coordinates(Handle, X, Y),
    abs(X - 3.5) < 0.001,
    abs(Y - 4.2) < 0.001,
    cpp_destroy(Handle).

test(point_coordinate_modification) :-
    point_new(0, 0, Handle),
    point_move(Handle, 10, 20),
    point_coordinates(Handle, X, Y),
    X =:= 10,
    Y =:= 20,
    cpp_destroy(Handle).

test(point_distance_calculation) :-
    point_new(0, 0, Origin),
    point_new(3, 4, P1),
    point_distance(Origin, P1, Distance),
    abs(Distance - 5.0) < 0.001,  % 3-4-5 triangle
    maplist(cpp_destroy, [Origin, P1]).

test(point_addition) :-
    point_new(1, 2, P1),
    point_new(3, 4, P2),
    point_add(P1, P2, Sum),
    point_coordinates(Sum, X, Y),
    X =:= 4,
    Y =:= 6,
    maplist(cpp_destroy, [P1, P2, Sum]).

test(point_copy) :-
    point_new(5, 7, Original),
    cpp_copy_object(Original, Copy),
    point_coordinates(Original, X1, Y1),
    point_coordinates(Copy, X2, Y2),
    X1 =:= X2,
    Y1 =:= Y2,
    maplist(cpp_destroy, [Original, Copy]).

%% Dynamic Array Tests

test(array_creation_and_size) :-
    array_new(10, Handle),
    array_length(Handle, Length),
    Length =:= 10,
    cpp_destroy(Handle).

test(array_element_access) :-
    array_new(5, Handle),
    array_set(Handle, 0, 3.14),
    array_set(Handle, 4, 2.71),
    array_get(Handle, 0, V1),
    array_get(Handle, 4, V2),
    abs(V1 - 3.14) < 0.001,
    abs(V2 - 2.71) < 0.001,
    cpp_destroy(Handle).

test(array_append_operations) :-
    array_new(0, Handle),
    array_append(Handle, 1.0),
    array_append(Handle, 2.0),
    array_append(Handle, 3.0),
    array_length(Handle, Length),
    Length =:= 3,
    array_get(Handle, 1, V),
    V =:= 2.0,
    cpp_destroy(Handle).

test(array_statistics) :-
    array_new(0, Handle),
    maplist(array_append(Handle), [1, 2, 3, 4, 5]),
    array_statistics(Handle, Stats),
    Stats.sum =:= 15,
    Stats.mean =:= 3.0,
    Stats.size =:= 5,
    cpp_destroy(Handle).

test(array_sorting) :-
    array_new(0, Handle),
    maplist(array_append(Handle), [5, 2, 8, 1, 9]),
    array_sort(Handle),
    array_get(Handle, 0, First),
    array_get(Handle, 4, Last),
    First =:= 1,
    Last =:= 9,
    cpp_destroy(Handle).

test(array_list_conversion) :-
    array_new(0, Handle),
    TestList = [1.5, 2.5, 3.5],
    array_from_list(Handle, TestList),
    array_to_list(Handle, ResultList),
    ResultList = TestList,
    cpp_destroy(Handle).

test(array_copy) :-
    array_new(0, Original),
    maplist(array_append(Original), [1, 2, 3]),
    cpp_copy_object(Original, Copy),
    array_to_list(Original, List1),
    array_to_list(Copy, List2),
    List1 = List2,
    maplist(cpp_destroy, [Original, Copy]).

%% Rectangle Tests

test(rectangle_creation_and_properties) :-
    rectangle_new("test_rect", 5, 3, Handle),
    rectangle_dimensions(Handle, W, H),
    W =:= 5,
    H =:= 3,
    rectangle_properties(Handle, Props),
    Props.area =:= 15,
    Props.perimeter =:= 16,
    abs(Props.diagonal - 5.831) < 0.1,
    Props.is_square =:= false,
    cpp_destroy(Handle).

test(rectangle_square_detection) :-
    rectangle_new("square", 4, 4, Handle),
    rectangle_properties(Handle, Props),
    Props.is_square =:= true,
    cpp_destroy(Handle).

test(rectangle_resizing) :-
    rectangle_new("resizable", 2, 3, Handle),
    rectangle_resize(Handle, 6, 8),
    rectangle_dimensions(Handle, W, H),
    W =:= 6,
    H =:= 8,
    cpp_destroy(Handle).

%% Circle Tests

test(circle_creation_and_properties) :-
    rectangle_new("test_circle", 2, Handle),
    circle_properties(Handle, Props),
    abs(Props.radius - 2) < 0.001,
    abs(Props.area - 12.566) < 0.1,  % π * 4
    abs(Props.circumference - 12.566) < 0.1,  % 2π * 2
    Props.diameter =:= 4,
    cpp_destroy(Handle).

test(circle_radius_modification) :-
    circle_new("mod_circle", 1, Handle),
    circle_set_radius(Handle, 3),
    circle_get_radius(Handle, NewRadius),
    NewRadius =:= 3,
    cpp_destroy(Handle).

%% Graph Node Tests

test(graph_node_creation_and_id) :-
    graph_node_new("test_node", Handle),
    graph_node_get_id(Handle, Id),
    Id = "test_node",
    cpp_destroy(Handle).

test(graph_node_properties) :-
    graph_node_new("prop_node", Handle),
    graph_node_set_property(Handle, "type", "test"),
    graph_node_set_property(Handle, "value", "42"),
    graph_node_get_property(Handle, "type", Type),
    graph_node_get_property(Handle, "value", Value),
    Type = "test",
    Value = "42",
    graph_node_has_property(Handle, "type"),
    \+ graph_node_has_property(Handle, "nonexistent"),
    cpp_destroy(Handle).

test(graph_node_property_removal) :-
    graph_node_new("removal_node", Handle),
    graph_node_set_property(Handle, "temp", "value"),
    graph_node_has_property(Handle, "temp"),
    graph_node_remove_property(Handle, "temp"),
    \+ graph_node_has_property(Handle, "temp"),
    cpp_destroy(Handle).

test(graph_node_all_properties) :-
    graph_node_new("all_props_node", Handle),
    graph_node_set_property(Handle, "key1", "value1"),
    graph_node_set_property(Handle, "key2", "value2"),
    graph_node_get_all_properties(Handle, AllProps),
    AllProps.key1 = "value1",
    AllProps.key2 = "value2",
    cpp_destroy(Handle).

test(graph_node_connections) :-
    graph_node_new("node1", N1),
    graph_node_new("node2", N2),
    graph_node_new("node3", N3),
    
    graph_node_add_connection(N1, N2),
    graph_node_add_connection(N1, N3),
    
    graph_node_is_connected_to(N1, "node2"),
    graph_node_is_connected_to(N1, "node3"),
    \+ graph_node_is_connected_to(N2, "node1"),
    
    graph_node_get_connections(N1, Connections),
    length(Connections, 2),
    member("node2", Connections),
    member("node3", Connections),
    
    graph_node_connection_count(N1, Count),
    Count =:= 2,
    
    maplist(cpp_destroy, [N1, N2, N3]).

test(graph_node_connection_removal) :-
    graph_node_new("source", Source),
    graph_node_new("target", Target),
    
    graph_node_add_connection(Source, Target),
    graph_node_is_connected_to(Source, "target"),
    
    graph_node_remove_connection(Source, "target"),
    \+ graph_node_is_connected_to(Source, "target"),
    
    maplist(cpp_destroy, [Source, Target]).

%% Advanced Operations Tests

test(with_object_wrapper) :-
    point_new(1, 1, Handle),
    cpp_with_object(Handle, (
        point_coordinates(Handle, X, Y),
        X =:= 1,
        Y =:= 1
    )),
    cpp_destroy(Handle).

test(with_object_nonexistent_fails) :-
    \+ cpp_with_object("nonexistent_handle", true).

%% Spell System Tests

test(conjure_spells) :-
    cast(conjure(point_object(2, 3)), PHandle),
    cast(conjure(array_object(5)), AHandle),
    cast(conjure(rectangle_object("spell_rect", 4, 6)), RHandle),
    cast(conjure(circle_object("spell_circle", 2.5)), CHandle),
    cast(conjure(graph_node_object("spell_node")), GHandle),
    
    cpp_type(PHandle, point),
    cpp_type(AHandle, dynamic_array),
    cpp_type(RHandle, rectangle),
    cpp_type(CHandle, circle),
    cpp_type(GHandle, graph_node),
    
    maplist(cpp_destroy, [PHandle, AHandle, RHandle, CHandle, GHandle]).

test(object_operation_spells) :-
    point_new(1, 2, Handle),
    cast(conjure(object_operation(copy(Handle))), CopyHandle),
    cpp_type(CopyHandle, point),
    point_coordinates(Handle, X1, Y1),
    point_coordinates(CopyHandle, X2, Y2),
    X1 =:= X2,
    Y1 =:= Y2,
    maplist(cpp_destroy, [Handle, CopyHandle]).

test(mathematical_computation_spells) :-
    point_new(0, 0, Origin),
    point_new(3, 4, P1),
    cast(conjure(mathematical_computation(point_distance(Origin, P1))), Distance),
    abs(Distance - 5.0) < 0.001,
    maplist(cpp_destroy, [Origin, P1]).

test(perceive_spells) :-
    point_new(5, 6, Handle),
    query(perceive(object_info(Handle)), Info),
    Info.handle = Handle,
    Info.type = point,
    cpp_destroy(Handle).

test(system_stats_perception) :-
    query(perceive(system_stats), Stats),
    integer(Stats.total).

test(capability_queries) :-
    query(perceive(capability_query(cpp_bridge_system)), Caps),
    member(object_creation, Caps.system),
    member(memory_management, Caps.system).

%% Performance Tests

test(benchmark_operations) :-
    cast(conjure(performance_test(benchmark(create_destroy_point, 100))), Result),
    Result.operation = create_destroy_point,
    Result.iterations =:= 100,
    integer(Result.duration),
    Result.unit = microseconds.

test(stress_testing) :-
    cast(conjure(stress_test(point, 50)), Handles),
    length(Handles, 50),
    % Objects are automatically cleaned up by stress test.

%% Exception Handling Tests

test(exception_propagation_std) :-
    cast(conjure(exception_test(std_exception)), Result),
    Result = caught_exception(error(cpp_exception, _)).

test(exception_propagation_bridge) :-
    cast(conjure(exception_test(bridge_exception)), Result),
    Result = caught_exception(error(cpp_bridge_error, _)).

test(exception_propagation_unknown) :-
    cast(conjure(exception_test(unknown)), Result),
    Result = caught_exception(error(unknown_cpp_exception, _)).

test(no_exception_case) :-
    cast(conjure(exception_test(none)), Result),
    Result = no_exception.

%% Type Conversion Tests

test(vector_list_conversion) :-
    TestList = [1.0, 2.0, 3.0],
    cpp_vector_to_list(double, TestList, ResultList),
    ResultList = TestList.

%% Memory Management Tests

test(memory_info_retrieval) :-
    array_new(10, Handle),
    cpp_memory_info(Handle, MemInfo),
    MemInfo.type = dynamic_array,
    MemInfo.handle = Handle,
    cpp_destroy(Handle).

test(system_info_retrieval) :-
    cpp_system_info(SysInfo),
    SysInfo.cpp_standard = 'C++20',
    SysInfo.bridge_version = '1.0.0',
    SysInfo.memory_management = 'RAII + shared_ptr'.

%% High-level Domain Operations Tests

test(point_operations_demo) :-
    point_operations_demo(Results),
    is_list(Results),
    member(distance_origin_to_p1:Dist1, Results),
    abs(Dist1 - 5.0) < 0.001.

test(array_operations_demo) :-
    array_operations_demo(Results),
    is_list(Results),
    member(original_numbers:Numbers, Results),
    Numbers = [1,2,3,4,5,6,7,8,9,10].

test(shape_comparison_demo) :-
    shape_comparison_demo(Results),
    is_list(Results),
    member(rectangle:RectProps, Results),
    member(circle:CircleProps, Results),
    RectProps.area =:= 15,
    abs(CircleProps.area - 25.0) < 1.0.

test(graph_demo) :-
    graph_demo(Results),
    is_list(Results),
    member(node1_connections:N1Conn, Results),
    member(node2_connections:N2Conn, Results),
    length(N1Conn, 1),
    length(N2Conn, 1).

test(demonstrate_all_objects) :-
    demonstrate_all_objects(Results),
    Results.points = [_|_],
    Results.arrays = [_|_],
    Results.shapes = [_|_],
    Results.graphs = [_|_].

%% System Health and Lifecycle Tests

test(system_health_check) :-
    system_health_check(Health),
    Health.status = healthy,
    integer(Health.object_stats.total),
    integer(Health.gc_removed).

test(benchmark_suite) :-
    benchmark_suite(Results),
    integer(Results.point_ops.duration),
    integer(Results.array_ops.duration).

test(exception_test_suite) :-
    exception_test_suite(Results),
    Results.no_exception = success,
    Results.std_exception = caught(_),
    Results.bridge_exception = caught(_),
    Results.unknown_exception = caught(_).

%% Temporary Object Management Tests

test(with_temporary_objects) :-
    ObjectSpecs = [point(1, 2), array(5), rectangle("temp", 3, 4)],
    with_temporary_objects(ObjectSpecs, (
        [PHandle, AHandle, RHandle] = Handles,
        cpp_type(PHandle, point),
        cpp_type(AHandle, dynamic_array),
        cpp_type(RHandle, rectangle)
    )),
    % Objects should be automatically cleaned up
    \+ cpp_exists(point_1),  % Assuming this was the first point created
    \+ cpp_exists(dynamic_array_1),
    \+ cpp_exists(rectangle_1).

%% Edge Cases and Error Handling Tests

test(nonexistent_object_operations) :-
    \+ cpp_exists("nonexistent"),
    \+ cpp_type("nonexistent", _),
    \+ cpp_string("nonexistent", _),
    \+ point_get_x("nonexistent", _).

test(invalid_array_access) :-
    array_new(5, Handle),
    \+ array_at(Handle, 10, _),  % Index out of bounds
    \+ array_set_at(Handle, 10, 5.0),
    cpp_destroy(Handle).

test(cleanup_all_objects) :-
    point_new(1, 1, _),  % Create some objects without cleaning up
    array_new(5, _),
    cpp_cleanup_all,
    cpp_list_all(Handles),
    length(Handles, 0).

%% Component System Tests

test(cpp_bridge_system_components) :-
    component(cpp_bridge_system, provides_capability, object_creation),
    component(cpp_bridge_system, provides_capability, memory_management),
    component(cpp_bridge_system, uses_standard, 'C++20'),
    component(cpp_bridge_system, memory_model, 'RAII + shared_ptr').

test(object_type_components) :-
    component(cpp_object_type(point), supports_operation, creation),
    component(cpp_object_type(point), data_type, geometric),
    component(cpp_object_type(point), copyable, true),
    
    component(cpp_object_type(dynamic_array), element_type, double),
    component(cpp_object_type(dynamic_array), data_type, container),
    
    component(cpp_object_type(rectangle), inherits_from, shape),
    component(cpp_object_type(circle), inherits_from, shape).

test(operation_category_components) :-
    component(memory_management, provides_operation, object_creation),
    component(mathematical_operations, provides_operation, distance_calculation),
    component(performance_testing, provides_operation, benchmarking).

test(spell_constructors) :-
    component(conjure, ctor, cpp_object),
    component(conjure, ctor, point_object),
    component(conjure, ctor, performance_test),
    component(perceive, ctor, object_info),
    component(perceive, ctor, system_stats).

%% Documentation Tests

test(domain_docstrings) :-
    docstring(cpp_bridge_domain, Doc),
    sub_atom(Doc, _, _, _, 'PlCxx'),
    sub_atom(Doc, _, _, _, 'RAII').

test(entity_docstrings) :-
    docstring(cpp_bridge_system, SysDoc),
    sub_atom(SysDoc, _, _, _, 'lifecycle'),
    
    docstring(memory_management, MemDoc),
    sub_atom(MemDoc, _, _, _, 'RAII').

:- end_tests(cpp_bridge_domain).