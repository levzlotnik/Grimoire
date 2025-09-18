% Auxiliary predicates for C++ bridge integration
% Provides high-level Prolog interface to C++ objects and operations

:- use_foreign_library(foreign(prolog_cpp_binding)).

%% Object Management Predicates

%! cpp_create(+Type, +Args, -Handle) is det.
%
%  Create a C++ object of the specified type with given arguments.
%  @param Type The C++ object type (point, dynamic_array, rectangle, circle, graph_node)
%  @param Args Arguments for object construction as a compound term
%  @param Handle Unique handle for the created object
%
%  @example
%    ?- cpp_create(point, args(3.0, 4.0), Handle).
%    Handle = point_1.
%
cpp_create(Type, Args, Handle) :-
    cpp_create_object(Type, Args, Handle).

%! cpp_destroy(+Handle) is semidet.
%
%  Destroy a C++ object and free its memory.
%  @param Handle Object handle to destroy
%
cpp_destroy(Handle) :-
    cpp_destroy_object(Handle).

%! cpp_exists(+Handle) is semidet.
%
%  Check if a C++ object exists.
%  @param Handle Object handle to check
%
cpp_exists(Handle) :-
    cpp_object_exists(Handle).

%! cpp_type(+Handle, -Type) is semidet.
%
%  Get the type of a C++ object.
%  @param Handle Object handle
%  @param Type Object type
%
cpp_type(Handle, Type) :-
    cpp_object_type(Handle, Type).

%! cpp_string(+Handle, -String) is semidet.
%
%  Get string representation of a C++ object.
%  @param Handle Object handle
%  @param String String representation
%
cpp_string(Handle, String) :-
    cpp_object_to_string(Handle, String).

%! cpp_list_all(-Handles) is det.
%
%  Get list of all registered object handles.
%  @param Handles List of all object handles
%
cpp_list_all(Handles) :-
    cpp_list_objects(Handles).

%! cpp_list_by_type(+Type, -Handles) is det.
%
%  Get list of object handles by type.
%  @param Type Object type to filter by
%  @param Handles List of matching object handles
%
cpp_list_by_type(Type, Handles) :-
    cpp_list_objects_by_type(Type, Handles).

%! cpp_stats(-Stats) is det.
%
%  Get object registry statistics.
%  @param Stats Dictionary with registry statistics
%
cpp_stats(Stats) :-
    cpp_object_stats(Stats).

%! cpp_gc(-Removed) is det.
%
%  Run garbage collection on object registry.
%  @param Removed Number of objects removed
%
cpp_gc(Removed) :-
    cpp_garbage_collect(Removed).

%% Point Operations

%! point_new(+X, +Y, -Handle) is det.
%
%  Create a new point object.
%  @param X X coordinate
%  @param Y Y coordinate  
%  @param Handle Point handle
%
point_new(X, Y, Handle) :-
    cpp_create(point, args(X, Y), Handle).

%! point_coordinates(+Handle, -X, -Y) is semidet.
%
%  Get point coordinates.
%  @param Handle Point handle
%  @param X X coordinate
%  @param Y Y coordinate
%
point_coordinates(Handle, X, Y) :-
    point_get_x(Handle, X),
    point_get_y(Handle, Y).

%! point_move(+Handle, +X, +Y) is semidet.
%
%  Move point to new coordinates.
%  @param Handle Point handle
%  @param X New X coordinate
%  @param Y New Y coordinate
%
point_move(Handle, X, Y) :-
    point_set_x(Handle, X),
    point_set_y(Handle, Y).

%! point_distance(+Handle1, +Handle2, -Distance) is semidet.
%
%  Calculate distance between two points.
%  @param Handle1 First point handle
%  @param Handle2 Second point handle
%  @param Distance Distance between points
%
point_distance(Handle1, Handle2, Distance) :-
    point_distance_to(Handle1, Handle2, Distance).

%! point_add(+Handle1, +Handle2, -ResultHandle) is semidet.
%
%  Add two points together, creating a new point.
%  @param Handle1 First point handle
%  @param Handle2 Second point handle
%  @param ResultHandle Handle of result point
%
point_add(Handle1, Handle2, ResultHandle) :-
    point_add(Handle1, Handle2, ResultHandle).

%% Dynamic Array Operations

%! array_new(+Size, -Handle) is det.
%
%  Create a new dynamic array.
%  @param Size Initial size
%  @param Handle Array handle
%
array_new(Size, Handle) :-
    cpp_create(dynamic_array, args(Size), Handle).

%! array_length(+Handle, -Length) is semidet.
%
%  Get array length.
%  @param Handle Array handle
%  @param Length Array length
%
array_length(Handle, Length) :-
    array_size(Handle, Length).

%! array_append(+Handle, +Value) is semidet.
%
%  Append value to array.
%  @param Handle Array handle
%  @param Value Value to append
%
array_append(Handle, Value) :-
    array_push_back(Handle, Value).

%! array_get(+Handle, +Index, -Value) is semidet.
%
%  Get value at index.
%  @param Handle Array handle
%  @param Index Zero-based index
%  @param Value Value at index
%
array_get(Handle, Index, Value) :-
    array_at(Handle, Index, Value).

%! array_set(+Handle, +Index, +Value) is semidet.
%
%  Set value at index.
%  @param Handle Array handle
%  @param Index Zero-based index
%  @param Value New value
%
array_set(Handle, Index, Value) :-
    array_set_at(Handle, Index, Value).

%! array_statistics(+Handle, -Stats) is semidet.
%
%  Get array statistics.
%  @param Handle Array handle
%  @param Stats Dictionary with sum, mean, etc.
%
array_statistics(Handle, Stats) :-
    array_sum(Handle, Sum),
    array_mean(Handle, Mean),
    array_size(Handle, Size),
    Stats = stats{sum: Sum, mean: Mean, size: Size}.

%! array_from_list(+Handle, +List) is semidet.
%
%  Populate array from Prolog list.
%  @param Handle Array handle
%  @param List Prolog list of numbers
%
array_from_list(Handle, List) :-
    array_from_list(Handle, List).

%! array_to_list(+Handle, -List) is semidet.
%
%  Convert array to Prolog list.
%  @param Handle Array handle
%  @param List Prolog list of numbers
%
array_to_list(Handle, List) :-
    array_to_list(Handle, List).

%% Shape Operations

%! rectangle_new(+Name, +Width, +Height, -Handle) is det.
%
%  Create a new rectangle.
%  @param Name Rectangle name
%  @param Width Rectangle width
%  @param Height Rectangle height
%  @param Handle Rectangle handle
%
rectangle_new(Name, Width, Height, Handle) :-
    cpp_create(rectangle, args(Name, Width, Height), Handle).

%! rectangle_dimensions(+Handle, -Width, -Height) is semidet.
%
%  Get rectangle dimensions.
%  @param Handle Rectangle handle
%  @param Width Rectangle width
%  @param Height Rectangle height
%
rectangle_dimensions(Handle, Width, Height) :-
    rectangle_get_width(Handle, Width),
    rectangle_get_height(Handle, Height).

%! rectangle_resize(+Handle, +Width, +Height) is semidet.
%
%  Resize rectangle.
%  @param Handle Rectangle handle
%  @param Width New width
%  @param Height New height
%
rectangle_resize(Handle, Width, Height) :-
    rectangle_set_width(Handle, Width),
    rectangle_set_height(Handle, Height).

%! rectangle_properties(+Handle, -Properties) is semidet.
%
%  Get rectangle properties.
%  @param Handle Rectangle handle
%  @param Properties Dictionary with area, perimeter, etc.
%
rectangle_properties(Handle, Properties) :-
    rectangle_area(Handle, Area),
    rectangle_perimeter(Handle, Perimeter),
    rectangle_diagonal(Handle, Diagonal),
    (rectangle_is_square(Handle) -> IsSquare = true ; IsSquare = false),
    Properties = properties{
        area: Area,
        perimeter: Perimeter,
        diagonal: Diagonal,
        is_square: IsSquare
    }.

%! circle_new(+Name, +Radius, -Handle) is det.
%
%  Create a new circle.
%  @param Name Circle name
%  @param Radius Circle radius
%  @param Handle Circle handle
%
circle_new(Name, Radius, Handle) :-
    cpp_create(circle, args(Name, Radius), Handle).

%! circle_properties(+Handle, -Properties) is semidet.
%
%  Get circle properties.
%  @param Handle Circle handle
%  @param Properties Dictionary with area, circumference, etc.
%
circle_properties(Handle, Properties) :-
    circle_get_radius(Handle, Radius),
    circle_area(Handle, Area),
    circle_perimeter(Handle, Circumference),
    circle_diameter(Handle, Diameter),
    Properties = properties{
        radius: Radius,
        area: Area,
        circumference: Circumference,
        diameter: Diameter
    }.

%% Graph Node Operations

%! graph_node_new(+Id, -Handle) is det.
%
%  Create a new graph node.
%  @param Id Node identifier
%  @param Handle Node handle
%
graph_node_new(Id, Handle) :-
    cpp_create(graph_node, args(Id), Handle).

%! graph_node_property(+Handle, +Key, +Value) is semidet.
%! graph_node_property(+Handle, +Key, -Value) is semidet.
%
%  Set or get node property.
%  @param Handle Node handle
%  @param Key Property key
%  @param Value Property value
%
graph_node_property(Handle, Key, Value) :-
    var(Value), !,
    graph_node_get_property(Handle, Key, Value).
graph_node_property(Handle, Key, Value) :-
    graph_node_set_property(Handle, Key, Value).

%! graph_node_properties(+Handle, -Properties) is semidet.
%
%  Get all node properties.
%  @param Handle Node handle
%  @param Properties Dictionary of all properties
%
graph_node_properties(Handle, Properties) :-
    graph_node_get_all_properties(Handle, Properties).

%! graph_connect(+Handle1, +Handle2) is semidet.
%
%  Connect two graph nodes.
%  @param Handle1 First node handle
%  @param Handle2 Second node handle
%
graph_connect(Handle1, Handle2) :-
    graph_node_add_connection(Handle1, Handle2).

%! graph_disconnect(+Handle, +TargetId) is semidet.
%
%  Disconnect node from target.
%  @param Handle Node handle
%  @param TargetId Target node ID to disconnect from
%
graph_disconnect(Handle, TargetId) :-
    graph_node_remove_connection(Handle, TargetId).

%! graph_connections(+Handle, -Connections) is semidet.
%
%  Get node connections.
%  @param Handle Node handle
%  @param Connections List of connected node IDs
%
graph_connections(Handle, Connections) :-
    graph_node_get_connections(Handle, Connections).

%! graph_connected(+Handle, +TargetId) is semidet.
%
%  Check if node is connected to target.
%  @param Handle Node handle
%  @param TargetId Target node ID
%
graph_connected(Handle, TargetId) :-
    graph_node_is_connected_to(Handle, TargetId).

%% Advanced Operations

%! cpp_with_object(+Handle, :Goal) is semidet.
%
%  Execute goal with object, ensuring proper cleanup.
%  @param Handle Object handle
%  @param Goal Goal to execute
%
cpp_with_object(Handle, Goal) :-
    cpp_exists(Handle),
    call(Goal),
    !.
cpp_with_object(Handle, _Goal) :-
    cpp_exists(Handle), !,
    fail.
cpp_with_object(_Handle, _Goal) :-
    throw(error(existence_error(cpp_object, Handle), context(cpp_with_object/2, 'Object does not exist'))).

%! cpp_copy_object(+Handle, -CopyHandle) is semidet.
%
%  Create a copy of an object (if supported by the type).
%  @param Handle Source object handle
%  @param CopyHandle Handle of copied object
%
cpp_copy_object(Handle, CopyHandle) :-
    cpp_type(Handle, Type),
    cpp_copy_object_by_type(Type, Handle, CopyHandle).

cpp_copy_object_by_type(point, Handle, CopyHandle) :-
    point_get_x(Handle, X),
    point_get_y(Handle, Y),
    point_new(X, Y, CopyHandle).

cpp_copy_object_by_type(dynamic_array, Handle, CopyHandle) :-
    array_to_list(Handle, List),
    array_new(0, CopyHandle),
    array_from_list(CopyHandle, List).

cpp_copy_object_by_type(Type, _Handle, _CopyHandle) :-
    throw(error(type_error(copyable_object, Type), context(cpp_copy_object/2, 'Object type not copyable'))).

%! cpp_benchmark(+Operation, +Iterations, -Duration, -Unit) is det.
%
%  Run performance benchmark.
%  @param Operation Operation to benchmark
%  @param Iterations Number of iterations
%  @param Duration Time taken
%  @param Unit Time unit
%
cpp_benchmark(Operation, Iterations, Duration, Unit) :-
    cpp_benchmark_operations(Operation, Iterations, Duration, Unit).

%! cpp_stress_test(+Type, +Count, -Handles) is det.
%
%  Run stress test creating many objects.
%  @param Type Object type to create
%  @param Count Number of objects to create
%  @param Handles List of created handles (automatically cleaned up)
%
cpp_stress_test(Type, Count, Handles) :-
    cpp_stress_test_objects(Type, Count, Handles).

%! cpp_system_info(-Info) is det.
%
%  Get system information about the C++ bridge.
%  @param Info System information dictionary
%
cpp_system_info(Info) :-
    cpp_system_info(Info).

%! cpp_memory_info(+Handle, -Info) is semidet.
%
%  Get memory information for an object.
%  @param Handle Object handle
%  @param Info Memory information dictionary
%
cpp_memory_info(Handle, Info) :-
    cpp_object_memory_info(Handle, Info).

%% Exception Testing

%! cpp_test_exceptions(+ErrorType) is semidet.
%
%  Test exception handling mechanism.
%  @param ErrorType Type of exception to test
%
cpp_test_exceptions(ErrorType) :-
    cpp_test_exception_handling(ErrorType).

%% Utility Predicates

%! cpp_cleanup_all is det.
%
%  Clean up all registered objects.
%
cpp_cleanup_all :-
    cpp_list_all(Handles),
    maplist(cpp_destroy, Handles).

%! cpp_object_summary(+Handle, -Summary) is semidet.
%
%  Get comprehensive object summary.
%  @param Handle Object handle
%  @param Summary Summary dictionary
%
cpp_object_summary(Handle, Summary) :-
    cpp_type(Handle, Type),
    cpp_string(Handle, String),
    cpp_memory_info(Handle, MemInfo),
    Summary = summary{
        handle: Handle,
        type: Type,
        string_repr: String,
        memory_info: MemInfo
    }.

%% High-level Mathematical Operations

%! point_operations_demo(-Results) is det.
%
%  Demonstrate various point operations.
%  @param Results List of operation results
%
point_operations_demo(Results) :-
    point_new(0, 0, Origin),
    point_new(3, 4, P1),
    point_new(1, 1, P2),
    point_distance(Origin, P1, Dist1),
    point_distance(P1, P2, Dist2),
    point_add(P1, P2, Sum),
    point_coordinates(Sum, SumX, SumY),
    Results = [
        distance_origin_to_p1: Dist1,
        distance_p1_to_p2: Dist2,
        sum_coordinates: (SumX, SumY)
    ],
    maplist(cpp_destroy, [Origin, P1, P2, Sum]).

%! array_operations_demo(-Results) is det.
%
%  Demonstrate various array operations.
%  @param Results Operation results
%
array_operations_demo(Results) :-
    array_new(0, Arr),
    numlist(1, 10, Numbers),
    maplist(array_append(Arr), Numbers),
    array_statistics(Arr, Stats),
    array_sort(Arr),
    array_to_list(Arr, SortedList),
    Results = [
        original_numbers: Numbers,
        statistics: Stats,
        sorted_list: SortedList
    ],
    cpp_destroy(Arr).

%! shape_comparison_demo(-Results) is det.
%
%  Compare different shapes.
%  @param Results Comparison results
%
shape_comparison_demo(Results) :-
    rectangle_new("rect1", 5, 3, Rect),
    circle_new("circle1", 2.82, Circle), % approximately same area
    rectangle_properties(Rect, RectProps),
    circle_properties(Circle, CircleProps),
    Results = [
        rectangle: RectProps,
        circle: CircleProps
    ],
    maplist(cpp_destroy, [Rect, Circle]).

%! graph_demo(-Results) is det.
%
%  Demonstrate graph operations.
%  @param Results Graph demo results
%
graph_demo(Results) :-
    graph_node_new("node1", N1),
    graph_node_new("node2", N2),
    graph_node_new("node3", N3),
    graph_node_property(N1, "type", "source"),
    graph_node_property(N2, "type", "processor"),
    graph_node_property(N3, "type", "sink"),
    graph_connect(N1, N2),
    graph_connect(N2, N3),
    graph_connections(N1, N1Connections),
    graph_connections(N2, N2Connections),
    graph_connections(N3, N3Connections),
    Results = [
        node1_connections: N1Connections,
        node2_connections: N2Connections,
        node3_connections: N3Connections
    ],
    maplist(cpp_destroy, [N1, N2, N3]).