% ECS semantics for Prolog-C++ binding domain
% Models C++ objects and operations as Grimoire entities and components

:- self_entity(cpp_bridge_domain).

% Import auxiliary predicates
:- use_module('auxiliary.pl', [
    cpp_create/3,
    cpp_destroy/1,
    cpp_exists/1,
    cpp_type/2,
    cpp_string/2,
    cpp_list_all/1,
    cpp_list_by_type/2,
    cpp_stats/1,
    cpp_gc/1,
    point_new/3,
    point_coordinates/3,
    point_move/3,
    point_distance/3,
    point_add/3,
    array_new/2,
    array_length/2,
    array_append/2,
    array_get/3,
    array_set/3,
    array_statistics/3,
    array_from_list/2,
    array_to_list/2,
    rectangle_new/4,
    rectangle_dimensions/3,
    rectangle_resize/3,
    rectangle_properties/2,
    circle_new/3,
    circle_properties/2,
    graph_node_new/2,
    graph_node_property/3,
    graph_node_properties/2,
    graph_connect/2,
    graph_disconnect/2,
    graph_connections/2,
    graph_connected/2,
    cpp_with_object/2,
    cpp_copy_object/2,
    cpp_benchmark/4,
    cpp_stress_test/3,
    cpp_system_info/1,
    cpp_memory_info/2,
    cpp_test_exceptions/1,
    cpp_cleanup_all/0,
    cpp_object_summary/2,
    point_operations_demo/1,
    array_operations_demo/1,
    shape_comparison_demo/1,
    graph_demo/1
]).

%% Entity Declarations

% Core bridge entity
entity(cpp_bridge_system).

% Object type entities
entity(cpp_object_type(point)).
entity(cpp_object_type(dynamic_array)).
entity(cpp_object_type(rectangle)).
entity(cpp_object_type(circle)).
entity(cpp_object_type(graph_node)).

% Operation category entities
entity(memory_management).
entity(mathematical_operations).
entity(container_operations).
entity(geometric_operations).
entity(graph_operations).
entity(performance_testing).
entity(exception_handling).

% Bridge capabilities
entity(type_conversion).
entity(object_lifecycle).
entity(raii_management).

%% Component Declarations

% System capabilities
component(cpp_bridge_system, provides_capability, object_creation).
component(cpp_bridge_system, provides_capability, memory_management).
component(cpp_bridge_system, provides_capability, type_safety).
component(cpp_bridge_system, provides_capability, exception_propagation).
component(cpp_bridge_system, provides_capability, container_conversion).
component(cpp_bridge_system, provides_capability, performance_monitoring).

component(cpp_bridge_system, uses_standard, 'C++20').
component(cpp_bridge_system, uses_library, 'SWI-Prolog PlCxx').
component(cpp_bridge_system, memory_model, 'RAII + shared_ptr').
component(cpp_bridge_system, thread_safety, true).

% Object type capabilities
component(cpp_object_type(point), supports_operation, creation).
component(cpp_object_type(point), supports_operation, coordinate_access).
component(cpp_object_type(point), supports_operation, distance_calculation).
component(cpp_object_type(point), supports_operation, arithmetic).
component(cpp_object_type(point), data_type, geometric).
component(cpp_object_type(point), copyable, true).

component(cpp_object_type(dynamic_array), supports_operation, creation).
component(cpp_object_type(dynamic_array), supports_operation, element_access).
component(cpp_object_type(dynamic_array), supports_operation, resizing).
component(cpp_object_type(dynamic_array), supports_operation, mathematical_analysis).
component(cpp_object_type(dynamic_array), supports_operation, sorting).
component(cpp_object_type(dynamic_array), supports_operation, list_conversion).
component(cpp_object_type(dynamic_array), data_type, container).
component(cpp_object_type(dynamic_array), element_type, double).
component(cpp_object_type(dynamic_array), copyable, true).

component(cpp_object_type(rectangle), supports_operation, creation).
component(cpp_object_type(rectangle), supports_operation, dimension_access).
component(cpp_object_type(rectangle), supports_operation, area_calculation).
component(cpp_object_type(rectangle), supports_operation, perimeter_calculation).
component(cpp_object_type(rectangle), supports_operation, property_analysis).
component(cpp_object_type(rectangle), data_type, geometric).
component(cpp_object_type(rectangle), inherits_from, shape).
component(cpp_object_type(rectangle), copyable, false).

component(cpp_object_type(circle), supports_operation, creation).
component(cpp_object_type(circle), supports_operation, radius_access).
component(cpp_object_type(circle), supports_operation, area_calculation).
component(cpp_object_type(circle), supports_operation, circumference_calculation).
component(cpp_object_type(circle), data_type, geometric).
component(cpp_object_type(circle), inherits_from, shape).
component(cpp_object_type(circle), copyable, false).

component(cpp_object_type(graph_node), supports_operation, creation).
component(cpp_object_type(graph_node), supports_operation, property_management).
component(cpp_object_type(graph_node), supports_operation, connection_management).
component(cpp_object_type(graph_node), supports_operation, graph_traversal).
component(cpp_object_type(graph_node), data_type, graph).
component(cpp_object_type(graph_node), uses_container, 'std::map').
component(cpp_object_type(graph_node), uses_container, 'std::vector').
component(cpp_object_type(graph_node), copyable, false).

% Operation category components
component(memory_management, provides_operation, object_creation).
component(memory_management, provides_operation, object_destruction).
component(memory_management, provides_operation, existence_checking).
component(memory_management, provides_operation, garbage_collection).
component(memory_management, provides_operation, memory_statistics).

component(mathematical_operations, provides_operation, distance_calculation).
component(mathematical_operations, provides_operation, point_arithmetic).
component(mathematical_operations, provides_operation, array_statistics).
component(mathematical_operations, provides_operation, geometric_properties).

component(container_operations, provides_operation, element_access).
component(container_operations, provides_operation, resizing).
component(container_operations, provides_operation, sorting).
component(container_operations, provides_operation, list_conversion).

component(geometric_operations, provides_operation, area_calculation).
component(geometric_operations, provides_operation, perimeter_calculation).
component(geometric_operations, provides_operation, shape_analysis).

component(graph_operations, provides_operation, node_creation).
component(graph_operations, provides_operation, property_management).
component(graph_operations, provides_operation, connection_management).
component(graph_operations, provides_operation, traversal).

component(performance_testing, provides_operation, benchmarking).
component(performance_testing, provides_operation, stress_testing).
component(performance_testing, provides_operation, memory_profiling).

component(exception_handling, provides_operation, exception_propagation).
component(exception_handling, provides_operation, error_translation).
component(exception_handling, provides_operation, safe_execution).

% Conversion capabilities
component(type_conversion, converts_between, 'std::vector<T>' - 'Prolog list').
component(type_conversion, converts_between, 'std::map<K,V>' - 'Prolog dict').
component(type_conversion, converts_between, 'C++ objects' - 'Prolog blobs').
component(type_conversion, supports_type, double).
component(type_conversion, supports_type, string).
component(type_conversion, supports_type, integer).

% RAII management
component(raii_management, ensures, automatic_cleanup).
component(raii_management, ensures, exception_safety).
component(raii_management, ensures, resource_ownership).
component(raii_management, uses_pattern, 'shared_ptr').
component(raii_management, uses_pattern, 'RAII').

%% Spell System Integration

% Object creation spells
component(conjure, ctor, cpp_object).
component(conjure, ctor, point_object).
component(conjure, ctor, array_object).
component(conjure, ctor, rectangle_object).
component(conjure, ctor, circle_object).
component(conjure, ctor, graph_node_object).

% Object manipulation spells
component(conjure, ctor, object_operation).
component(conjure, ctor, mathematical_computation).
component(conjure, ctor, container_manipulation).
component(conjure, ctor, graph_modification).

% Performance and testing spells
component(conjure, ctor, performance_test).
component(conjure, ctor, stress_test).
component(conjure, ctor, exception_test).

% Query spells for inspection
component(perceive, ctor, object_info).
component(perceive, ctor, system_stats).
component(perceive, ctor, memory_info).
component(perceive, ctor, capability_query).

%% Spell Implementations

% Object creation spells
cast(conjure(cpp_object(Type, Args)), Handle) :-
    cpp_create(Type, Args, Handle).

cast(conjure(point_object(X, Y)), Handle) :-
    point_new(X, Y, Handle).

cast(conjure(array_object(Size)), Handle) :-
    array_new(Size, Handle).

cast(conjure(rectangle_object(Name, Width, Height)), Handle) :-
    rectangle_new(Name, Width, Height, Handle).

cast(conjure(circle_object(Name, Radius)), Handle) :-
    circle_new(Name, Radius, Handle).

cast(conjure(graph_node_object(Id)), Handle) :-
    graph_node_new(Id, Handle).

% Object manipulation spells
cast(conjure(object_operation(destroy(Handle))), Result) :-
    (cpp_destroy(Handle) -> Result = success ; Result = failure).

cast(conjure(object_operation(copy(Handle))), CopyHandle) :-
    cpp_copy_object(Handle, CopyHandle).

cast(conjure(mathematical_computation(point_distance(H1, H2))), Distance) :-
    point_distance(H1, H2, Distance).

cast(conjure(mathematical_computation(array_stats(Handle))), Stats) :-
    array_statistics(Handle, Stats).

cast(conjure(container_manipulation(array_from_list(Handle, List))), Result) :-
    (array_from_list(Handle, List) -> Result = success ; Result = failure).

cast(conjure(container_manipulation(array_sort(Handle))), Result) :-
    (array_sort(Handle) -> Result = success ; Result = failure).

cast(conjure(graph_modification(connect(H1, H2))), Result) :-
    (graph_connect(H1, H2) -> Result = success ; Result = failure).

cast(conjure(graph_modification(set_property(Handle, Key, Value))), Result) :-
    (graph_node_property(Handle, Key, Value) -> Result = success ; Result = failure).

% Performance testing spells
cast(conjure(performance_test(benchmark(Operation, Iterations))), Result) :-
    cpp_benchmark(Operation, Iterations, Duration, Unit),
    Result = benchmark_result{
        operation: Operation,
        iterations: Iterations,
        duration: Duration,
        unit: Unit
    }.

cast(conjure(stress_test(Type, Count)), Handles) :-
    cpp_stress_test(Type, Count, Handles).

cast(conjure(exception_test(ErrorType)), Result) :-
    catch(
        (cpp_test_exceptions(ErrorType) -> Result = no_exception),
        Error,
        Result = caught_exception(Error)
    ).

% Query spell implementations
query(perceive(object_info(Handle)), Info) :-
    cpp_object_summary(Handle, Info).

query(perceive(system_stats), Stats) :-
    cpp_stats(Stats).

query(perceive(memory_info(Handle)), MemInfo) :-
    cpp_memory_info(Handle, MemInfo).

query(perceive(capability_query(Entity)), Capabilities) :-
    findall(Cap, component(Entity, supports_operation, Cap), OpCaps),
    findall(Cap, component(Entity, provides_capability, Cap), SysCaps),
    Capabilities = capabilities{
        operations: OpCaps,
        system: SysCaps
    }.

%% High-level Domain Operations

% Demonstrate all object types
demonstrate_all_objects(Results) :-
    point_operations_demo(PointResults),
    array_operations_demo(ArrayResults),
    shape_comparison_demo(ShapeResults),
    graph_demo(GraphResults),
    Results = demo_results{
        points: PointResults,
        arrays: ArrayResults,
        shapes: ShapeResults,
        graphs: GraphResults
    }.

% System health check
system_health_check(Health) :-
    cpp_system_info(SystemInfo),
    cpp_stats(Stats),
    cpp_gc(GcRemoved),
    Health = health_check{
        system_info: SystemInfo,
        object_stats: Stats,
        gc_removed: GcRemoved,
        status: healthy
    }.

% Performance benchmark suite
benchmark_suite(Results) :-
    cpp_benchmark(create_destroy_point, 1000, Duration1, Unit1),
    cpp_benchmark(array_operations, 100, Duration2, Unit2),
    Results = benchmark_suite{
        point_ops: benchmark{duration: Duration1, unit: Unit1},
        array_ops: benchmark{duration: Duration2, unit: Unit2}
    }.

% Exception handling test suite
exception_test_suite(Results) :-
    catch(cpp_test_exceptions(none), _, NoExceptionResult = success),
    catch(cpp_test_exceptions(std_exception), Error1, StdExceptionResult = caught(Error1)),
    catch(cpp_test_exceptions(bridge_exception), Error2, BridgeExceptionResult = caught(Error2)),
    catch(cpp_test_exceptions(unknown), Error3, UnknownExceptionResult = caught(Error3)),
    Results = exception_tests{
        no_exception: NoExceptionResult,
        std_exception: StdExceptionResult,
        bridge_exception: BridgeExceptionResult,
        unknown_exception: UnknownExceptionResult
    }.

%% Cleanup and Lifecycle Management

% Automatic cleanup on domain shutdown
cleanup_domain :-
    cpp_cleanup_all,
    cpp_gc(_).

% Safe object operations with automatic cleanup
with_temporary_objects(ObjectSpecs, Goal) :-
    maplist(create_temp_object, ObjectSpecs, Handles),
    call_cleanup(
        call(Goal, Handles),
        maplist(cpp_destroy, Handles)
    ).

create_temp_object(point(X, Y), Handle) :-
    point_new(X, Y, Handle).
create_temp_object(array(Size), Handle) :-
    array_new(Size, Handle).
create_temp_object(rectangle(Name, W, H), Handle) :-
    rectangle_new(Name, W, H, Handle).
create_temp_object(circle(Name, R), Handle) :-
    circle_new(Name, R, Handle).
create_temp_object(graph_node(Id), Handle) :-
    graph_node_new(Id, Handle).

%% Domain Documentation

docstring(cpp_bridge_domain,
    {|string(_)||
    Advanced Prolog-C++ binding domain using PlCxx interface.
    
    Provides comprehensive integration between Prolog and C++ including:
    - C++ objects as Prolog blobs with RAII memory management
    - STL container conversions (vector↔list, map↔dict)
    - Exception propagation from C++ to Prolog
    - Template-based type safety
    - Performance monitoring and benchmarking
    - Modern C++ features (smart pointers, move semantics)
    
    Object Types:
    - Point: 2D coordinate with distance/arithmetic operations
    - DynamicArray: Resizable array with mathematical operations
    - Rectangle/Circle: Geometric shapes with property calculations
    - GraphNode: Graph node with properties and connections
    
    Features:
    - Thread-safe object registry
    - Automatic garbage collection
    - Exception-safe operations
    - Performance benchmarking
    - Memory profiling
    - Type-safe conversions
    |}).

docstring(cpp_bridge_system,
    {|string(_)||
    Core C++ bridge system managing object lifecycle and operations.
    Provides type-safe, exception-safe interface to C++ objects.
    |}).

docstring(memory_management,
    {|string(_)||
    RAII-based memory management for C++ objects in Prolog.
    Ensures automatic cleanup and exception safety.
    |}).

docstring(mathematical_operations,
    {|string(_)||
    Mathematical operations on C++ objects including geometric
    calculations, statistical analysis, and arithmetic operations.
    |}).

docstring(performance_testing,
    {|string(_)||
    Performance testing and benchmarking capabilities for
    C++ bridge operations and object lifecycle management.
    |}).

docstring(exception_handling,
    {|string(_)||
    Exception propagation and safe execution mechanisms
    for C++ operations called from Prolog.
    |}).
