#include "cpp_bridge.hpp"
#include "data_structures.hpp"
#include "object_manager.hpp"
#include <chrono>

// Advanced type conversion predicates

PREDICATE(cpp_vector_to_list, 3) {
    try {
        std::string type = A1.as_string();
        PlTerm input = A2;
        
        if (type == "double") {
            std::vector<double> vec = cpp_bridge::ContainerConverter::list_to_vector<double>(input);
            PlTerm list = cpp_bridge::ContainerConverter::vector_to_list(vec);
            return A3.unify_term(list);
        }
        else if (type == "string") {
            std::vector<std::string> vec = cpp_bridge::ContainerConverter::list_to_vector<std::string>(input);
            PlTerm list = cpp_bridge::ContainerConverter::vector_to_list(vec);
            return A3.unify_term(list);
        }
        else if (type == "int") {
            std::vector<int64_t> vec = cpp_bridge::ContainerConverter::list_to_vector<int64_t>(input);
            PlTerm list = cpp_bridge::ContainerConverter::vector_to_list(vec);
            return A3.unify_term(list);
        }
        else {
            throw cpp_bridge::BridgeException("Unsupported vector type: " + type);
        }
        
    } catch (const std::exception& e) {
        PlTerm except = PlTerm_term("error",
                                   PlTerm_term("conversion_error",
                                              PlTerm_atom(e.what())),
                                   PlTerm_var());
        throw PlException(except);
    }
}

PREDICATE(cpp_map_to_dict, 3) {
    try {
        std::string key_type = A1.as_string();
        std::string value_type = A2.as_string();
        PlTerm dict = A3;
        
        // For now, support string->string maps
        if (key_type == "string" && value_type == "string") {
            std::map<std::string, std::string> cpp_map = 
                cpp_bridge::ContainerConverter::dict_to_map<std::string, std::string>(dict);
            PlTerm result_dict = cpp_bridge::ContainerConverter::map_to_dict(cpp_map);
            return A3.unify_term(result_dict);
        }
        else {
            throw cpp_bridge::BridgeException("Unsupported map types: " + key_type + "->" + value_type);
        }
        
    } catch (const std::exception& e) {
        PlTerm except = PlTerm_term("error",
                                   PlTerm_term("conversion_error",
                                              PlTerm_atom(e.what())),
                                   PlTerm_var());
        throw PlException(except);
    }
}

PREDICATE(cpp_test_exception_handling, 1) {
    try {
        std::string error_type = A1.as_string();
        
        if (error_type == "std_exception") {
            throw std::runtime_error("Test standard exception");
        }
        else if (error_type == "bridge_exception") {
            throw cpp_bridge::BridgeException("Test bridge exception");
        }
        else if (error_type == "unknown") {
            throw 42; // Unknown exception type
        }
        else {
            return true; // No exception
        }
        
    } catch (const cpp_bridge::BridgeException& e) {
        PlTerm except = PlTerm_term("error",
                                   PlTerm_term("cpp_bridge_error",
                                              PlTerm_atom(e.what())),
                                   PlTerm_var());
        throw PlException(except);
    } catch (const std::exception& e) {
        PlTerm except = PlTerm_term("error",
                                   PlTerm_term("cpp_exception",
                                              PlTerm_atom(e.what())),
                                   PlTerm_var());
        throw PlException(except);
    } catch (...) {
        PlTerm except = PlTerm_term("error",
                                   PlTerm_atom("unknown_cpp_exception"),
                                   PlTerm_var());
        throw PlException(except);
    }
}

// Performance and stress testing predicates

PREDICATE(cpp_stress_test_objects, 3) {
    try {
        std::string type = A1.as_string();
        size_t count = A2.as_int64_t();
        
        std::vector<std::string> handles;
        
        // Create many objects
        for (size_t i = 0; i < count; ++i) {
            std::string handle;
            
            if (type == "point") {
                handle = cpp_bridge::ObjectFactory::create_point(i, i * 2);
            }
            else if (type == "dynamic_array") {
                handle = cpp_bridge::ObjectFactory::create_dynamic_array(10);
            }
            else {
                throw cpp_bridge::BridgeException("Unsupported type for stress test: " + type);
            }
            
            handles.push_back(handle);
        }
        
        // Convert handles to Prolog list
        PlTerm list = cpp_bridge::ContainerConverter::vector_to_list(handles);
        
        // Clean up objects
        for (const auto& handle : handles) {
            cpp_bridge::ObjectManager::instance().remove_object(handle);
        }
        
        return A3.unify_term(list);
        
    } catch (const std::exception& e) {
        PlTerm except = PlTerm_term("error",
                                   PlTerm_term("stress_test_error",
                                              PlTerm_atom(e.what())),
                                   PlTerm_var());
        throw PlException(except);
    }
}

PREDICATE(cpp_benchmark_operations, 4) {
    try {
        std::string operation = A1.as_string();
        size_t iterations = A2.as_int64_t();
        
        auto start = std::chrono::high_resolution_clock::now();
        
        if (operation == "create_destroy_point") {
            for (size_t i = 0; i < iterations; ++i) {
                std::string handle = cpp_bridge::ObjectFactory::create_point(i, i);
                cpp_bridge::ObjectManager::instance().remove_object(handle);
            }
        }
        else if (operation == "array_operations") {
            std::string handle = cpp_bridge::ObjectFactory::create_dynamic_array(0);
            auto array = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::DynamicArray>(handle);
            
            for (size_t i = 0; i < iterations; ++i) {
                array->push_back(i);
                if (i > 0 && i % 100 == 0) {
                    array->sort();
                }
            }
            
            cpp_bridge::ObjectManager::instance().remove_object(handle);
        }
        else {
            throw cpp_bridge::BridgeException("Unknown benchmark operation: " + operation);
        }
        
        auto end = std::chrono::high_resolution_clock::now();
        auto duration = std::chrono::duration_cast<std::chrono::microseconds>(end - start);
        
        return A3.unify_integer(duration.count()) && 
               A4.unify_atom("microseconds");
               
    } catch (const std::exception& e) {
        PlTerm except = PlTerm_term("error",
                                   PlTerm_term("benchmark_error",
                                              PlTerm_atom(e.what())),
                                   PlTerm_var());
        throw PlException(except);
    }
}

// Memory management and introspection predicates

PREDICATE(cpp_object_memory_info, 2) {
    try {
        std::string handle = A1.as_string();
        auto obj = cpp_bridge::ObjectManager::instance().get_object(handle);
        
        PlTerm_var info_dict = PlTerm::dict();
        info_dict.put_dict(PlTerm_atom("type"), PlTerm_atom(obj->get_type_name().c_str()));
        info_dict.put_dict(PlTerm_atom("handle"), PlTerm_atom(handle.c_str()));
        info_dict.put_dict(PlTerm_atom("string_repr"), PlTerm_atom(obj->to_string().c_str()));
        
        // Add type-specific memory information
        if (obj->get_type_name() == "dynamic_array") {
            auto array = std::dynamic_pointer_cast<cpp_bridge::DynamicArray>(obj);
            if (array) {
                info_dict.put_dict(PlTerm_atom("size"), PlTerm_integer(array->size()));
                info_dict.put_dict(PlTerm_atom("capacity_estimate"), 
                                   PlTerm_integer(array->size() * sizeof(double)));
            }
        }
        
        return A2.unify_term(info_dict);
        
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(cpp_system_info, 1) {
    try {
        PlTerm_var info_dict = PlTerm::dict();
        
        auto stats = cpp_bridge::ObjectManager::instance().get_stats();
        
        info_dict.put_dict(PlTerm_atom("total_objects"), PlTerm_integer(stats.total_objects));
        
        PlTerm_var types_dict = PlTerm::dict();
        for (const auto& [type, count] : stats.objects_by_type) {
            types_dict.put_dict(PlTerm_atom(type.c_str()), PlTerm_integer(count));
        }
        info_dict.put_dict(PlTerm_atom("objects_by_type"), types_dict);
        
        // Add system information
        info_dict.put_dict(PlTerm_atom("cpp_standard"), PlTerm_atom("C++20"));
        info_dict.put_dict(PlTerm_atom("bridge_version"), PlTerm_atom("1.0.0"));
        info_dict.put_dict(PlTerm_atom("memory_management"), PlTerm_atom("RAII + shared_ptr"));
        
        return A1.unify_term(info_dict);
        
    } catch (const std::exception& e) {
        return false;
    }
}
