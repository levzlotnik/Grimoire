#include "cpp_bridge.hpp"
#include <iostream>
#include <sstream>

namespace cpp_bridge {

// Static member definitions
std::map<std::string, size_t> ManagedObject::type_registry_;

// Blob type definition for ManagedObject
static PL_blob_t managed_object_blob = {
    PL_BLOB_MAGIC,
    PL_BLOB_UNIQUE,
    "managed_object",
    nullptr,    // release
    nullptr,    // compare
    nullptr,    // write
    nullptr     // acquire
};

PL_blob_t* ManagedObject::blob_type() {
    return &managed_object_blob;
}

void ManagedObject::write_fields(IOSTREAM* stream) {
    std::string repr = to_string();
    Sfprintf(stream, "%s", repr.c_str());
}

int ManagedObject::compare_fields(PlBlob* other) {
    auto* other_obj = dynamic_cast<ManagedObject*>(other);
    if (!other_obj) {
        return -1; // Different types
    }
    
    // Compare by type name first
    std::string this_type = get_type_name();
    std::string other_type = other_obj->get_type_name();
    
    if (this_type < other_type) return -1;
    if (this_type > other_type) return 1;
    
    // Same type, compare string representations
    std::string this_str = to_string();
    std::string other_str = other_obj->to_string();
    
    if (this_str < other_str) return -1;
    if (this_str > other_str) return 1;
    return 0;
}

} // namespace cpp_bridge

// Foreign predicates for basic object management

PREDICATE(cpp_create_object, 3) {
    try {
        std::string type = A1.as_string();
        PlTerm args = A2;
        
        std::string handle;
        
        if (type == "point") {
            double x = 0.0, y = 0.0;
            if (args.arity() >= 1) x = args[1].as_double();
            if (args.arity() >= 2) y = args[2].as_double();
            handle = cpp_bridge::ObjectFactory::create_point(x, y);
        }
        else if (type == "dynamic_array") {
            size_t size = 0;
            if (args.arity() >= 1) size = args[1].as_int64_t();
            handle = cpp_bridge::ObjectFactory::create_dynamic_array(size);
        }
        else if (type == "rectangle") {
            if (args.arity() < 3) {
                throw cpp_bridge::BridgeException("Rectangle requires name, width, height");
            }
            std::string name = args[1].as_string();
            double width = args[2].as_double();
            double height = args[3].as_double();
            handle = cpp_bridge::ObjectFactory::create_rectangle(name, width, height);
        }
        else if (type == "circle") {
            if (args.arity() < 2) {
                throw cpp_bridge::BridgeException("Circle requires name, radius");
            }
            std::string name = args[1].as_string();
            double radius = args[2].as_double();
            handle = cpp_bridge::ObjectFactory::create_circle(name, radius);
        }
        else if (type == "graph_node") {
            if (args.arity() < 1) {
                throw cpp_bridge::BridgeException("GraphNode requires id");
            }
            std::string id = args[1].as_string();
            handle = cpp_bridge::ObjectFactory::create_graph_node(id);
        }
        else {
            throw cpp_bridge::BridgeException("Unknown object type: " + type);
        }
        
        return A3.unify_atom(handle.c_str());
        
    } catch (const std::exception& e) {
        PlTerm except = PlTerm_term("error",
                                   PlTerm_term("cpp_exception",
                                              PlTerm_atom(e.what())),
                                   PlTerm_var());
        throw PlException(except);
    }
}

PREDICATE(cpp_object_exists, 1) {
    try {
        std::string handle = A1.as_string();
        return cpp_bridge::ObjectManager::instance().has_object(handle);
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(cpp_object_type, 2) {
    try {
        std::string handle = A1.as_string();
        auto obj = cpp_bridge::ObjectManager::instance().get_object(handle);
        return A2.unify_atom(obj->get_type_name().c_str());
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(cpp_object_to_string, 2) {
    try {
        std::string handle = A1.as_string();
        auto obj = cpp_bridge::ObjectManager::instance().get_object(handle);
        return A2.unify_atom(obj->to_string().c_str());
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(cpp_destroy_object, 1) {
    try {
        std::string handle = A1.as_string();
        return cpp_bridge::ObjectManager::instance().remove_object(handle);
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(cpp_list_objects, 1) {
    try {
        auto handles = cpp_bridge::ObjectManager::instance().get_all_handles();
        PlTerm_var list;
        PlTail tail(list);
        
        for (const auto& handle : handles) {
            tail.append(PlTerm_atom(handle.c_str()));
        }
        tail.close();
        
        return A1.unify_term(list);
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(cpp_list_objects_by_type, 2) {
    try {
        std::string type = A1.as_string();
        auto handles = cpp_bridge::ObjectManager::instance().get_objects_by_type(type);
        
        PlTerm_var list;
        PlTail tail(list);
        
        for (const auto& handle : handles) {
            tail.append(PlTerm_atom(handle.c_str()));
        }
        tail.close();
        
        return A2.unify_term(list);
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(cpp_object_stats, 1) {
    try {
        auto stats = cpp_bridge::ObjectManager::instance().get_stats();
        
        PlTerm_var dict = PlTerm::dict();
        dict.put_dict(PlTerm_atom("total"), PlTerm_integer(stats.total_objects));
        
        PlTerm_var by_type_dict = PlTerm::dict();
        for (const auto& [type, count] : stats.objects_by_type) {
            by_type_dict.put_dict(PlTerm_atom(type.c_str()), PlTerm_integer(count));
        }
        dict.put_dict(PlTerm_atom("by_type"), by_type_dict);
        
        return A1.unify_term(dict);
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(cpp_garbage_collect, 1) {
    try {
        size_t removed = cpp_bridge::ObjectManager::instance().garbage_collect();
        return A1.unify_integer(removed);
    } catch (const std::exception& e) {
        return false;
    }
}
