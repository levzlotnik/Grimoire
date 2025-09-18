#include "data_structures.hpp"
#include "object_manager.hpp"
#include <cmath>

// Foreign predicates for Point operations

PREDICATE(point_get_x, 2) {
    try {
        std::string handle = A1.as_string();
        auto point = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::Point>(handle);
        return A2.unify_float(point->x());
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(point_get_y, 2) {
    try {
        std::string handle = A1.as_string();
        auto point = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::Point>(handle);
        return A2.unify_float(point->y());
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(point_set_x, 2) {
    try {
        std::string handle = A1.as_string();
        double x = A2.as_double();
        auto point = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::Point>(handle);
        point->set_x(x);
        return true;
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(point_set_y, 2) {
    try {
        std::string handle = A1.as_string();
        double y = A2.as_double();
        auto point = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::Point>(handle);
        point->set_y(y);
        return true;
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(point_distance_to, 3) {
    try {
        std::string handle1 = A1.as_string();
        std::string handle2 = A2.as_string();
        auto point1 = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::Point>(handle1);
        auto point2 = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::Point>(handle2);
        double distance = point1->distance_to(*point2);
        return A3.unify_float(distance);
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(point_add, 3) {
    try {
        std::string handle1 = A1.as_string();
        std::string handle2 = A2.as_string();
        auto point1 = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::Point>(handle1);
        auto point2 = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::Point>(handle2);
        
        cpp_bridge::Point result = *point1 + *point2;
        std::string result_handle = cpp_bridge::ObjectManager::instance().create_object<cpp_bridge::Point>(result.x(), result.y());
        
        return A3.unify_atom(result_handle.c_str());
    } catch (const std::exception& e) {
        return false;
    }
}

// Foreign predicates for DynamicArray operations

PREDICATE(array_size, 2) {
    try {
        std::string handle = A1.as_string();
        auto array = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::DynamicArray>(handle);
        return A2.unify_integer(array->size());
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(array_push_back, 2) {
    try {
        std::string handle = A1.as_string();
        double value = A2.as_double();
        auto array = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::DynamicArray>(handle);
        array->push_back(value);
        return true;
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(array_pop_back, 1) {
    try {
        std::string handle = A1.as_string();
        auto array = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::DynamicArray>(handle);
        array->pop_back();
        return true;
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(array_at, 3) {
    try {
        std::string handle = A1.as_string();
        size_t index = A2.as_int64_t();
        auto array = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::DynamicArray>(handle);
        double value = array->at(index);
        return A3.unify_float(value);
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(array_set_at, 3) {
    try {
        std::string handle = A1.as_string();
        size_t index = A2.as_int64_t();
        double value = A3.as_double();
        auto array = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::DynamicArray>(handle);
        array->at(index) = value;
        return true;
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(array_sum, 2) {
    try {
        std::string handle = A1.as_string();
        auto array = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::DynamicArray>(handle);
        return A2.unify_float(array->sum());
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(array_mean, 2) {
    try {
        std::string handle = A1.as_string();
        auto array = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::DynamicArray>(handle);
        return A2.unify_float(array->mean());
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(array_sort, 1) {
    try {
        std::string handle = A1.as_string();
        auto array = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::DynamicArray>(handle);
        array->sort();
        return true;
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(array_to_list, 2) {
    try {
        std::string handle = A1.as_string();
        auto array = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::DynamicArray>(handle);
        auto vec = array->to_vector();
        
        PlTerm list = cpp_bridge::ContainerConverter::vector_to_list(vec);
        return A2.unify_term(list);
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(array_from_list, 2) {
    try {
        std::string handle = A1.as_string();
        PlTerm list = A2;
        auto array = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::DynamicArray>(handle);
        
        auto vec = cpp_bridge::ContainerConverter::list_to_vector<double>(list);
        array->from_vector(vec);
        return true;
    } catch (const std::exception& e) {
        return false;
    }
}

// Foreign predicates for Shape operations (Rectangle)

PREDICATE(rectangle_get_width, 2) {
    try {
        std::string handle = A1.as_string();
        auto rect = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::Rectangle>(handle);
        return A2.unify_float(rect->width());
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(rectangle_get_height, 2) {
    try {
        std::string handle = A1.as_string();
        auto rect = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::Rectangle>(handle);
        return A2.unify_float(rect->height());
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(rectangle_set_width, 2) {
    try {
        std::string handle = A1.as_string();
        double width = A2.as_double();
        auto rect = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::Rectangle>(handle);
        rect->set_width(width);
        return true;
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(rectangle_set_height, 2) {
    try {
        std::string handle = A1.as_string();
        double height = A2.as_double();
        auto rect = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::Rectangle>(handle);
        rect->set_height(height);
        return true;
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(rectangle_area, 2) {
    try {
        std::string handle = A1.as_string();
        auto rect = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::Rectangle>(handle);
        return A2.unify_float(rect->area());
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(rectangle_perimeter, 2) {
    try {
        std::string handle = A1.as_string();
        auto rect = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::Rectangle>(handle);
        return A2.unify_float(rect->perimeter());
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(rectangle_is_square, 1) {
    try {
        std::string handle = A1.as_string();
        auto rect = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::Rectangle>(handle);
        return rect->is_square();
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(rectangle_diagonal, 2) {
    try {
        std::string handle = A1.as_string();
        auto rect = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::Rectangle>(handle);
        return A2.unify_float(rect->diagonal());
    } catch (const std::exception& e) {
        return false;
    }
}

// Foreign predicates for Circle operations

PREDICATE(circle_get_radius, 2) {
    try {
        std::string handle = A1.as_string();
        auto circle = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::Circle>(handle);
        return A2.unify_float(circle->radius());
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(circle_set_radius, 2) {
    try {
        std::string handle = A1.as_string();
        double radius = A2.as_double();
        auto circle = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::Circle>(handle);
        circle->set_radius(radius);
        return true;
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(circle_area, 2) {
    try {
        std::string handle = A1.as_string();
        auto circle = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::Circle>(handle);
        return A2.unify_float(circle->area());
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(circle_perimeter, 2) {
    try {
        std::string handle = A1.as_string();
        auto circle = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::Circle>(handle);
        return A2.unify_float(circle->perimeter());
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(circle_diameter, 2) {
    try {
        std::string handle = A1.as_string();
        auto circle = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::Circle>(handle);
        return A2.unify_float(circle->diameter());
    } catch (const std::exception& e) {
        return false;
    }
}

// Foreign predicates for GraphNode operations

PREDICATE(graph_node_get_id, 2) {
    try {
        std::string handle = A1.as_string();
        auto node = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::GraphNode>(handle);
        return A2.unify_atom(node->id().c_str());
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(graph_node_set_property, 3) {
    try {
        std::string handle = A1.as_string();
        std::string key = A2.as_string();
        std::string value = A3.as_string();
        auto node = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::GraphNode>(handle);
        node->set_property(key, value);
        return true;
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(graph_node_get_property, 3) {
    try {
        std::string handle = A1.as_string();
        std::string key = A2.as_string();
        auto node = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::GraphNode>(handle);
        std::string value = node->get_property(key);
        return A3.unify_atom(value.c_str());
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(graph_node_has_property, 2) {
    try {
        std::string handle = A1.as_string();
        std::string key = A2.as_string();
        auto node = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::GraphNode>(handle);
        return node->has_property(key);
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(graph_node_remove_property, 2) {
    try {
        std::string handle = A1.as_string();
        std::string key = A2.as_string();
        auto node = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::GraphNode>(handle);
        node->remove_property(key);
        return true;
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(graph_node_get_all_properties, 2) {
    try {
        std::string handle = A1.as_string();
        auto node = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::GraphNode>(handle);
        auto props = node->get_all_properties();
        
        PlTerm dict = cpp_bridge::ContainerConverter::map_to_dict(props);
        return A2.unify_term(dict);
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(graph_node_add_connection, 2) {
    try {
        std::string handle1 = A1.as_string();
        std::string handle2 = A2.as_string();
        auto node1 = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::GraphNode>(handle1);
        auto node2 = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::GraphNode>(handle2);
        node1->add_connection(node2);
        return true;
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(graph_node_remove_connection, 2) {
    try {
        std::string handle = A1.as_string();
        std::string target_id = A2.as_string();
        auto node = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::GraphNode>(handle);
        node->remove_connection(target_id);
        return true;
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(graph_node_get_connections, 2) {
    try {
        std::string handle = A1.as_string();
        auto node = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::GraphNode>(handle);
        auto connection_ids = node->get_connection_ids();
        
        PlTerm list = cpp_bridge::ContainerConverter::vector_to_list(connection_ids);
        return A2.unify_term(list);
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(graph_node_connection_count, 2) {
    try {
        std::string handle = A1.as_string();
        auto node = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::GraphNode>(handle);
        return A2.unify_integer(node->connection_count());
    } catch (const std::exception& e) {
        return false;
    }
}

PREDICATE(graph_node_is_connected_to, 2) {
    try {
        std::string handle = A1.as_string();
        std::string target_id = A2.as_string();
        auto node = cpp_bridge::ObjectManager::instance().get_object<cpp_bridge::GraphNode>(handle);
        return node->is_connected_to(target_id);
    } catch (const std::exception& e) {
        return false;
    }
}