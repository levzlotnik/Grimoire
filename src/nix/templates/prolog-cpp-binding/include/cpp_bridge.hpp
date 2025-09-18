#pragma once

#include <SWI-Prolog.h>
#include <SWI-cpp2.h>
#include <memory>
#include <vector>
#include <map>
#include <string>
#include <stdexcept>
#include <type_traits>

/**
 * @file cpp_bridge.hpp
 * @brief Advanced C++ to Prolog bridge using PlCxx interface
 * 
 * This header provides a comprehensive bridge between C++ and Prolog,
 * demonstrating state-of-the-art integration patterns including:
 * - C++ objects as Prolog blobs with RAII
 * - STL container conversions
 * - Exception propagation
 * - Template-based type safety
 * - Modern C++ features (smart pointers, move semantics)
 */

namespace cpp_bridge {

/**
 * @brief Exception class for C++ bridge errors
 */
class BridgeException : public std::runtime_error {
public:
    explicit BridgeException(const std::string& message) 
        : std::runtime_error("C++ Bridge Error: " + message) {}
};

/**
 * @brief Base class for all C++ objects exposed to Prolog as blobs
 * 
 * Provides automatic memory management and type safety when passing
 * C++ objects between languages.
 */
class ManagedObject : public PlBlob {
private:
    static std::map<std::string, size_t> type_registry_;
    
public:
    ManagedObject() = default;
    virtual ~ManagedObject() = default;
    
    // Non-copyable but movable
    ManagedObject(const ManagedObject&) = delete;
    ManagedObject& operator=(const ManagedObject&) = delete;
    ManagedObject(ManagedObject&&) = default;
    ManagedObject& operator=(ManagedObject&&) = default;
    
    /**
     * @brief Get the type name for this object
     */
    virtual std::string get_type_name() const = 0;
    
    /**
     * @brief Get a string representation of this object
     */
    virtual std::string to_string() const = 0;
    
    /**
     * @brief Clone this object (for copy semantics if needed)
     */
    virtual std::unique_ptr<ManagedObject> clone() const = 0;
    
    // PlBlob interface implementation
    PL_blob_t* blob_type() override;
    void write_fields(IOSTREAM* stream) override;
    int compare_fields(PlBlob* other) override;
    
    /**
     * @brief Register a C++ type with the blob system
     */
    template<typename T>
    static void register_type() {
        static_assert(std::is_base_of_v<ManagedObject, T>, 
                      "Type must inherit from ManagedObject");
        type_registry_[T::type_name()] = sizeof(T);
    }
};

/**
 * @brief Template wrapper for automatic type registration and blob creation
 */
template<typename T>
class TypedBlob : public ManagedObject {
public:
    static constexpr const char* type_name() {
        return T::cpp_type_name();
    }
    
    std::string get_type_name() const override {
        return T::cpp_type_name();
    }
    
    /**
     * @brief Create a Prolog term containing this object as a blob
     */
    static PlTerm create_blob(std::unique_ptr<T> obj) {
        PlTerm term;
        term.unify_blob(obj.release());
        return term;
    }
    
    /**
     * @brief Extract C++ object from Prolog blob term
     */
    static T* from_blob(PlTerm term) {
        if (!term.is_blob()) {
            throw BridgeException("Term is not a blob");
        }
        
        auto* blob = static_cast<T*>(term.as_blob());
        if (!blob || blob->get_type_name() != T::cpp_type_name()) {
            throw BridgeException("Blob type mismatch");
        }
        
        return blob;
    }
};

/**
 * @brief Utility class for converting between STL containers and Prolog terms
 */
class ContainerConverter {
public:
    /**
     * @brief Convert std::vector to Prolog list
     */
    template<typename T>
    static PlTerm vector_to_list(const std::vector<T>& vec) {
        PlTerm_var list;
        PlTail tail(list);
        
        for (const auto& item : vec) {
            if constexpr (std::is_same_v<T, std::string>) {
                tail.append(PlTerm_atom(item.c_str()));
            } else if constexpr (std::is_integral_v<T>) {
                tail.append(PlTerm_integer(item));
            } else if constexpr (std::is_floating_point_v<T>) {
                tail.append(PlTerm_float(item));
            } else {
                // For complex types, convert to blob
                auto obj = std::make_unique<T>(item);
                tail.append(TypedBlob<T>::create_blob(std::move(obj)));
            }
        }
        
        tail.close();
        return list;
    }
    
    /**
     * @brief Convert Prolog list to std::vector
     */
    template<typename T>
    static std::vector<T> list_to_vector(PlTerm list) {
        std::vector<T> result;
        
        PlTail tail(list);
        PlTerm_var item;
        
        while (tail.next(item)) {
            if constexpr (std::is_same_v<T, std::string>) {
                result.push_back(item.as_string());
            } else if constexpr (std::is_integral_v<T>) {
                result.push_back(item.as_int64_t());
            } else if constexpr (std::is_floating_point_v<T>) {
                result.push_back(item.as_double());
            } else {
                // For complex types, extract from blob
                T* obj = TypedBlob<T>::from_blob(item);
                result.push_back(*obj);
            }
        }
        
        return result;
    }
    
    /**
     * @brief Convert std::map to Prolog dict
     */
    template<typename K, typename V>
    static PlTerm map_to_dict(const std::map<K, V>& map) {
        PlTerm_var dict = PlTerm::dict();
        
        for (const auto& [key, value] : map) {
            PlTerm key_term, value_term;
            
            // Convert key
            if constexpr (std::is_same_v<K, std::string>) {
                key_term = PlTerm_atom(key.c_str());
            } else if constexpr (std::is_integral_v<K>) {
                key_term = PlTerm_integer(key);
            }
            
            // Convert value
            if constexpr (std::is_same_v<V, std::string>) {
                value_term = PlTerm_atom(value.c_str());
            } else if constexpr (std::is_integral_v<V>) {
                value_term = PlTerm_integer(value);
            } else if constexpr (std::is_floating_point_v<V>) {
                value_term = PlTerm_float(value);
            }
            
            dict.put_dict(key_term, value_term);
        }
        
        return dict;
    }
    
    /**
     * @brief Convert Prolog dict to std::map
     */
    template<typename K, typename V>
    static std::map<K, V> dict_to_map(PlTerm dict) {
        std::map<K, V> result;
        
        // Iterate over dict keys
        PlTerm_var keys = dict.dict_keys();
        PlTail key_tail(keys);
        PlTerm_var key;
        
        while (key_tail.next(key)) {
            PlTerm_var value = dict.get_dict(key);
            
            K cpp_key;
            V cpp_value;
            
            // Convert key
            if constexpr (std::is_same_v<K, std::string>) {
                cpp_key = key.as_string();
            } else if constexpr (std::is_integral_v<K>) {
                cpp_key = key.as_int64_t();
            }
            
            // Convert value
            if constexpr (std::is_same_v<V, std::string>) {
                cpp_value = value.as_string();
            } else if constexpr (std::is_integral_v<V>) {
                cpp_value = value.as_int64_t();
            } else if constexpr (std::is_floating_point_v<V>) {
                cpp_value = value.as_double();
            }
            
            result[cpp_key] = cpp_value;
        }
        
        return result;
    }
};

/**
 * @brief Exception handler that converts C++ exceptions to Prolog exceptions
 */
class ExceptionHandler {
public:
    /**
     * @brief Execute a function and convert any C++ exceptions to Prolog exceptions
     */
    template<typename Func>
    static bool safe_call(Func&& func) {
        try {
            func();
            return true;
        } catch (const BridgeException& e) {
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
        return false;
    }
};

/**
 * @brief Macro for defining foreign predicates with automatic exception handling
 */
#define DEFINE_SAFE_PREDICATE(name, arity, body) \
    PREDICATE(name, arity) { \
        return ExceptionHandler::safe_call([&]() { \
            body \
        }); \
    }

} // namespace cpp_bridge
