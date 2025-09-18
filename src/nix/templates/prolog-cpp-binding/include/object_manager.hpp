#pragma once

#include "cpp_bridge.hpp"
#include "data_structures.hpp"
#include <unordered_map>
#include <memory>
#include <string>
#include <mutex>
#include <atomic>

/**
 * @file object_manager.hpp
 * @brief Advanced object lifecycle management for C++ objects in Prolog
 * 
 * Provides sophisticated memory management, object registry, and lifecycle
 * control for C++ objects used in Prolog predicates.
 */

namespace cpp_bridge {

/**
 * @brief Thread-safe registry for managing C++ object lifecycles
 * 
 * This class provides centralized management of C++ objects that are
 * exposed to Prolog, including:
 * - Automatic cleanup on Prolog garbage collection
 * - Object lookup by handle/ID
 * - Reference counting and lifecycle management
 * - Thread-safe operations
 */
class ObjectManager {
private:
    static ObjectManager* instance_;
    static std::mutex instance_mutex_;
    
    mutable std::mutex registry_mutex_;
    std::unordered_map<std::string, std::shared_ptr<ManagedObject>> objects_;
    std::atomic<uint64_t> next_id_{1};
    
    // Private constructor for singleton
    ObjectManager() = default;
    
public:
    ~ObjectManager() = default;
    
    // Non-copyable, non-movable
    ObjectManager(const ObjectManager&) = delete;
    ObjectManager& operator=(const ObjectManager&) = delete;
    ObjectManager(ObjectManager&&) = delete;
    ObjectManager& operator=(ObjectManager&&) = delete;
    
    /**
     * @brief Get the singleton instance
     */
    static ObjectManager& instance() {
        std::lock_guard<std::mutex> lock(instance_mutex_);
        if (!instance_) {
            instance_ = new ObjectManager();
        }
        return *instance_;
    }
    
    /**
     * @brief Register a new object and return its handle
     */
    template<typename T, typename... Args>
    std::string create_object(Args&&... args) {
        static_assert(std::is_base_of_v<ManagedObject, T>,
                      "T must inherit from ManagedObject");
        
        auto obj = std::make_shared<T>(std::forward<Args>(args)...);
        std::string handle = generate_handle(T::cpp_type_name());
        
        std::lock_guard<std::mutex> lock(registry_mutex_);
        objects_[handle] = std::static_pointer_cast<ManagedObject>(obj);
        
        return handle;
    }
    
    /**
     * @brief Register an existing object and return its handle
     */
    std::string register_object(std::shared_ptr<ManagedObject> obj) {
        if (!obj) {
            throw BridgeException("Cannot register null object");
        }
        
        std::string handle = generate_handle(obj->get_type_name());
        
        std::lock_guard<std::mutex> lock(registry_mutex_);
        objects_[handle] = obj;
        
        return handle;
    }
    
    /**
     * @brief Get an object by handle with type checking
     */
    template<typename T>
    std::shared_ptr<T> get_object(const std::string& handle) {
        static_assert(std::is_base_of_v<ManagedObject, T>,
                      "T must inherit from ManagedObject");
        
        std::lock_guard<std::mutex> lock(registry_mutex_);
        auto it = objects_.find(handle);
        
        if (it == objects_.end()) {
            throw BridgeException("Object not found: " + handle);
        }
        
        auto typed_obj = std::dynamic_pointer_cast<T>(it->second);
        if (!typed_obj) {
            throw BridgeException("Object type mismatch for handle: " + handle);
        }
        
        return typed_obj;
    }
    
    /**
     * @brief Get an object by handle without type checking
     */
    std::shared_ptr<ManagedObject> get_object(const std::string& handle) {
        std::lock_guard<std::mutex> lock(registry_mutex_);
        auto it = objects_.find(handle);
        
        if (it == objects_.end()) {
            throw BridgeException("Object not found: " + handle);
        }
        
        return it->second;
    }
    
    /**
     * @brief Check if an object exists
     */
    bool has_object(const std::string& handle) const {
        std::lock_guard<std::mutex> lock(registry_mutex_);
        return objects_.find(handle) != objects_.end();
    }
    
    /**
     * @brief Remove an object from the registry
     */
    bool remove_object(const std::string& handle) {
        std::lock_guard<std::mutex> lock(registry_mutex_);
        return objects_.erase(handle) > 0;
    }
    
    /**
     * @brief Get all object handles of a specific type
     */
    std::vector<std::string> get_objects_by_type(const std::string& type_name) const {
        std::vector<std::string> handles;
        
        std::lock_guard<std::mutex> lock(registry_mutex_);
        for (const auto& [handle, obj] : objects_) {
            if (obj && obj->get_type_name() == type_name) {
                handles.push_back(handle);
            }
        }
        
        return handles;
    }
    
    /**
     * @brief Get all registered object handles
     */
    std::vector<std::string> get_all_handles() const {
        std::vector<std::string> handles;
        
        std::lock_guard<std::mutex> lock(registry_mutex_);
        for (const auto& [handle, obj] : objects_) {
            handles.push_back(handle);
        }
        
        return handles;
    }
    
    /**
     * @brief Get registry statistics
     */
    struct RegistryStats {
        size_t total_objects;
        std::map<std::string, size_t> objects_by_type;
    };
    
    RegistryStats get_stats() const {
        RegistryStats stats;
        stats.total_objects = 0;
        
        std::lock_guard<std::mutex> lock(registry_mutex_);
        stats.total_objects = objects_.size();
        
        for (const auto& [handle, obj] : objects_) {
            if (obj) {
                stats.objects_by_type[obj->get_type_name()]++;
            }
        }
        
        return stats;
    }
    
    /**
     * @brief Clear all objects (use with caution)
     */
    void clear() {
        std::lock_guard<std::mutex> lock(registry_mutex_);
        objects_.clear();
    }
    
    /**
     * @brief Garbage collect unused objects
     * 
     * Removes objects that have only one reference (the registry)
     */
    size_t garbage_collect() {
        std::lock_guard<std::mutex> lock(registry_mutex_);
        size_t removed = 0;
        
        auto it = objects_.begin();
        while (it != objects_.end()) {
            if (it->second.use_count() == 1) {
                it = objects_.erase(it);
                ++removed;
            } else {
                ++it;
            }
        }
        
        return removed;
    }

private:
    /**
     * @brief Generate a unique handle for an object
     */
    std::string generate_handle(const std::string& type_name) {
        uint64_t id = next_id_.fetch_add(1);
        return type_name + "_" + std::to_string(id);
    }
};

/**
 * @brief RAII wrapper for automatic object cleanup
 */
template<typename T>
class ObjectHandle {
private:
    std::string handle_;
    std::shared_ptr<T> object_;
    
public:
    ObjectHandle() = default;
    
    explicit ObjectHandle(const std::string& handle) 
        : handle_(handle), object_(ObjectManager::instance().get_object<T>(handle)) {}
    
    template<typename... Args>
    explicit ObjectHandle(Args&&... args) {
        handle_ = ObjectManager::instance().create_object<T>(std::forward<Args>(args)...);
        object_ = ObjectManager::instance().get_object<T>(handle_);
    }
    
    ~ObjectHandle() {
        if (!handle_.empty()) {
            ObjectManager::instance().remove_object(handle_);
        }
    }
    
    // Move semantics
    ObjectHandle(ObjectHandle&& other) noexcept 
        : handle_(std::move(other.handle_)), object_(std::move(other.object_)) {
        other.handle_.clear();
    }
    
    ObjectHandle& operator=(ObjectHandle&& other) noexcept {
        if (this != &other) {
            if (!handle_.empty()) {
                ObjectManager::instance().remove_object(handle_);
            }
            handle_ = std::move(other.handle_);
            object_ = std::move(other.object_);
            other.handle_.clear();
        }
        return *this;
    }
    
    // Non-copyable
    ObjectHandle(const ObjectHandle&) = delete;
    ObjectHandle& operator=(const ObjectHandle&) = delete;
    
    const std::string& handle() const { return handle_; }
    T* get() const { return object_.get(); }
    T& operator*() const { return *object_; }
    T* operator->() const { return object_.get(); }
    
    explicit operator bool() const { return object_ != nullptr; }
    
    void reset() {
        if (!handle_.empty()) {
            ObjectManager::instance().remove_object(handle_);
            handle_.clear();
            object_.reset();
        }
    }
};

/**
 * @brief Factory class for creating objects from Prolog
 */
class ObjectFactory {
public:
    /**
     * @brief Create a Point object
     */
    static std::string create_point(double x = 0.0, double y = 0.0) {
        return ObjectManager::instance().create_object<Point>(x, y);
    }
    
    /**
     * @brief Create a DynamicArray object
     */
    static std::string create_dynamic_array(size_t size = 0) {
        return ObjectManager::instance().create_object<DynamicArray>(size);
    }
    
    /**
     * @brief Create a Rectangle object
     */
    static std::string create_rectangle(const std::string& name, double width, double height) {
        return ObjectManager::instance().create_object<Rectangle>(name, width, height);
    }
    
    /**
     * @brief Create a Circle object
     */
    static std::string create_circle(const std::string& name, double radius) {
        return ObjectManager::instance().create_object<Circle>(name, radius);
    }
    
    /**
     * @brief Create a GraphNode object
     */
    static std::string create_graph_node(const std::string& id) {
        return ObjectManager::instance().create_object<GraphNode>(id);
    }
};

// Static member definitions
ObjectManager* ObjectManager::instance_ = nullptr;
std::mutex ObjectManager::instance_mutex_;

} // namespace cpp_bridge
