#pragma once

#include "cpp_bridge.hpp"
#include <vector>
#include <string>
#include <memory>
#include <algorithm>
#include <numeric>
#include <cmath>
#include <map>

/**
 * @file data_structures.hpp
 * @brief Example C++ data structures exposed to Prolog
 * 
 * Demonstrates various patterns for exposing C++ classes to Prolog:
 * - Simple value objects
 * - Container-like objects
 * - Objects with complex behavior
 * - Inheritance hierarchies
 */

namespace cpp_bridge {

/**
 * @brief Simple point class demonstrating basic blob integration
 */
class Point : public TypedBlob<Point> {
private:
    double x_, y_;
    
public:
    Point(double x = 0.0, double y = 0.0) : x_(x), y_(y) {}
    
    // Required for TypedBlob
    static constexpr const char* cpp_type_name() { return "point"; }
    
    // Accessors
    double x() const { return x_; }
    double y() const { return y_; }
    void set_x(double x) { x_ = x; }
    void set_y(double y) { y_ = y; }
    
    // Operations
    double distance_to(const Point& other) const {
        double dx = x_ - other.x_;
        double dy = y_ - other.y_;
        return std::sqrt(dx * dx + dy * dy);
    }
    
    Point operator+(const Point& other) const {
        return Point(x_ + other.x_, y_ + other.y_);
    }
    
    Point& operator+=(const Point& other) {
        x_ += other.x_;
        y_ += other.y_;
        return *this;
    }
    
    // ManagedObject interface
    std::string to_string() const override {
        return "point(" + std::to_string(x_) + ", " + std::to_string(y_) + ")";
    }
    
    std::unique_ptr<ManagedObject> clone() const override {
        return std::make_unique<Point>(x_, y_);
    }
};

/**
 * @brief Dynamic array class demonstrating container integration
 */
class DynamicArray : public TypedBlob<DynamicArray> {
private:
    std::vector<double> data_;
    
public:
    DynamicArray() = default;
    explicit DynamicArray(size_t size) : data_(size, 0.0) {}
    DynamicArray(std::initializer_list<double> init) : data_(init) {}
    
    // Required for TypedBlob
    static constexpr const char* cpp_type_name() { return "dynamic_array"; }
    
    // Container operations
    size_t size() const { return data_.size(); }
    bool empty() const { return data_.empty(); }
    void clear() { data_.clear(); }
    
    void push_back(double value) { data_.push_back(value); }
    void pop_back() { 
        if (!data_.empty()) data_.pop_back(); 
    }
    
    double& at(size_t index) {
        if (index >= data_.size()) {
            throw BridgeException("Index out of bounds: " + std::to_string(index));
        }
        return data_[index];
    }
    
    const double& at(size_t index) const {
        if (index >= data_.size()) {
            throw BridgeException("Index out of bounds: " + std::to_string(index));
        }
        return data_[index];
    }
    
    void resize(size_t new_size) { data_.resize(new_size); }
    
    // Mathematical operations
    double sum() const {
        return std::accumulate(data_.begin(), data_.end(), 0.0);
    }
    
    double mean() const {
        if (data_.empty()) return 0.0;
        return sum() / data_.size();
    }
    
    double min() const {
        if (data_.empty()) return 0.0;
        return *std::min_element(data_.begin(), data_.end());
    }
    
    double max() const {
        if (data_.empty()) return 0.0;
        return *std::max_element(data_.begin(), data_.end());
    }
    
    void sort() {
        std::sort(data_.begin(), data_.end());
    }
    
    void reverse() {
        std::reverse(data_.begin(), data_.end());
    }
    
    // Conversion to/from Prolog lists
    std::vector<double> to_vector() const { return data_; }
    void from_vector(const std::vector<double>& vec) { data_ = vec; }
    
    // ManagedObject interface
    std::string to_string() const override {
        std::string result = "dynamic_array([";
        for (size_t i = 0; i < data_.size(); ++i) {
            if (i > 0) result += ", ";
            result += std::to_string(data_[i]);
        }
        result += "])";
        return result;
    }
    
    std::unique_ptr<ManagedObject> clone() const override {
        auto copy = std::make_unique<DynamicArray>();
        copy->data_ = data_;
        return copy;
    }
};

/**
 * @brief Base class for demonstrating inheritance
 */
class Shape : public TypedBlob<Shape> {
protected:
    std::string name_;
    
public:
    explicit Shape(const std::string& name) : name_(name) {}
    virtual ~Shape() = default;
    
    // Required for TypedBlob
    static constexpr const char* cpp_type_name() { return "shape"; }
    
    const std::string& name() const { return name_; }
    void set_name(const std::string& name) { name_ = name; }
    
    // Virtual methods
    virtual double area() const = 0;
    virtual double perimeter() const = 0;
    virtual std::string shape_type() const = 0;
    
    // ManagedObject interface
    std::string to_string() const override {
        return shape_type() + "(" + name_ + ")";
    }
};

/**
 * @brief Rectangle class demonstrating inheritance
 */
class Rectangle : public Shape {
private:
    double width_, height_;
    
public:
    Rectangle(const std::string& name, double width, double height)
        : Shape(name), width_(width), height_(height) {}
    
    // Required for TypedBlob (overrides Shape's type)
    static constexpr const char* cpp_type_name() { return "rectangle"; }
    std::string get_type_name() const override { return "rectangle"; }
    
    double width() const { return width_; }
    double height() const { return height_; }
    void set_width(double width) { width_ = width; }
    void set_height(double height) { height_ = height; }
    
    // Shape interface
    double area() const override { return width_ * height_; }
    double perimeter() const override { return 2 * (width_ + height_); }
    std::string shape_type() const override { return "rectangle"; }
    
    // Additional methods
    bool is_square() const { 
        return std::abs(width_ - height_) < 1e-9; 
    }
    
    double diagonal() const {
        return std::sqrt(width_ * width_ + height_ * height_);
    }
    
    // ManagedObject interface
    std::unique_ptr<ManagedObject> clone() const override {
        return std::make_unique<Rectangle>(name_, width_, height_);
    }
};

/**
 * @brief Circle class demonstrating inheritance
 */
class Circle : public Shape {
private:
    double radius_;
    static constexpr double PI = 3.14159265358979323846;
    
public:
    Circle(const std::string& name, double radius)
        : Shape(name), radius_(radius) {}
    
    // Required for TypedBlob (overrides Shape's type)
    static constexpr const char* cpp_type_name() { return "circle"; }
    std::string get_type_name() const override { return "circle"; }
    
    double radius() const { return radius_; }
    void set_radius(double radius) { radius_ = radius; }
    
    // Shape interface
    double area() const override { return PI * radius_ * radius_; }
    double perimeter() const override { return 2 * PI * radius_; }
    std::string shape_type() const override { return "circle"; }
    
    // Additional methods
    double diameter() const { return 2 * radius_; }
    double circumference() const { return perimeter(); }
    
    // ManagedObject interface
    std::unique_ptr<ManagedObject> clone() const override {
        return std::make_unique<Circle>(name_, radius_);
    }
};

/**
 * @brief Complex class demonstrating advanced features
 */
class GraphNode : public TypedBlob<GraphNode> {
private:
    std::string id_;
    std::map<std::string, std::string> properties_;
    std::vector<std::shared_ptr<GraphNode>> connections_;
    
public:
    explicit GraphNode(const std::string& id) : id_(id) {}
    
    // Required for TypedBlob
    static constexpr const char* cpp_type_name() { return "graph_node"; }
    
    const std::string& id() const { return id_; }
    
    // Property management
    void set_property(const std::string& key, const std::string& value) {
        properties_[key] = value;
    }
    
    std::string get_property(const std::string& key) const {
        auto it = properties_.find(key);
        return it != properties_.end() ? it->second : "";
    }
    
    bool has_property(const std::string& key) const {
        return properties_.find(key) != properties_.end();
    }
    
    void remove_property(const std::string& key) {
        properties_.erase(key);
    }
    
    std::map<std::string, std::string> get_all_properties() const {
        return properties_;
    }
    
    // Connection management
    void add_connection(std::shared_ptr<GraphNode> node) {
        if (node && node.get() != this) {
            connections_.push_back(node);
        }
    }
    
    void remove_connection(const std::string& node_id) {
        connections_.erase(
            std::remove_if(connections_.begin(), connections_.end(),
                          [&node_id](const std::weak_ptr<GraphNode>& weak_node) {
                              auto node = weak_node.lock();
                              return !node || node->id() == node_id;
                          }),
            connections_.end());
    }
    
    std::vector<std::string> get_connection_ids() const {
        std::vector<std::string> ids;
        for (const auto& conn : connections_) {
            if (conn) {
                ids.push_back(conn->id());
            }
        }
        return ids;
    }
    
    size_t connection_count() const {
        return connections_.size();
    }
    
    bool is_connected_to(const std::string& node_id) const {
        return std::any_of(connections_.begin(), connections_.end(),
                          [&node_id](const std::shared_ptr<GraphNode>& node) {
                              return node && node->id() == node_id;
                          });
    }
    
    // ManagedObject interface
    std::string to_string() const override {
        std::string result = "graph_node(" + id_ + ", properties: {";
        bool first = true;
        for (const auto& [key, value] : properties_) {
            if (!first) result += ", ";
            result += key + ": " + value;
            first = false;
        }
        result += "}, connections: [";
        first = true;
        for (const auto& conn : connections_) {
            if (conn) {
                if (!first) result += ", ";
                result += conn->id();
                first = false;
            }
        }
        result += "])";
        return result;
    }
    
    std::unique_ptr<ManagedObject> clone() const override {
        auto copy = std::make_unique<GraphNode>(id_);
        copy->properties_ = properties_;
        // Note: connections are not deep-copied to avoid circular references
        return copy;
    }
};

} // namespace cpp_bridge