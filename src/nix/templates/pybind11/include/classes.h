#pragma once

#include <string>
#include <vector>
#include <memory>
#include <iostream>

namespace pybind_demo {

/**
 * Custom exception for demonstration
 */
class PyBindDemoException : public std::exception {
private:
    std::string message_;

public:
    explicit PyBindDemoException(const std::string& message) : message_(message) {}
    const char* what() const noexcept override { return message_.c_str(); }
};

/**
 * Simple class demonstrating basic features
 */
class Calculator {
private:
    double value_;
    std::string name_;

public:
    // Constructors
    Calculator();
    Calculator(double initial_value);
    Calculator(double initial_value, const std::string& name);
    
    // Copy constructor and assignment
    Calculator(const Calculator& other);
    Calculator& operator=(const Calculator& other);
    
    // Destructor
    ~Calculator();

    // Basic methods
    double get_value() const;
    void set_value(double value);
    
    const std::string& get_name() const;
    void set_name(const std::string& name);
    
    // Mathematical operations (return self for chaining)
    Calculator& add(double value);
    Calculator& subtract(double value);
    Calculator& multiply(double value);
    Calculator& divide(double value);  // Can throw PyBindDemoException
    
    // Operator overloading
    Calculator operator+(const Calculator& other) const;
    Calculator operator-(const Calculator& other) const;
    Calculator operator*(double scalar) const;
    bool operator==(const Calculator& other) const;
    bool operator!=(const Calculator& other) const;
    
    // Static methods
    static Calculator create_named(const std::string& name, double value = 0.0);
    static double pi();
    
    // Method with vector operations
    std::vector<double> apply_to_vector(const std::vector<double>& values) const;
    
    // String representation
    std::string to_string() const;
    
    // Friend function for stream output
    friend std::ostream& operator<<(std::ostream& os, const Calculator& calc);
};

/**
 * Abstract base class demonstrating inheritance and virtual methods
 */
class Shape {
protected:
    std::string name_;
    
public:
    explicit Shape(const std::string& name) : name_(name) {}
    virtual ~Shape() = default;
    
    const std::string& get_name() const { return name_; }
    
    // Pure virtual methods
    virtual double area() const = 0;
    virtual double perimeter() const = 0;
    
    // Virtual method with default implementation
    virtual std::string description() const {
        return "A shape named " + name_;
    }
    
    // Static factory method
    static std::shared_ptr<Shape> create_rectangle(double width, double height, const std::string& name = "rectangle");
    static std::shared_ptr<Shape> create_circle(double radius, const std::string& name = "circle");
};

/**
 * Concrete class inheriting from Shape
 */
class Rectangle : public Shape {
private:
    double width_;
    double height_;
    
public:
    Rectangle(double width, double height, const std::string& name = "rectangle");
    
    double get_width() const { return width_; }
    double get_height() const { return height_; }
    void set_width(double width);
    void set_height(double height);
    
    // Override virtual methods
    double area() const override;
    double perimeter() const override;
    std::string description() const override;
};

/**
 * Another concrete class inheriting from Shape
 */
class Circle : public Shape {
private:
    double radius_;
    
public:
    Circle(double radius, const std::string& name = "circle");
    
    double get_radius() const { return radius_; }
    void set_radius(double radius);
    
    // Override virtual methods
    double area() const override;
    double perimeter() const override;
    std::string description() const override;
};

/**
 * Class demonstrating property-like access
 */
class DataContainer {
private:
    std::vector<double> data_;
    std::string label_;
    bool read_only_;
    
public:
    DataContainer();
    explicit DataContainer(const std::vector<double>& initial_data);
    
    // Property-like getters and setters
    const std::vector<double>& get_data() const;
    void set_data(const std::vector<double>& data);
    
    const std::string& get_label() const;
    void set_label(const std::string& label);
    
    bool is_read_only() const;
    void set_read_only(bool read_only);
    
    // Data manipulation methods
    void append(double value);
    void extend(const std::vector<double>& values);
    void clear();
    size_t size() const;
    bool empty() const;
    
    // Statistical methods
    double mean() const;
    double sum() const;
    double min() const;
    double max() const;
    
    // Indexing support
    double& operator[](size_t index);
    const double& operator[](size_t index) const;
    
    // Iterator support for range-based loops
    std::vector<double>::iterator begin() { return data_.begin(); }
    std::vector<double>::iterator end() { return data_.end(); }
    std::vector<double>::const_iterator begin() const { return data_.begin(); }
    std::vector<double>::const_iterator end() const { return data_.end(); }
};

/**
 * Class demonstrating smart pointer usage
 */
class ResourceManager {
private:
    std::shared_ptr<std::vector<double>> shared_data_;
    std::unique_ptr<std::string> unique_name_;
    
public:
    ResourceManager();
    explicit ResourceManager(const std::string& name);
    
    // Smart pointer methods
    std::shared_ptr<std::vector<double>> get_shared_data() const;
    void set_shared_data(std::shared_ptr<std::vector<double>> data);
    
    const std::string& get_name() const;
    void set_name(const std::string& name);
    
    // Resource operations
    void create_shared_resource(size_t size, double default_value = 0.0);
    bool has_shared_resource() const;
    size_t shared_resource_use_count() const;
};

} // namespace pybind_demo