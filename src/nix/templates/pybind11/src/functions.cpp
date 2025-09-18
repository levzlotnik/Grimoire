#include "../include/functions.h"
#include "../include/classes.h"
#include <stdexcept>
#include <algorithm>
#include <functional>
#include <sstream>

namespace pybind_demo {
namespace functions {

// Basic function
int add(int a, int b) {
    return a + b;
}

// Function with default arguments
std::string greet(const std::string& name, const std::string& greeting) {
    return greeting + " " + name;
}

// Overloaded functions
double multiply(double a, double b) {
    return a * b;
}

int multiply(int a, int b) {
    return a * b;
}

std::vector<double> multiply(const std::vector<double>& vec, double scalar) {
    std::vector<double> result;
    result.reserve(vec.size());
    
    for (const auto& value : vec) {
        result.push_back(value * scalar);
    }
    
    return result;
}

// Function with keyword arguments
std::string format_message(const std::string& message, 
                          const std::string& prefix, 
                          bool uppercase) {
    std::string result = prefix + " " + message;
    
    if (uppercase) {
        std::transform(result.begin(), result.end(), result.begin(), ::toupper);
    }
    
    return result;
}

// Function working with STL containers
std::vector<int> process_list(const std::vector<int>& input) {
    std::vector<int> result;
    result.reserve(input.size());
    
    for (const auto& value : input) {
        // Square each value and add 1
        result.push_back(value * value + 1);
    }
    
    return result;
}

std::unordered_map<std::string, int> process_dict(const std::unordered_map<std::string, int>& input) {
    std::unordered_map<std::string, int> result;
    
    for (const auto& [key, value] : input) {
        // Convert key to uppercase and double the value
        std::string upper_key = key;
        std::transform(upper_key.begin(), upper_key.end(), upper_key.begin(), ::toupper);
        result[upper_key] = value * 2;
    }
    
    return result;
}

// Function that can throw exceptions
int divide(int a, int b) {
    if (b == 0) {
        throw PyBindDemoException("Division by zero");
    }
    return a / b;
}

// Function with complex return type
std::pair<std::vector<double>, std::vector<double>> 
complex_computation(const std::vector<double>& data, double threshold) {
    std::vector<double> filtered_data;
    std::vector<double> stats;
    
    // Filter data above threshold
    for (const auto& value : data) {
        if (value > threshold) {
            filtered_data.push_back(value);
        }
    }
    
    // Compute statistics [mean, min, max, count]
    if (!filtered_data.empty()) {
        double sum = 0.0;
        double min_val = filtered_data[0];
        double max_val = filtered_data[0];
        
        for (const auto& value : filtered_data) {
            sum += value;
            min_val = std::min(min_val, value);
            max_val = std::max(max_val, value);
        }
        
        stats.push_back(sum / filtered_data.size());  // mean
        stats.push_back(min_val);                     // min
        stats.push_back(max_val);                     // max
        stats.push_back(static_cast<double>(filtered_data.size())); // count
    } else {
        // Empty result
        stats = {0.0, 0.0, 0.0, 0.0};
    }
    
    return std::make_pair(filtered_data, stats);
}

// Functions working with smart pointers
std::shared_ptr<std::vector<double>> create_shared_vector(size_t size, double default_value) {
    return std::make_shared<std::vector<double>>(size, default_value);
}

void modify_shared_vector(std::shared_ptr<std::vector<double>> vec, double multiplier) {
    if (vec) {
        for (auto& value : *vec) {
            value *= multiplier;
        }
    }
}

// Function with callback
std::vector<int> process_with_callback(const std::vector<int>& data, 
                                      std::function<int(int)> callback) {
    std::vector<int> result;
    result.reserve(data.size());
    
    for (const auto& value : data) {
        result.push_back(callback(value));
    }
    
    return result;
}

} // namespace functions
} // namespace pybind_demo
