#pragma once

#include <string>
#include <vector>
#include <unordered_map>
#include <memory>

namespace pybind_demo {

/**
 * Function demonstrations for PyBind11
 */
namespace functions {

// Basic function
int add(int a, int b);

// Function with default arguments
std::string greet(const std::string& name, const std::string& greeting = "Hello");

// Overloaded functions
double multiply(double a, double b);
int multiply(int a, int b);
std::vector<double> multiply(const std::vector<double>& vec, double scalar);

// Function with keyword arguments
std::string format_message(const std::string& message, 
                          const std::string& prefix = "[INFO]", 
                          bool uppercase = false);

// Function working with STL containers
std::vector<int> process_list(const std::vector<int>& input);
std::unordered_map<std::string, int> process_dict(const std::unordered_map<std::string, int>& input);

// Function that can throw exceptions
int divide(int a, int b);

// Function with complex return type
std::pair<std::vector<double>, std::vector<double>> 
complex_computation(const std::vector<double>& data, double threshold);

// Functions working with smart pointers
std::shared_ptr<std::vector<double>> create_shared_vector(size_t size, double default_value = 0.0);
void modify_shared_vector(std::shared_ptr<std::vector<double>> vec, double multiplier);

// Function with callback
std::vector<int> process_with_callback(const std::vector<int>& data, 
                                      std::function<int(int)> callback);

} // namespace functions
} // namespace pybind_demo
