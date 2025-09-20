#pragma once

#include <string>
#include <vector>
#include <unordered_map>
#include <memory>
#include <functional>

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
std::vector<int64_t> process_list(const std::vector<int64_t>& input);
std::unordered_map<std::string, int> process_dict(const std::unordered_map<std::string, int>& input);

// Function that can throw exceptions
int divide(int a, int b);

// Function with complex return type
std::pair<std::vector<double>, std::vector<double>> 
complex_computation(const std::vector<double>& data, double threshold);

// Functions working with smart pointers
std::vector<double> create_vector(size_t size, double default_value = 0.0);
void modify_vector_inplace(std::vector<double>& vec, double multiplier);

// Function with callback
std::vector<int64_t> process_with_callback(const std::vector<int64_t>& data, 
                                          std::function<int64_t(int64_t)> callback);


} // namespace functions
} // namespace pybind_demo
