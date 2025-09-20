#pragma once

#include <vector>
#include <string>
#include <pybind11/numpy.h>

namespace py = pybind11;

namespace pybind_demo {
namespace numpy_demo {

/**
 * Functions demonstrating NumPy integration with PyBind11
 */

// Basic array operations
py::array_t<double> square_array(py::array_t<double> input);
py::array_t<double> add_arrays(py::array_t<double> a, py::array_t<double> b);
double dot_product(py::array_t<double> a, py::array_t<double> b);

// Matrix operations (represented as vectors with width/height)
struct Matrix {
    std::vector<double> data;
    size_t rows;
    size_t cols;
    
    Matrix(size_t rows, size_t cols, double fill_value = 0.0);
    Matrix(const std::vector<std::vector<double>>& matrix_data);
    
    double& operator()(size_t row, size_t col);
    const double& operator()(size_t row, size_t col) const;
    
    std::vector<std::vector<double>> to_nested_vector() const;
    std::string to_string() const;
};

// Matrix operations
Matrix matrix_multiply(const Matrix& a, const Matrix& b);
Matrix matrix_transpose(const Matrix& matrix);
Matrix matrix_add(const Matrix& a, const Matrix& b);
std::vector<double> matrix_vector_multiply(const Matrix& matrix, const std::vector<double>& vector);

// Statistical operations
double mean(const std::vector<double>& data);
double variance(const std::vector<double>& data);
double standard_deviation(const std::vector<double>& data);
std::vector<double> normalize(const std::vector<double>& data);

// Signal processing operations
std::vector<double> moving_average(const std::vector<double>& data, size_t window_size);
std::vector<double> diff(const std::vector<double>& data);
std::vector<double> cumsum(const std::vector<double>& data);

// Advanced operations that would benefit from NumPy buffer protocol
py::array_t<double> apply_function(py::array_t<double> input, 
                                  py::function func);
py::array_t<double> element_wise_operation(py::array_t<double> a, 
                                          py::array_t<double> b,
                                          py::function op);

// Image processing simulation (treating images as 1D arrays)
struct Image {
    std::vector<unsigned char> pixels;
    size_t width;
    size_t height;
    size_t channels;  // 1 for grayscale, 3 for RGB
    
    Image(size_t width, size_t height, size_t channels = 1);
    
    unsigned char& pixel(size_t x, size_t y, size_t channel = 0);
    const unsigned char& pixel(size_t x, size_t y, size_t channel = 0) const;
    
    std::vector<double> to_double_array() const;
    void from_double_array(const std::vector<double>& data);
};

// Image operations
Image gaussian_blur(const Image& image, double sigma);
Image edge_detection(const Image& image);
Image resize(const Image& image, size_t new_width, size_t new_height);

} // namespace numpy_demo
} // namespace pybind_demo
