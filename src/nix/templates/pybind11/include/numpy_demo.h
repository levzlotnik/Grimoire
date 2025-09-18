#pragma once

#include <vector>
#include <string>

namespace pybind_demo {
namespace numpy_demo {

/**
 * Functions demonstrating NumPy integration with PyBind11
 */

// Basic array operations
std::vector<double> square_array(const std::vector<double>& input);
std::vector<double> add_arrays(const std::vector<double>& a, const std::vector<double>& b);
double dot_product(const std::vector<double>& a, const std::vector<double>& b);

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
std::vector<double> apply_function(const std::vector<double>& input, 
                                  double (*func)(double));
std::vector<double> element_wise_operation(const std::vector<double>& a, 
                                          const std::vector<double>& b,
                                          double (*op)(double, double));

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
