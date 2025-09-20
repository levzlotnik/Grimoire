#include "../include/numpy_demo.h"
#include "../include/classes.h"
#include <algorithm>
#include <numeric>
#include <cmath>
#include <sstream>
#include <iomanip>
#include <pybind11/numpy.h>

namespace py = pybind11;

namespace pybind_demo {
namespace numpy_demo {

// Basic array operations
py::array_t<double> square_array(py::array_t<double> input) {
    py::buffer_info buf_info = input.request();
    
    // Create output array with same shape
    auto result = py::array_t<double>(buf_info.size);
    py::buffer_info result_info = result.request();
    
    double* input_ptr = static_cast<double*>(buf_info.ptr);
    double* result_ptr = static_cast<double*>(result_info.ptr);
    
    // Square each element
    for (py::ssize_t i = 0; i < buf_info.size; i++) {
        result_ptr[i] = input_ptr[i] * input_ptr[i];
    }
    
    return result;
}

py::array_t<double> add_arrays(py::array_t<double> a, py::array_t<double> b) {
    py::buffer_info buf_a = a.request();
    py::buffer_info buf_b = b.request();
    
    if (buf_a.size != buf_b.size) {
        throw PyBindDemoException("Arrays must have the same size");
    }
    
    // Create output array with same shape as first input
    auto result = py::array_t<double>(buf_a.size);
    py::buffer_info result_info = result.request();
    
    double* a_ptr = static_cast<double*>(buf_a.ptr);
    double* b_ptr = static_cast<double*>(buf_b.ptr);
    double* result_ptr = static_cast<double*>(result_info.ptr);
    
    // Add corresponding elements
    for (py::ssize_t i = 0; i < buf_a.size; i++) {
        result_ptr[i] = a_ptr[i] + b_ptr[i];
    }
    
    return result;
}

double dot_product(py::array_t<double> a, py::array_t<double> b) {
    py::buffer_info buf_a = a.request();
    py::buffer_info buf_b = b.request();
    
    if (buf_a.size != buf_b.size) {
        throw PyBindDemoException("Arrays must have the same size for dot product");
    }
    
    double* a_ptr = static_cast<double*>(buf_a.ptr);
    double* b_ptr = static_cast<double*>(buf_b.ptr);
    
    double result = 0.0;
    for (py::ssize_t i = 0; i < buf_a.size; i++) {
        result += a_ptr[i] * b_ptr[i];
    }
    
    return result;
}

// Matrix implementation
Matrix::Matrix(size_t rows, size_t cols, double fill_value) 
    : data(rows * cols, fill_value), rows(rows), cols(cols) {}

Matrix::Matrix(const std::vector<std::vector<double>>& matrix_data) {
    if (matrix_data.empty() || matrix_data[0].empty()) {
        throw PyBindDemoException("Matrix cannot be empty");
    }
    
    rows = matrix_data.size();
    cols = matrix_data[0].size();
    
    // Check that all rows have the same length
    for (const auto& row : matrix_data) {
        if (row.size() != cols) {
            throw PyBindDemoException("All matrix rows must have the same length");
        }
    }
    
    data.reserve(rows * cols);
    for (const auto& row : matrix_data) {
        data.insert(data.end(), row.begin(), row.end());
    }
}

double& Matrix::operator()(size_t row, size_t col) {
    if (row >= rows || col >= cols) {
        throw PyBindDemoException("Matrix index out of bounds");
    }
    return data[row * cols + col];
}

const double& Matrix::operator()(size_t row, size_t col) const {
    if (row >= rows || col >= cols) {
        throw PyBindDemoException("Matrix index out of bounds");
    }
    return data[row * cols + col];
}

std::vector<std::vector<double>> Matrix::to_nested_vector() const {
    std::vector<std::vector<double>> result(rows, std::vector<double>(cols));
    
    for (size_t i = 0; i < rows; ++i) {
        for (size_t j = 0; j < cols; ++j) {
            result[i][j] = (*this)(i, j);
        }
    }
    
    return result;
}

std::string Matrix::to_string() const {
    std::ostringstream oss;
    oss << std::fixed << std::setprecision(2);
    oss << "Matrix(" << rows << "x" << cols << "):\n";
    
    for (size_t i = 0; i < rows; ++i) {
        oss << "[";
        for (size_t j = 0; j < cols; ++j) {
            oss << std::setw(8) << (*this)(i, j);
            if (j < cols - 1) oss << ", ";
        }
        oss << "]\n";
    }
    
    return oss.str();
}

// Matrix operations
Matrix matrix_multiply(const Matrix& a, const Matrix& b) {
    if (a.cols != b.rows) {
        throw PyBindDemoException("Matrix dimensions incompatible for multiplication");
    }
    
    Matrix result(a.rows, b.cols, 0.0);
    
    for (size_t i = 0; i < a.rows; ++i) {
        for (size_t j = 0; j < b.cols; ++j) {
            for (size_t k = 0; k < a.cols; ++k) {
                result(i, j) += a(i, k) * b(k, j);
            }
        }
    }
    
    return result;
}

Matrix matrix_transpose(const Matrix& matrix) {
    Matrix result(matrix.cols, matrix.rows);
    
    for (size_t i = 0; i < matrix.rows; ++i) {
        for (size_t j = 0; j < matrix.cols; ++j) {
            result(j, i) = matrix(i, j);
        }
    }
    
    return result;
}

Matrix matrix_add(const Matrix& a, const Matrix& b) {
    if (a.rows != b.rows || a.cols != b.cols) {
        throw PyBindDemoException("Matrix dimensions must match for addition");
    }
    
    Matrix result(a.rows, a.cols);
    
    for (size_t i = 0; i < a.rows; ++i) {
        for (size_t j = 0; j < a.cols; ++j) {
            result(i, j) = a(i, j) + b(i, j);
        }
    }
    
    return result;
}

std::vector<double> matrix_vector_multiply(const Matrix& matrix, const std::vector<double>& vector) {
    if (matrix.cols != vector.size()) {
        throw PyBindDemoException("Matrix columns must match vector size");
    }
    
    std::vector<double> result(matrix.rows, 0.0);
    
    for (size_t i = 0; i < matrix.rows; ++i) {
        for (size_t j = 0; j < matrix.cols; ++j) {
            result[i] += matrix(i, j) * vector[j];
        }
    }
    
    return result;
}

// Statistical operations
double mean(const std::vector<double>& data) {
    if (data.empty()) {
        return 0.0;  // Return 0 for empty array as expected by tests
    }
    return std::accumulate(data.begin(), data.end(), 0.0) / data.size();
}

double variance(const std::vector<double>& data) {
    if (data.empty()) {
        throw PyBindDemoException("Cannot compute variance of empty array");
    }
    
    double mean_val = mean(data);
    double variance_sum = 0.0;
    
    for (const auto& value : data) {
        double diff = value - mean_val;
        variance_sum += diff * diff;
    }
    
    return variance_sum / data.size();
}

double standard_deviation(const std::vector<double>& data) {
    return std::sqrt(variance(data));
}

std::vector<double> normalize(const std::vector<double>& data) {
    if (data.empty()) {
        return data;
    }
    
    double mean_val = mean(data);
    double std_val = standard_deviation(data);
    
    if (std_val == 0.0) {
        // Return zeros for constant arrays as expected by tests
        return std::vector<double>(data.size(), 0.0);
    }
    
    std::vector<double> result;
    result.reserve(data.size());
    
    for (const auto& value : data) {
        result.push_back((value - mean_val) / std_val);
    }
    
    return result;
}

// Signal processing operations
std::vector<double> moving_average(const std::vector<double>& data, size_t window_size) {
    if (window_size == 0 || window_size > data.size()) {
        throw PyBindDemoException("Invalid window size for moving average");
    }
    
    std::vector<double> result;
    result.reserve(data.size() - window_size + 1);
    
    for (size_t i = 0; i <= data.size() - window_size; ++i) {
        double sum = 0.0;
        for (size_t j = i; j < i + window_size; ++j) {
            sum += data[j];
        }
        result.push_back(sum / window_size);
    }
    
    return result;
}

std::vector<double> diff(const std::vector<double>& data) {
    if (data.size() < 2) {
        return {};
    }
    
    std::vector<double> result;
    result.reserve(data.size() - 1);
    
    for (size_t i = 1; i < data.size(); ++i) {
        result.push_back(data[i] - data[i-1]);
    }
    
    return result;
}

std::vector<double> cumsum(const std::vector<double>& data) {
    std::vector<double> result;
    result.reserve(data.size());
    
    double sum = 0.0;
    for (const auto& value : data) {
        sum += value;
        result.push_back(sum);
    }
    
    return result;
}

// Advanced operations
py::array_t<double> apply_function(py::array_t<double> input, py::function func) {
    py::buffer_info buf_info = input.request();
    
    // Create output array with same shape
    auto result = py::array_t<double>(buf_info.size);
    py::buffer_info result_info = result.request();
    
    double* input_ptr = static_cast<double*>(buf_info.ptr);
    double* result_ptr = static_cast<double*>(result_info.ptr);
    
    // Apply function to each element
    for (py::ssize_t i = 0; i < buf_info.size; i++) {
        py::object py_result = func(input_ptr[i]);
        result_ptr[i] = py_result.cast<double>();
    }
    
    return result;
}

py::array_t<double> element_wise_operation(py::array_t<double> a, 
                                          py::array_t<double> b,
                                          py::function op) {
    py::buffer_info buf_a = a.request();
    py::buffer_info buf_b = b.request();
    
    if (buf_a.size != buf_b.size) {
        throw PyBindDemoException("Arrays must have the same size for element-wise operation");
    }
    
    // Create output array with same shape as first input
    auto result = py::array_t<double>(buf_a.size);
    py::buffer_info result_info = result.request();
    
    double* a_ptr = static_cast<double*>(buf_a.ptr);
    double* b_ptr = static_cast<double*>(buf_b.ptr);
    double* result_ptr = static_cast<double*>(result_info.ptr);
    
    // Apply operation to corresponding elements
    for (py::ssize_t i = 0; i < buf_a.size; i++) {
        py::object py_result = op(a_ptr[i], b_ptr[i]);
        result_ptr[i] = py_result.cast<double>();
    }
    
    return result;
}

// Image implementation
Image::Image(size_t width, size_t height, size_t channels) 
    : width(width), height(height), channels(channels) {
    if (width == 0 || height == 0 || channels == 0) {
        throw PyBindDemoException("Image dimensions must be positive");
    }
    pixels.resize(width * height * channels, 0);
}

unsigned char& Image::pixel(size_t x, size_t y, size_t channel) {
    if (x >= width || y >= height || channel >= channels) {
        throw PyBindDemoException("Image pixel coordinates out of bounds");
    }
    return pixels[(y * width + x) * channels + channel];
}

const unsigned char& Image::pixel(size_t x, size_t y, size_t channel) const {
    if (x >= width || y >= height || channel >= channels) {
        throw PyBindDemoException("Image pixel coordinates out of bounds");
    }
    return pixels[(y * width + x) * channels + channel];
}

std::vector<double> Image::to_double_array() const {
    std::vector<double> result;
    result.reserve(pixels.size());
    
    for (const auto& pixel : pixels) {
        result.push_back(static_cast<double>(pixel) / 255.0);
    }
    
    return result;
}

void Image::from_double_array(const std::vector<double>& data) {
    if (data.size() != pixels.size()) {
        throw PyBindDemoException("Data size must match image size");
    }
    
    for (size_t i = 0; i < data.size(); ++i) {
        double clamped = std::max(0.0, std::min(1.0, data[i]));
        pixels[i] = static_cast<unsigned char>(clamped * 255.0);
    }
}

// Image operations (simplified implementations)
Image gaussian_blur(const Image& image, double sigma) {
    // Simplified 3x3 gaussian blur
    Image result = image;
    
    if (sigma <= 0.0) {
        return result;
    }
    
    // Simple 3x3 kernel approximation
    const double kernel[9] = {1, 2, 1, 2, 4, 2, 1, 2, 1};
    const double kernel_sum = 16.0;
    
    for (size_t y = 1; y < image.height - 1; ++y) {
        for (size_t x = 1; x < image.width - 1; ++x) {
            for (size_t c = 0; c < image.channels; ++c) {
                double sum = 0.0;
                int k = 0;
                
                for (int dy = -1; dy <= 1; ++dy) {
                    for (int dx = -1; dx <= 1; ++dx) {
                        sum += image.pixel(x + dx, y + dy, c) * kernel[k++];
                    }
                }
                
                result.pixel(x, y, c) = static_cast<unsigned char>(sum / kernel_sum);
            }
        }
    }
    
    return result;
}

Image edge_detection(const Image& image) {
    // Simplified Sobel edge detection
    Image result = image;
    
    const int sobel_x[9] = {-1, 0, 1, -2, 0, 2, -1, 0, 1};
    const int sobel_y[9] = {-1, -2, -1, 0, 0, 0, 1, 2, 1};
    
    for (size_t y = 1; y < image.height - 1; ++y) {
        for (size_t x = 1; x < image.width - 1; ++x) {
            for (size_t c = 0; c < image.channels; ++c) {
                double gx = 0.0, gy = 0.0;
                int k = 0;
                
                for (int dy = -1; dy <= 1; ++dy) {
                    for (int dx = -1; dx <= 1; ++dx) {
                        double pixel_val = image.pixel(x + dx, y + dy, c);
                        gx += pixel_val * sobel_x[k];
                        gy += pixel_val * sobel_y[k];
                        k++;
                    }
                }
                
                double magnitude = std::sqrt(gx * gx + gy * gy);
                result.pixel(x, y, c) = static_cast<unsigned char>(std::min(255.0, magnitude));
            }
        }
    }
    
    return result;
}

Image resize(const Image& image, size_t new_width, size_t new_height) {
    // Simple nearest-neighbor resize
    Image result(new_width, new_height, image.channels);
    
    double x_scale = static_cast<double>(image.width) / new_width;
    double y_scale = static_cast<double>(image.height) / new_height;
    
    for (size_t y = 0; y < new_height; ++y) {
        for (size_t x = 0; x < new_width; ++x) {
            size_t src_x = static_cast<size_t>(x * x_scale);
            size_t src_y = static_cast<size_t>(y * y_scale);
            
            // Clamp to source image bounds
            src_x = std::min(src_x, image.width - 1);
            src_y = std::min(src_y, image.height - 1);
            
            for (size_t c = 0; c < image.channels; ++c) {
                result.pixel(x, y, c) = image.pixel(src_x, src_y, c);
            }
        }
    }
    
    return result;
}

} // namespace numpy_demo
} // namespace pybind_demo
