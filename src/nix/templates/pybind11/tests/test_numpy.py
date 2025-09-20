"""
Tests for PyBind11 NumPy integration.

This module tests NumPy integration features including:
- Array operations using buffer protocol
- Matrix class with linear algebra operations
- Vectorized operations for performance
- Image processing operations
- Statistical computations
- Zero-copy array access patterns
"""

import pytest
import numpy as np
from typing import List, Callable

import pybind_demo
from pybind_demo import numpy_demo


@pytest.mark.numpy
class TestBasicArrayOperations:
    """Test basic NumPy array operations."""
    
    def test_square_array(self, sample_numpy_arrays, float_comparison):
        """Test squaring array elements."""
        # Test with 1D arrays
        for name, arr in sample_numpy_arrays.items():
            if arr.ndim == 1:
                result = numpy_demo.square_array(arr)
                expected = arr ** 2
                float_comparison.assert_array_close(result, expected)
    
    def test_square_array_types(self):
        """Test square_array with different array types."""
        # Test different dtypes
        arrays = [
            np.array([1, 2, 3], dtype=np.int32),
            np.array([1.0, 2.0, 3.0], dtype=np.float32),
            np.array([1.0, 2.0, 3.0], dtype=np.float64),
        ]
        
        for arr in arrays:
            result = numpy_demo.square_array(arr)
            expected = arr.astype(np.float64) ** 2
            np.testing.assert_allclose(result, expected)
    
    def test_square_array_empty(self):
        """Test square_array with empty array."""
        empty_arr = np.array([], dtype=np.float64)
        result = numpy_demo.square_array(empty_arr)
        assert len(result) == 0
        assert result.dtype == np.float64
    
    def test_square_array_special_values(self):
        """Test square_array with special floating point values."""
        special_arr = np.array([0.0, -0.0, 1.0, -1.0, np.inf, -np.inf])
        result = numpy_demo.square_array(special_arr)
        
        assert result[0] == 0.0
        assert result[1] == 0.0
        assert result[2] == 1.0
        assert result[3] == 1.0
        assert np.isinf(result[4])
        assert np.isinf(result[5])
    
    def test_add_arrays(self, sample_numpy_arrays, float_comparison):
        """Test adding two arrays."""
        # Test with compatible arrays
        arr1 = np.array([1.0, 2.0, 3.0, 4.0])
        arr2 = np.array([5.0, 6.0, 7.0, 8.0])
        
        result = numpy_demo.add_arrays(arr1, arr2)
        expected = arr1 + arr2
        float_comparison.assert_array_close(result, expected)
    
    def test_add_arrays_broadcasting(self):
        """Test array addition with broadcasting."""
        # Test same-size arrays
        arr1 = np.array([1.0, 2.0, 3.0])
        arr2 = np.array([4.0, 5.0, 6.0])
        
        result = numpy_demo.add_arrays(arr1, arr2)
        expected = np.array([5.0, 7.0, 9.0])
        np.testing.assert_allclose(result, expected)
    
    def test_add_arrays_size_mismatch(self):
        """Test array addition with size mismatch."""
        arr1 = np.array([1.0, 2.0, 3.0])
        arr2 = np.array([4.0, 5.0])
        
        # Should raise an exception for incompatible sizes
        with pytest.raises(pybind_demo.PyBindDemoException):
            numpy_demo.add_arrays(arr1, arr2)
    
    def test_dot_product(self, float_comparison):
        """Test dot product calculation."""
        # Test basic dot product
        arr1 = np.array([1.0, 2.0, 3.0])
        arr2 = np.array([4.0, 5.0, 6.0])
        
        result = numpy_demo.dot_product(arr1, arr2)
        expected = np.dot(arr1, arr2)  # 1*4 + 2*5 + 3*6 = 32
        float_comparison.assert_close(result, expected)
    
    def test_dot_product_orthogonal(self):
        """Test dot product of orthogonal vectors."""
        arr1 = np.array([1.0, 0.0, 0.0])
        arr2 = np.array([0.0, 1.0, 0.0])
        
        result = numpy_demo.dot_product(arr1, arr2)
        assert abs(result) < 1e-10  # Should be zero
    
    def test_dot_product_size_mismatch(self):
        """Test dot product with size mismatch."""
        arr1 = np.array([1.0, 2.0, 3.0])
        arr2 = np.array([4.0, 5.0])
        
        with pytest.raises(pybind_demo.PyBindDemoException):
            numpy_demo.dot_product(arr1, arr2)


@pytest.mark.numpy
class TestMatrixClass:
    """Test Matrix class functionality."""
    
    def test_matrix_creation_size(self):
        """Test matrix creation with size and fill value."""
        matrix = numpy_demo.Matrix(3, 4, 2.5)
        
        assert matrix.rows == 3
        assert matrix.cols == 4
        assert len(matrix.data) == 12  # 3 * 4
        assert all(abs(x - 2.5) < 1e-10 for x in matrix.data)
    
    def test_matrix_creation_default_fill(self):
        """Test matrix creation with default fill value."""
        matrix = numpy_demo.Matrix(2, 3)
        
        assert matrix.rows == 2
        assert matrix.cols == 3
        assert len(matrix.data) == 6
        assert all(x == 0.0 for x in matrix.data)
    
    def test_matrix_creation_from_nested_list(self, sample_matrices):
        """Test matrix creation from nested lists."""
        for name, matrix_data in sample_matrices.items():
            matrix = numpy_demo.Matrix(matrix_data)
            
            assert matrix.rows == len(matrix_data)
            assert matrix.cols == len(matrix_data[0]) if matrix_data else 0
            
            # Verify data
            nested = matrix.to_nested_vector()
            assert nested == matrix_data
    
    def test_matrix_element_access(self):
        """Test matrix element access."""
        matrix_data = [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]
        matrix = numpy_demo.Matrix(matrix_data)
        
        # Test element access
        assert matrix[0, 0] == 1.0
        assert matrix[0, 1] == 2.0
        assert matrix[0, 2] == 3.0
        assert matrix[1, 0] == 4.0
        assert matrix[1, 1] == 5.0
        assert matrix[1, 2] == 6.0
    
    def test_matrix_element_modification(self):
        """Test matrix element modification."""
        matrix = numpy_demo.Matrix(2, 2, 0.0)
        
        # Modify elements
        matrix[0, 0] = 10.0
        matrix[0, 1] = 20.0
        matrix[1, 0] = 30.0
        matrix[1, 1] = 40.0
        
        # Verify modifications
        assert matrix[0, 0] == 10.0
        assert matrix[0, 1] == 20.0
        assert matrix[1, 0] == 30.0
        assert matrix[1, 1] == 40.0
    
    def test_matrix_bounds_checking(self):
        """Test matrix bounds checking."""
        matrix = numpy_demo.Matrix(2, 3, 1.0)
        
        # Valid accesses should work
        _ = matrix[0, 0]
        _ = matrix[1, 2]
        
        # Out of bounds should raise exceptions
        with pytest.raises(pybind_demo.PyBindDemoException):
            _ = matrix[2, 0]  # Row out of bounds
        
        with pytest.raises(pybind_demo.PyBindDemoException):
            _ = matrix[0, 3]  # Column out of bounds
        
        with pytest.raises(pybind_demo.PyBindDemoException):
            matrix[2, 0] = 1.0  # Assignment out of bounds
    
    def test_matrix_to_nested_vector(self):
        """Test conversion to nested vector."""
        matrix_data = [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]]
        matrix = numpy_demo.Matrix(matrix_data)
        
        result = matrix.to_nested_vector()
        assert result == matrix_data
    
    def test_matrix_string_representation(self):
        """Test matrix string representation."""
        matrix_data = [[1.0, 2.0], [3.0, 4.0]]
        matrix = numpy_demo.Matrix(matrix_data)
        
        str_repr = matrix.to_string()
        assert "1" in str_repr
        assert "2" in str_repr
        assert "3" in str_repr
        assert "4" in str_repr
        
        # Test __str__ and __repr__
        assert str(matrix) == str_repr
        assert repr(matrix) == str_repr


@pytest.mark.numpy
class TestMatrixOperations:
    """Test matrix linear algebra operations."""
    
    def test_matrix_multiply(self, float_comparison):
        """Test matrix multiplication."""
        # Test compatible matrices
        a_data = [[1.0, 2.0], [3.0, 4.0]]
        b_data = [[5.0, 6.0], [7.0, 8.0]]
        
        a = numpy_demo.Matrix(a_data)
        b = numpy_demo.Matrix(b_data)
        
        result = numpy_demo.matrix_multiply(a, b)
        
        # Expected: [[1*5+2*7, 1*6+2*8], [3*5+4*7, 3*6+4*8]] = [[19, 22], [43, 50]]
        expected_data = [[19.0, 22.0], [43.0, 50.0]]
        result_data = result.to_nested_vector()
        
        for i in range(len(expected_data)):
            for j in range(len(expected_data[i])):
                float_comparison.assert_close(result_data[i][j], expected_data[i][j])
    
    def test_matrix_multiply_dimensions(self):
        """Test matrix multiplication with different dimensions."""
        # 2x3 * 3x2 = 2x2
        a_data = [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]
        b_data = [[7.0, 8.0], [9.0, 10.0], [11.0, 12.0]]
        
        a = numpy_demo.Matrix(a_data)
        b = numpy_demo.Matrix(b_data)
        
        result = numpy_demo.matrix_multiply(a, b)
        
        assert result.rows == 2
        assert result.cols == 2
        
        # Expected: [[1*7+2*9+3*11, 1*8+2*10+3*12], [4*7+5*9+6*11, 4*8+5*10+6*12]]
        #         = [[58, 64], [139, 154]]
        assert abs(result[0, 0] - 58.0) < 1e-10
        assert abs(result[0, 1] - 64.0) < 1e-10
        assert abs(result[1, 0] - 139.0) < 1e-10
        assert abs(result[1, 1] - 154.0) < 1e-10
    
    def test_matrix_multiply_incompatible(self):
        """Test matrix multiplication with incompatible dimensions."""
        a = numpy_demo.Matrix(2, 3, 1.0)  # 2x3
        b = numpy_demo.Matrix(2, 2, 1.0)  # 2x2 (incompatible: 3 != 2)
        
        with pytest.raises(pybind_demo.PyBindDemoException):
            numpy_demo.matrix_multiply(a, b)
    
    def test_matrix_transpose(self):
        """Test matrix transpose."""
        matrix_data = [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]
        matrix = numpy_demo.Matrix(matrix_data)
        
        result = numpy_demo.matrix_transpose(matrix)
        
        assert result.rows == 3
        assert result.cols == 2
        
        # Expected transpose: [[1, 4], [2, 5], [3, 6]]
        assert result[0, 0] == 1.0
        assert result[0, 1] == 4.0
        assert result[1, 0] == 2.0
        assert result[1, 1] == 5.0
        assert result[2, 0] == 3.0
        assert result[2, 1] == 6.0
    
    def test_matrix_transpose_square(self):
        """Test transpose of square matrix."""
        matrix_data = [[1.0, 2.0], [3.0, 4.0]]
        matrix = numpy_demo.Matrix(matrix_data)
        
        result = numpy_demo.matrix_transpose(matrix)
        
        assert result.rows == 2
        assert result.cols == 2
        
        # Expected: [[1, 3], [2, 4]]
        expected_data = [[1.0, 3.0], [2.0, 4.0]]
        result_data = result.to_nested_vector()
        assert result_data == expected_data
    
    def test_matrix_add(self, float_comparison):
        """Test matrix addition."""
        a_data = [[1.0, 2.0], [3.0, 4.0]]
        b_data = [[5.0, 6.0], [7.0, 8.0]]
        
        a = numpy_demo.Matrix(a_data)
        b = numpy_demo.Matrix(b_data)
        
        result = numpy_demo.matrix_add(a, b)
        
        # Expected: [[6, 8], [10, 12]]
        expected_data = [[6.0, 8.0], [10.0, 12.0]]
        result_data = result.to_nested_vector()
        
        for i in range(len(expected_data)):
            for j in range(len(expected_data[i])):
                float_comparison.assert_close(result_data[i][j], expected_data[i][j])
    
    def test_matrix_add_incompatible(self):
        """Test matrix addition with incompatible dimensions."""
        a = numpy_demo.Matrix(2, 3, 1.0)
        b = numpy_demo.Matrix(3, 2, 1.0)
        
        with pytest.raises(pybind_demo.PyBindDemoException):
            numpy_demo.matrix_add(a, b)
    
    def test_matrix_vector_multiply(self, float_comparison):
        """Test matrix-vector multiplication."""
        matrix_data = [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]
        vector = np.array([7.0, 8.0, 9.0])
        
        matrix = numpy_demo.Matrix(matrix_data)
        result = numpy_demo.matrix_vector_multiply(matrix, vector)
        
        # Expected: [1*7+2*8+3*9, 4*7+5*8+6*9] = [50, 122]
        expected = np.array([50.0, 122.0])
        float_comparison.assert_array_close(result, expected)
    
    def test_matrix_vector_multiply_incompatible(self):
        """Test matrix-vector multiplication with incompatible dimensions."""
        matrix = numpy_demo.Matrix(2, 3, 1.0)  # 2x3 matrix
        vector = np.array([1.0, 2.0])          # 2-element vector (incompatible)
        
        with pytest.raises(pybind_demo.PyBindDemoException):
            numpy_demo.matrix_vector_multiply(matrix, vector)


@pytest.mark.numpy
class TestStatisticalOperations:
    """Test statistical operations on arrays."""
    
    def test_mean(self, sample_numpy_arrays, float_comparison):
        """Test mean calculation."""
        # Test with known data
        data = np.array([1.0, 2.0, 3.0, 4.0, 5.0])
        result = numpy_demo.mean(data)
        expected = np.mean(data)
        float_comparison.assert_close(result, expected)
        
        # Test with sample arrays
        for name, arr in sample_numpy_arrays.items():
            if arr.ndim == 1 and len(arr) > 0:
                result = numpy_demo.mean(arr)
                expected = np.mean(arr)
                float_comparison.assert_close(result, expected)
    
    def test_mean_empty(self):
        """Test mean of empty array."""
        empty_arr = np.array([], dtype=np.float64)
        result = numpy_demo.mean(empty_arr)
        assert result == 0.0  # Or could be NaN, depends on implementation
    
    def test_variance(self, float_comparison):
        """Test variance calculation."""
        data = np.array([1.0, 2.0, 3.0, 4.0, 5.0])
        result = numpy_demo.variance(data)
        expected = np.var(data, ddof=0)  # Population variance
        float_comparison.assert_close(result, expected)
    
    def test_variance_constant(self):
        """Test variance of constant array."""
        data = np.array([5.0, 5.0, 5.0, 5.0])
        result = numpy_demo.variance(data)
        assert abs(result) < 1e-10  # Should be zero
    
    def test_standard_deviation(self, float_comparison):
        """Test standard deviation calculation."""
        data = np.array([1.0, 2.0, 3.0, 4.0, 5.0])
        result = numpy_demo.standard_deviation(data)
        expected = np.std(data, ddof=0)
        float_comparison.assert_close(result, expected)
    
    def test_normalize(self, float_comparison):
        """Test data normalization."""
        data = np.array([1.0, 2.0, 3.0, 4.0, 5.0])
        result = numpy_demo.normalize(data)
        
        # Normalized data should have mean ~0 and std ~1
        result_mean = np.mean(result)
        result_std = np.std(result, ddof=0)
        
        float_comparison.assert_close(result_mean, 0.0, atol=1e-10)
        float_comparison.assert_close(result_std, 1.0)
    
    def test_normalize_constant(self):
        """Test normalization of constant array."""
        data = np.array([5.0, 5.0, 5.0, 5.0])
        result = numpy_demo.normalize(data)
        
        # Constant array should normalize to zeros (or handle gracefully)
        assert all(abs(x) < 1e-10 for x in result) or all(np.isnan(x) for x in result)


@pytest.mark.numpy
class TestSignalProcessing:
    """Test signal processing operations."""
    
    def test_moving_average(self, float_comparison):
        """Test moving average calculation."""
        data = np.array([1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0])
        window_size = 3
        
        result = numpy_demo.moving_average(data, window_size)
        
        # Expected moving average for window_size=3:
        # [2.0, 3.0, 4.0, 5.0, 6.0, 7.0] (first 3 elements: (1+2+3)/3=2, etc.)
        expected = np.array([2.0, 3.0, 4.0, 5.0, 6.0, 7.0])
        float_comparison.assert_array_close(result, expected)
    
    def test_moving_average_edge_cases(self):
        """Test moving average edge cases."""
        # Window size equal to array length
        data = np.array([1.0, 2.0, 3.0])
        result = numpy_demo.moving_average(data, 3)
        expected = np.array([2.0])  # Single value: mean of all
        np.testing.assert_allclose(result, expected)
        
        # Window size larger than array
        with pytest.raises(pybind_demo.PyBindDemoException):
            numpy_demo.moving_average(data, 5)
    
    def test_diff(self, float_comparison):
        """Test difference calculation."""
        data = np.array([1.0, 3.0, 6.0, 10.0, 15.0])
        result = numpy_demo.diff(data)
        
        # Expected differences: [2.0, 3.0, 4.0, 5.0]
        expected = np.array([2.0, 3.0, 4.0, 5.0])
        float_comparison.assert_array_close(result, expected)
    
    def test_diff_constant(self):
        """Test difference of constant array."""
        data = np.array([5.0, 5.0, 5.0, 5.0])
        result = numpy_demo.diff(data)
        
        # Differences should all be zero
        expected = np.array([0.0, 0.0, 0.0])
        np.testing.assert_allclose(result, expected)
    
    def test_cumsum(self, float_comparison):
        """Test cumulative sum calculation."""
        data = np.array([1.0, 2.0, 3.0, 4.0, 5.0])
        result = numpy_demo.cumsum(data)
        
        # Expected cumulative sum: [1.0, 3.0, 6.0, 10.0, 15.0]
        expected = np.array([1.0, 3.0, 6.0, 10.0, 15.0])
        float_comparison.assert_array_close(result, expected)
    
    def test_cumsum_negative(self):
        """Test cumulative sum with negative numbers."""
        data = np.array([1.0, -2.0, 3.0, -4.0, 5.0])
        result = numpy_demo.cumsum(data)
        
        # Expected: [1.0, -1.0, 2.0, -2.0, 3.0]
        expected = np.array([1.0, -1.0, 2.0, -2.0, 3.0])
        np.testing.assert_allclose(result, expected)


@pytest.mark.numpy
class TestAdvancedOperations:
    """Test advanced array operations."""
    
    def test_apply_function(self):
        """Test applying function to each element."""
        data = np.array([1.0, 2.0, 3.0, 4.0])
        
        # Define a simple function
        def square_plus_one(x: float) -> float:
            return x * x + 1.0
        
        result = numpy_demo.apply_function(data, square_plus_one)
        expected = np.array([2.0, 5.0, 10.0, 17.0])
        np.testing.assert_allclose(result, expected)
    
    def test_apply_function_lambda(self):
        """Test applying lambda function."""
        data = np.array([1.0, 2.0, 3.0, 4.0])
        
        result = numpy_demo.apply_function(data, lambda x: x * 2.0)
        expected = data * 2.0
        np.testing.assert_allclose(result, expected)
    
    def test_apply_function_complex(self):
        """Test applying complex function."""
        data = np.array([0.0, 1.0, 2.0, 3.0])
        
        # Apply sine function
        import math
        result = numpy_demo.apply_function(data, math.sin)
        expected = np.sin(data)
        np.testing.assert_allclose(result, expected, atol=1e-10)
    
    def test_element_wise_operation(self):
        """Test element-wise binary operations."""
        a = np.array([1.0, 2.0, 3.0, 4.0])
        b = np.array([5.0, 6.0, 7.0, 8.0])
        
        # Define operations
        def add_op(x: float, y: float) -> float:
            return x + y
        
        def multiply_op(x: float, y: float) -> float:
            return x * y
        
        # Test addition
        result_add = numpy_demo.element_wise_operation(a, b, add_op)
        expected_add = a + b
        np.testing.assert_allclose(result_add, expected_add)
        
        # Test multiplication
        result_mul = numpy_demo.element_wise_operation(a, b, multiply_op)
        expected_mul = a * b
        np.testing.assert_allclose(result_mul, expected_mul)
    
    def test_element_wise_operation_size_mismatch(self):
        """Test element-wise operation with size mismatch."""
        a = np.array([1.0, 2.0, 3.0])
        b = np.array([4.0, 5.0])
        
        def dummy_op(x: float, y: float) -> float:
            return x + y
        
        with pytest.raises(pybind_demo.PyBindDemoException):
            numpy_demo.element_wise_operation(a, b, dummy_op)


@pytest.mark.numpy
class TestImageClass:
    """Test Image class for image processing demonstrations."""
    
    def test_image_creation(self):
        """Test image creation."""
        img = numpy_demo.Image(4, 3, 1)  # 4x3 grayscale
        
        assert img.width == 4
        assert img.height == 3
        assert img.channels == 1
        assert len(img.pixels) == 12  # 4 * 3 * 1
    
    def test_image_creation_rgb(self):
        """Test RGB image creation."""
        img = numpy_demo.Image(2, 2, 3)  # 2x2 RGB
        
        assert img.width == 2
        assert img.height == 2
        assert img.channels == 3
        assert len(img.pixels) == 12  # 2 * 2 * 3
    
    def test_image_pixel_access(self):
        """Test pixel access in images."""
        img = numpy_demo.Image(3, 2, 1)
        
        # Set some pixel values
        img.pixel[0, 0, 0] = 100
        img.pixel[1, 0, 0] = 150
        img.pixel[2, 1, 0] = 200
        
        # Read back values
        assert img.pixel[0, 0, 0] == 100
        assert img.pixel[1, 0, 0] == 150
        assert img.pixel[2, 1, 0] == 200
    
    def test_image_pixel_access_rgb(self):
        """Test pixel access in RGB images."""
        img = numpy_demo.Image(2, 2, 3)
        
        # Set RGB values for one pixel
        img.pixel[0, 0, 0] = 255  # Red
        img.pixel[0, 0, 1] = 128  # Green
        img.pixel[0, 0, 2] = 64   # Blue
        
        # Read back
        assert img.pixel[0, 0, 0] == 255
        assert img.pixel[0, 0, 1] == 128
        assert img.pixel[0, 0, 2] == 64
    
    def test_image_pixel_bounds(self):
        """Test pixel access bounds checking."""
        img = numpy_demo.Image(2, 2, 1)
        
        # Valid access
        img.pixel[0, 0] = 100
        assert img.pixel[0, 0] == 100
        
        # Out of bounds
        with pytest.raises(pybind_demo.PyBindDemoException):
            _ = img.pixel[2, 0]  # x out of bounds
        
        with pytest.raises(pybind_demo.PyBindDemoException):
            _ = img.pixel[0, 2]  # y out of bounds
        
        with pytest.raises(pybind_demo.PyBindDemoException):
            _ = img.pixel[0, 0, 1]  # channel out of bounds for grayscale
    
    def test_image_to_double_array(self):
        """Test conversion to double array."""
        img = numpy_demo.Image(2, 2, 1)
        
        # Set some values
        img.pixel[0, 0] = 0
        img.pixel[1, 0] = 128
        img.pixel[0, 1] = 255
        img.pixel[1, 1] = 64
        
        # Convert to double array
        double_array = img.to_double_array()
        
        # Should be normalized to [0, 1]
        expected = [0.0, 128.0/255.0, 255.0/255.0, 64.0/255.0]
        np.testing.assert_allclose(double_array, expected, atol=1e-6)
    
    def test_image_from_double_array(self):
        """Test loading from double array."""
        img = numpy_demo.Image(2, 2, 1)
        
        # Create double array [0, 1]
        double_data = [0.0, 0.5, 1.0, 0.25]
        img.from_double_array(double_data)
        
        # Check converted values
        assert img.pixel[0, 0] == 0
        assert img.pixel[1, 0] == int(0.5 * 255)
        assert img.pixel[0, 1] == 255
        assert img.pixel[1, 1] == int(0.25 * 255)


@pytest.mark.numpy
class TestImageProcessing:
    """Test image processing operations."""
    
    def test_gaussian_blur(self):
        """Test Gaussian blur operation."""
        # Create a simple test image
        img = numpy_demo.Image(5, 5, 1)
        
        # Set center pixel to max value
        img.pixel[2, 2] = 255
        
        # Apply blur
        blurred = numpy_demo.gaussian_blur(img, 1.0)
        
        # Blurred image should have spread the intensity
        assert blurred.width == img.width
        assert blurred.height == img.height
        assert blurred.channels == img.channels
        
        # Center should still be bright but not necessarily max
        center_value = blurred.pixel[2, 2]
        assert center_value > 0
        
        # Neighbors should have some intensity too
        neighbor_value = blurred.pixel[1, 2]
        assert neighbor_value > 0
        assert neighbor_value < center_value
    
    def test_edge_detection(self):
        """Test edge detection operation."""
        # Create image with a vertical edge
        img = numpy_demo.Image(4, 4, 1)
        
        # Left half black, right half white
        for y in range(4):
            for x in range(2):
                img.pixel[x, y] = 0
            for x in range(2, 4):
                img.pixel[x, y] = 255
        
        # Apply edge detection
        edges = numpy_demo.edge_detection(img)
        
        assert edges.width == img.width
        assert edges.height == img.height
        assert edges.channels == img.channels
        
        # Should detect the vertical edge
        # Edge pixels should have high intensity
        edge_intensity = edges.pixel[1, 1]  # Near the edge
        background_intensity = edges.pixel[0, 0]  # Far from edge
        
        # Edge should be more intense than background
        # (exact values depend on implementation)
        assert edge_intensity >= background_intensity
    
    def test_resize(self):
        """Test image resize operation."""
        # Create original image
        img = numpy_demo.Image(4, 4, 1)
        
        # Fill with pattern
        for y in range(4):
            for x in range(4):
                img.pixel[x, y] = (x + y) * 30
        
        # Resize to different dimensions
        resized = numpy_demo.resize(img, 2, 6)  # Make it 2x6
        
        assert resized.width == 2
        assert resized.height == 6
        assert resized.channels == img.channels
        
        # Resized image should have some reasonable values
        # (exact values depend on interpolation method)
        for y in range(6):
            for x in range(2):
                value = resized.pixel[x, y]
                assert 0 <= value <= 255


@pytest.mark.integration
@pytest.mark.numpy
class TestNumpyIntegration:
    """Integration tests combining multiple NumPy features."""
    
    def test_matrix_numpy_interoperability(self):
        """Test Matrix class working with NumPy arrays."""
        # Create matrix from NumPy array
        np_array = np.array([[1.0, 2.0], [3.0, 4.0]])
        matrix = numpy_demo.Matrix(np_array.tolist())
        
        # Use matrix in calculations
        vector = np.array([1.0, 1.0])
        result = numpy_demo.matrix_vector_multiply(matrix, vector)
        
        # Compare with NumPy
        expected = np.dot(np_array, vector)
        np.testing.assert_allclose(result, expected)
    
    def test_statistical_processing_pipeline(self):
        """Test a complete statistical processing pipeline."""
        # Generate test data
        data = np.array([1.0, 4.0, 2.0, 8.0, 5.0, 7.0, 3.0, 6.0])
        
        # 1. Calculate basic statistics
        mean_val = numpy_demo.mean(data)
        var_val = numpy_demo.variance(data)
        std_val = numpy_demo.standard_deviation(data)
        
        # 2. Normalize the data
        normalized = numpy_demo.normalize(data)
        
        # 3. Apply transformations
        squared = numpy_demo.square_array(normalized)
        
        # 4. Calculate moving average
        smoothed = numpy_demo.moving_average(squared, 3)
        
        # Verify the pipeline produces reasonable results
        assert isinstance(mean_val, float)
        assert isinstance(var_val, float)
        assert isinstance(std_val, float)
        assert len(normalized) == len(data)
        assert len(squared) == len(data)
        assert len(smoothed) == len(data) - 2  # Window size 3
        
        # Normalized data should have mean ~0, std ~1
        assert abs(np.mean(normalized)) < 1e-10
        assert abs(np.std(normalized, ddof=0) - 1.0) < 1e-10
    
    def test_image_matrix_processing(self):
        """Test processing image data through matrix operations."""
        # Create test image
        img = numpy_demo.Image(3, 3, 1)
        
        # Fill with gradient pattern
        for y in range(3):
            for x in range(3):
                img.pixel[x, y] = (x + y) * 30
        
        # Convert to matrix via double array
        double_data = img.to_double_array()
        matrix_data = [double_data[i:i+3] for i in range(0, 9, 3)]
        matrix = numpy_demo.Matrix(matrix_data)
        
        # Apply matrix transformation (transpose)
        transposed = numpy_demo.matrix_transpose(matrix)
        
        # Convert back to image
        result_img = numpy_demo.Image(3, 3, 1)
        flat_data = []
        for i in range(3):
            for j in range(3):
                flat_data.append(transposed[i, j])
        
        result_img.from_double_array(flat_data)
        
        # Verify transformation
        assert result_img.width == img.width
        assert result_img.height == img.height
        
        # Check that transpose worked
        for x in range(3):
            for y in range(3):
                original_value = img.pixel[x, y]
                transposed_value = result_img.pixel[y, x]
                assert abs(original_value - transposed_value) <= 1  # Allow for rounding
