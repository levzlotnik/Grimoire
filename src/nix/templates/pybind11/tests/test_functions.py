"""
Tests for PyBind11 function bindings.

This module tests all function binding features including:
- Basic functions with various argument types
- Overloaded functions
- Default and keyword arguments
- Callback functions
- Container processing functions
- Exception handling
- Smart pointer functions
"""

import pytest
import numpy as np
from typing import List, Dict, Any, Callable

import pybind_demo
from pybind_demo import functions as funcs


class TestBasicFunctions:
    """Test basic function bindings."""
    
    def test_add_function(self, sample_integers):
        """Test basic integer addition function."""
        # Test with various integer pairs
        assert funcs.add(5, 3) == 8
        assert funcs.add(-1, 1) == 0
        assert funcs.add(0, 0) == 0
        assert funcs.add(-5, -3) == -8
        
        # Test with sample data
        for i, a in enumerate(sample_integers[:5]):
            for b in sample_integers[i:i+3]:
                result = funcs.add(a, b)
                assert result == a + b
    
    def test_add_type_conversion(self):
        """Test automatic type conversion in add function."""
        # Test explicit int conversion (C++ requires int types)
        assert funcs.add(int(5.7), int(3.2)) == 8  # Explicit conversion to integers
        assert funcs.add(5, int(3.9)) == 8
        
        # Test boolean conversion
        assert funcs.add(True, False) == 1
        assert funcs.add(True, True) == 2
    
    def test_add_error_handling(self):
        """Test error handling for invalid inputs."""
        with pytest.raises(TypeError):
            funcs.add("5", 3)
        
        with pytest.raises(TypeError):
            funcs.add(5, "3")
        
        with pytest.raises(TypeError):
            funcs.add(5, None)


class TestDefaultArguments:
    """Test functions with default arguments."""
    
    def test_greet_default(self, sample_strings):
        """Test greet function with default greeting."""
        # Default greeting
        assert funcs.greet("World") == "Hello World"
        assert funcs.greet("Alice") == "Hello Alice"
        
        # Test with sample strings
        for name in sample_strings:
            result = funcs.greet(name)
            assert result == f"Hello {name}"
    
    def test_greet_custom(self, sample_strings):
        """Test greet function with custom greeting."""
        # Custom greetings
        assert funcs.greet("World", "Hi") == "Hi World"
        assert funcs.greet("Bob", "Welcome") == "Welcome Bob"
        
        # Test with various combinations
        greetings = ["Hi", "Welcome", "Good morning", "Hey", ""]
        for name in sample_strings[:3]:
            for greeting in greetings:
                result = funcs.greet(name, greeting)
                assert result == f"{greeting} {name}"
    
    def test_greet_keyword_args(self):
        """Test greet function with keyword arguments."""
        # Positional + keyword
        assert funcs.greet("Alice", greeting="Howdy") == "Howdy Alice"
        
        # All keyword
        result = funcs.greet(name="Bob", greeting="Hey")
        assert result == "Hey Bob"
        
        # Only name as keyword (greeting uses default)
        assert funcs.greet(name="Charlie") == "Hello Charlie"


class TestKeywordArguments:
    """Test functions with keyword arguments."""
    
    def test_format_message_defaults(self):
        """Test format_message with default arguments."""
        result = funcs.format_message("test message")
        assert result == "[INFO] test message"
    
    def test_format_message_custom_prefix(self):
        """Test format_message with custom prefix."""
        result = funcs.format_message("warning", "[WARN]")
        assert result == "[WARN] warning"
        
        result = funcs.format_message("error", prefix="[ERROR]")
        assert result == "[ERROR] error"
    
    def test_format_message_uppercase(self):
        """Test format_message with uppercase option."""
        result = funcs.format_message("hello", uppercase=True)
        assert result == "[INFO] HELLO"
        
        result = funcs.format_message("world", "[DEBUG]", True)
        assert result == "[DEBUG] WORLD"
        
        result = funcs.format_message("test", prefix="[TEST]", uppercase=True)
        assert result == "[TEST] TEST"
    
    def test_format_message_combinations(self, sample_strings):
        """Test various combinations of format_message arguments."""
        prefixes = ["[INFO]", "[WARN]", "[ERROR]", "[DEBUG]", ""]
        
        for message in sample_strings[:3]:
            for prefix in prefixes[:2]:
                for uppercase in [True, False]:
                    result = funcs.format_message(message, prefix, uppercase)
                    expected = message.upper() if uppercase else message
                    assert result == f"{prefix} {expected}"


class TestOverloadedFunctions:
    """Test overloaded function bindings."""
    
    def test_multiply_doubles(self, sample_floats):
        """Test multiply function with double arguments."""
        # Basic tests
        assert funcs.multiply(2.5, 3.0) == 7.5
        assert funcs.multiply(-1.0, 5.0) == -5.0
        assert funcs.multiply(0.0, 100.0) == 0.0
        
        # Test with sample data
        for i, a in enumerate(sample_floats[:5]):
            for b in sample_floats[i:i+3]:
                if not (np.isinf(a) or np.isinf(b)):  # Skip infinity
                    result = funcs.multiply(a, b)
                    expected = a * b
                    assert abs(result - expected) < 1e-10
    
    def test_multiply_integers(self, sample_integers):
        """Test multiply function with integer arguments."""
        # Basic tests
        assert funcs.multiply(3, 4) == 12
        assert funcs.multiply(-2, 5) == -10
        assert funcs.multiply(0, 100) == 0
        
        # Test with sample data
        for i, a in enumerate(sample_integers[:5]):
            for b in sample_integers[i:i+3]:
                result = funcs.multiply(a, b)
                assert result == a * b
    
    def test_multiply_vector_scalar(self, sample_numpy_arrays):
        """Test multiply function with vector and scalar."""
        # Test with various arrays
        vector = [1.0, 2.0, 3.0, 4.0]
        scalar = 2.5
        result = funcs.multiply(vector, scalar)
        expected = [x * scalar for x in vector]
        
        assert len(result) == len(expected)
        for r, e in zip(result, expected):
            assert abs(r - e) < 1e-10
        
        # Test with NumPy array data
        for name, arr in sample_numpy_arrays.items():
            if arr.ndim == 1 and len(arr) > 0:
                vector_list = arr.tolist()
                result = funcs.multiply(vector_list, 3.0)
                expected = [x * 3.0 for x in vector_list]
                
                assert len(result) == len(expected)
                for r, e in zip(result, expected):
                    assert abs(r - e) < 1e-10
    
    def test_multiply_type_resolution(self):
        """Test that the correct overload is selected."""
        # Should call integer version
        result_int = funcs.multiply(3, 4)
        assert isinstance(result_int, int)
        assert result_int == 12
        
        # Should call double version
        result_float = funcs.multiply(3.0, 4.0)
        assert isinstance(result_float, float)
        assert result_float == 12.0
        
        # Should call vector version
        result_vec = funcs.multiply([1.0, 2.0], 3.0)
        assert isinstance(result_vec, list)
        assert result_vec == [3.0, 6.0]


class TestContainerFunctions:
    """Test functions that work with STL containers."""
    
    def test_process_list(self, sample_lists):
        """Test list processing function."""
        # Basic test
        input_list = [1, 2, 3, 4, 5]
        result = funcs.process_list(input_list)
        expected = [x*x + 1 for x in input_list]  # square each + 1
        assert result == expected
        
        # Test with sample data (only integer lists)
        for lst in sample_lists:
            if all(isinstance(x, int) for x in lst):
                result = funcs.process_list(lst)
                expected = [x*x + 1 for x in lst]
                assert result == expected
    
    def test_process_list_empty(self):
        """Test list processing with empty list."""
        result = funcs.process_list([])
        assert result == []
    
    def test_process_list_negative(self):
        """Test list processing with negative numbers."""
        input_list = [-1, -2, -3]
        result = funcs.process_list(input_list)
        expected = [2, 5, 10]  # (-1)^2+1=2, (-2)^2+1=5, (-3)^2+1=10
        assert result == expected
    
    def test_process_dict(self, sample_dicts):
        """Test dictionary processing function."""
        # Basic test
        input_dict = {"hello": 5, "world": 10}
        result = funcs.process_dict(input_dict)
        expected = {"HELLO": 10, "WORLD": 20}  # uppercase keys, double values
        assert result == expected
        
        # Test with sample data (only string key, int value dicts)
        for d in sample_dicts:
            if all(isinstance(k, str) and isinstance(v, int) for k, v in d.items()):
                result = funcs.process_dict(d)
                expected = {k.upper(): v * 2 for k, v in d.items()}
                assert result == expected
    
    def test_process_dict_empty(self):
        """Test dictionary processing with empty dict."""
        result = funcs.process_dict({})
        assert result == {}
    
    def test_process_dict_special_chars(self):
        """Test dictionary processing with special characters."""
        input_dict = {"test_key": 1, "key-with-dash": 2, "key with space": 3}
        result = funcs.process_dict(input_dict)
        expected = {"TEST_KEY": 2, "KEY-WITH-DASH": 4, "KEY WITH SPACE": 6}
        assert result == expected


class TestExceptionHandling:
    """Test exception handling in functions."""
    
    def test_divide_normal(self):
        """Test normal division operation."""
        assert funcs.divide(10, 2) == 5
        assert funcs.divide(15, 3) == 5
        assert funcs.divide(-10, 2) == -5
        assert funcs.divide(10, -2) == -5
        assert funcs.divide(0, 5) == 0
    
    def test_divide_by_zero(self):
        """Test division by zero throws exception."""
        with pytest.raises(pybind_demo.PyBindDemoException) as exc_info:
            funcs.divide(10, 0)
        
        assert "Division by zero" in str(exc_info.value)
    
    def test_divide_large_numbers(self):
        """Test division with large numbers."""
        # Should not overflow
        result = funcs.divide(1000000, 1000)
        assert result == 1000
        
        # Test integer truncation
        result = funcs.divide(10, 3)
        assert result == 3  # Integer division


class TestComplexComputations:
    """Test functions with complex return types."""
    
    def test_complex_computation_basic(self):
        """Test complex computation with basic data."""
        data = [1.0, 2.0, 3.0, 4.0, 5.0]
        threshold = 3.0
        
        filtered_data, stats = funcs.complex_computation(data, threshold)
        
        # Should filter values > threshold
        expected_filtered = [4.0, 5.0]
        assert filtered_data == expected_filtered
        
        # Stats should contain mean, min, max, count of filtered data
        assert len(stats) == 4
        assert stats[0] == 4.5  # mean of [4.0, 5.0]
        assert stats[1] == 4.0  # min
        assert stats[2] == 5.0  # max
        assert stats[3] == 2    # count
    
    def test_complex_computation_no_matches(self):
        """Test complex computation when no data matches threshold."""
        data = [1.0, 2.0, 3.0]
        threshold = 5.0
        
        filtered_data, stats = funcs.complex_computation(data, threshold)
        
        assert filtered_data == []
        assert len(stats) == 4
        assert stats[0] == 0.0  # mean of empty data
        assert stats[1] == 0.0  # min
        assert stats[2] == 0.0  # max
        assert stats[3] == 0    # count
    
    def test_complex_computation_all_match(self):
        """Test complex computation when all data matches threshold."""
        data = [5.0, 6.0, 7.0, 8.0]
        threshold = 4.0
        
        filtered_data, stats = funcs.complex_computation(data, threshold)
        
        assert filtered_data == data
        assert len(stats) == 4
        assert stats[0] == 6.5  # mean
        assert stats[1] == 5.0  # min
        assert stats[2] == 8.0  # max
        assert stats[3] == 4    # count



class TestCallbackFunctions:
    """Test functions that accept callbacks."""
    
    def test_process_with_callback_basic(self):
        """Test processing data with basic callback."""
        data = [1, 2, 3, 4, 5]
        
        # Define a simple callback
        def square_callback(x: int) -> int:
            return x * x
        
        result = funcs.process_with_callback(data, square_callback)
        expected = [1, 4, 9, 16, 25]
        assert result == expected
    
    def test_process_with_callback_lambda(self):
        """Test processing data with lambda callback."""
        data = [1, 2, 3, 4, 5]
        
        # Use lambda
        result = funcs.process_with_callback(data, lambda x: x + 10)
        expected = [11, 12, 13, 14, 15]
        assert result == expected
    
    def test_process_with_callback_complex(self):
        """Test processing data with complex callback logic."""
        data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        
        # Complex callback: square if even, cube if odd
        def complex_callback(x: int) -> int:
            if x % 2 == 0:
                return x * x
            else:
                return x * x * x
        
        result = funcs.process_with_callback(data, complex_callback)
        expected = []
        for x in data:
            if x % 2 == 0:
                expected.append(x * x)
            else:
                expected.append(x * x * x)
        
        assert result == expected
    
    def test_process_with_callback_empty(self):
        """Test processing empty data with callback."""
        data = []
        
        def dummy_callback(x: int) -> int:
            return x
        
        result = funcs.process_with_callback(data, dummy_callback)
        assert result == []
    
    def test_process_with_callback_type_errors(self):
        """Test callback with wrong return type handling."""
        data = [1, 2, 3]
        
        # Callback that returns wrong type
        def bad_callback(x: int) -> str:  # Should return int64_t, not str
            return str(x)
        
        # This should raise an error since string can't be converted to int64_t
        with pytest.raises((TypeError, RuntimeError)):
            funcs.process_with_callback(data, bad_callback)


@pytest.mark.integration
class TestFunctionIntegration:
    """Integration tests combining multiple function features."""
    
    def test_function_chaining(self):
        """Test chaining multiple function calls."""
        # Chain: add -> multiply -> format
        intermediate1 = funcs.add(5, 3)  # 8
        intermediate2 = funcs.multiply(intermediate1, 2.0)  # 16.0
        final_result = funcs.format_message(f"Result: {intermediate2}")
        
        assert final_result == "[INFO] Result: 16.0"
    
    def test_container_function_pipeline(self):
        """Test processing data through multiple container functions."""
        # Start with a list
        data = [1, 2, 3, 4, 5]
        
        # Process through list function
        processed_list = funcs.process_list(data)  # [2, 5, 10, 17, 26]
        
        # Convert to dict and process
        data_dict = {f"item_{i}": val for i, val in enumerate(processed_list)}
        processed_dict = funcs.process_dict(data_dict)
        
        # Verify final result
        expected_dict = {f"ITEM_{i}": val * 2 for i, val in enumerate(processed_list)}
        assert processed_dict == expected_dict
    
    def test_exception_recovery(self):
        """Test recovering from exceptions and continuing processing."""
        # Process multiple operations, some should fail
        operations = [
            (10, 2),   # Success: 5
            (10, 0),   # Failure: division by zero
            (15, 3),   # Success: 5
            (20, 0),   # Failure: division by zero
            (21, 7),   # Success: 3
        ]
        
        results = []
        errors = []
        
        for a, b in operations:
            try:
                result = funcs.divide(a, b)
                results.append(result)
            except pybind_demo.PyBindDemoException as e:
                errors.append(str(e))
        
        assert results == [5, 5, 3]
        assert len(errors) == 2
        assert all("Division by zero" in error for error in errors)
