"""
Tests for PyBind11 exception handling.

This module tests exception handling features including:
- C++ → Python exception mapping
- Custom exception classes
- Exception translation and propagation
- Error recovery and cleanup
- Exception safety guarantees
"""

import pytest
import sys
from typing import Any, Callable

import numpy as np
import pybind_demo
from pybind_demo import functions as funcs


class TestStandardExceptions:
    """Test standard C++ to Python exception mapping."""
    
    def test_division_by_zero_exception(self):
        """Test division by zero throws custom exception."""
        with pytest.raises(pybind_demo.PyBindDemoException) as exc_info:
            funcs.divide(10, 0)
        
        # Check exception details
        exception = exc_info.value
        assert "Division by zero" in str(exception)
        assert isinstance(exception, pybind_demo.PyBindDemoException)
    
    def test_calculator_division_by_zero(self):
        """Test Calculator division by zero exception."""
        calc = pybind_demo.Calculator(42.0)
        
        with pytest.raises(pybind_demo.PyBindDemoException) as exc_info:
            calc.divide(0.0)
        
        # Calculator state should be unchanged after exception
        assert calc.value == 42.0
        assert "Division by zero" in str(exc_info.value)
    
    def test_exception_inheritance(self):
        """Test that custom exceptions inherit from appropriate base classes."""
        try:
            funcs.divide(1, 0)
        except pybind_demo.PyBindDemoException as e:
            # Should be instance of standard Python exception hierarchy
            assert isinstance(e, Exception)
            # Should have standard exception attributes
            assert hasattr(e, 'args')
            assert len(e.args) > 0
    
    def test_exception_message_content(self):
        """Test exception message content and formatting."""
        test_cases = [
            (10, 0, "Division by zero"),
            (42, 0, "Division by zero"),
            (-5, 0, "Division by zero"),
        ]
        
        for numerator, denominator, expected_msg in test_cases:
            with pytest.raises(pybind_demo.PyBindDemoException) as exc_info:
                funcs.divide(numerator, denominator)
            
            assert expected_msg in str(exc_info.value)
    
    def test_exception_context_information(self):
        """Test that exceptions contain useful context information."""
        with pytest.raises(pybind_demo.PyBindDemoException) as exc_info:
            funcs.divide(100, 0)
        
        exception = exc_info.value
        error_msg = str(exception)
        
        # Should contain descriptive error message
        assert len(error_msg) > 0
        assert "Division by zero" in error_msg
        
        # Exception should have proper type name
        assert type(exception).__name__ == "PyBindDemoException"


class TestExceptionPropagation:
    """Test exception propagation through various call patterns."""
    
    def test_exception_in_method_chain(self):
        """Test exception during method chaining."""
        calc = pybind_demo.Calculator(10.0)
        
        # Should work up to the point of exception
        with pytest.raises(pybind_demo.PyBindDemoException):
            calc.add(5.0).multiply(2.0).divide(0.0).subtract(1.0)
        
        # Calculator should be in state after last successful operation
        # add(5) -> 15, multiply(2) -> 30, then divide(0) fails
        assert calc.value == 30.0
    
    def test_exception_in_nested_calls(self):
        """Test exception propagation through nested function calls."""
        # Create scenario where exception occurs in nested context
        def nested_calculation():
            return funcs.divide(funcs.add(5, 3), funcs.add(2, -2))
        
        with pytest.raises(pybind_demo.PyBindDemoException):
            nested_calculation()
    
    def test_exception_with_container_operations(self):
        """Test exceptions during container processing."""
        # This tests if exceptions properly propagate through STL conversions
        data = [1, 2, 3, 4, 5]
        
        # Define callback that throws for certain values
        def problematic_callback(x: int) -> int:
            if x == 3:
                # This will cause divide by zero in our test
                return funcs.divide(10, 0)
            return x * 2
        
        with pytest.raises(pybind_demo.PyBindDemoException):
            funcs.process_with_callback(data, problematic_callback)
    
    def test_exception_in_operator_overloading(self):
        """Test exceptions in operator overloaded methods."""
        calc1 = pybind_demo.Calculator(10.0)
        calc2 = pybind_demo.Calculator(0.0)
        
        # Division operator that might cause issues (if implemented)
        # For now, test other operators that might have validation
        try:
            result = calc1 + calc2  # Should work
            assert result.value == 10.0
        except pybind_demo.PyBindDemoException:
            # If operator+ has validation that fails, that's also valid
            pass
    
    def test_exception_recovery(self):
        """Test that operations can continue after exceptions."""
        calc = pybind_demo.Calculator(20.0)
        
        # First operation should fail
        with pytest.raises(pybind_demo.PyBindDemoException):
            calc.divide(0.0)
        
        # Calculator should still be usable
        assert calc.value == 20.0
        
        # Subsequent operations should work
        calc.add(5.0)
        assert calc.value == 25.0
        
        calc.multiply(2.0)
        assert calc.value == 50.0


class TestExceptionSafety:
    """Test exception safety guarantees."""
    
    def test_object_state_after_exception(self):
        """Test that object state is consistent after exceptions."""
        calc = pybind_demo.Calculator(100.0, "test_calc")
        original_value = calc.value
        original_name = calc.name
        
        # Exception should not corrupt object state
        with pytest.raises(pybind_demo.PyBindDemoException):
            calc.divide(0.0)
        
        # Object should be in valid state
        assert calc.value == original_value
        assert calc.name == original_name
        
        # Object should still be fully functional
        calc.add(10.0)
        assert calc.value == 110.0
    
    def test_resource_cleanup_after_exception(self):
        """Test that resources are properly cleaned up after exceptions."""
        # Create resource manager
        manager = pybind_demo.ResourceManager("test_manager")
        manager.create_shared_resource(10, 5.0)
        
        # Verify resource exists
        assert manager.has_shared_resource()
        initial_count = manager.shared_resource_use_count()
        
        # Exception in some operation shouldn't leak resources
        try:
            # Perform operation that might throw
            calc = pybind_demo.Calculator(10.0)
            calc.divide(0.0)  # This will throw
        except pybind_demo.PyBindDemoException:
            pass
        
        # Resource manager should still be in valid state
        assert manager.has_shared_resource()
        # Reference count should be stable (no leaks)
        assert manager.shared_resource_use_count() == initial_count
    
    def test_container_state_after_exception(self):
        """Test container state consistency after exceptions."""
        container = pybind_demo.DataContainer([1.0, 2.0, 3.0, 4.0])
        original_size = container.size()
        original_data = list(container.data)
        
        # Try to perform operation that might modify container and throw
        try:
            # If there were a method that could throw during modification,
            # test it here. For now, test exception in statistical methods
            if container.empty():  # This shouldn't be true, but if it were...
                pass
        except pybind_demo.PyBindDemoException:
            pass
        
        # Container should maintain its state
        assert container.size() == original_size
        assert list(container.data) == original_data
    
    def test_multiple_objects_after_exception(self):
        """Test that exceptions in one object don't affect others."""
        calc1 = pybind_demo.Calculator(10.0, "calc1")
        calc2 = pybind_demo.Calculator(20.0, "calc2")
        
        # Exception in calc1 shouldn't affect calc2
        with pytest.raises(pybind_demo.PyBindDemoException):
            calc1.divide(0.0)
        
        # calc2 should be unaffected
        assert calc2.value == 20.0
        assert calc2.name == "calc2"
        
        # calc2 should still work normally
        calc2.multiply(2.0)
        assert calc2.value == 40.0


class TestExceptionTypes:
    """Test different types of exceptions and their handling."""
    
    def test_runtime_errors(self):
        """Test runtime error conditions."""
        # Division by zero is a runtime error
        with pytest.raises(pybind_demo.PyBindDemoException):
            funcs.divide(1, 0)
    
    def test_invalid_argument_errors(self):
        """Test invalid argument handling."""
        # Test with invalid arguments to various functions
        calc = pybind_demo.Calculator()
        
        # Most functions should handle type conversion automatically,
        # but test edge cases that might cause issues
        try:
            # Very large numbers that might cause overflow
            calc.value = 1e100
            calc.multiply(1e100)  # Might overflow
        except (pybind_demo.PyBindDemoException, OverflowError):
            # Either custom exception or Python's OverflowError is acceptable
            pass
    
    def test_out_of_range_errors(self):
        """Test out of range access errors."""
        container = pybind_demo.DataContainer([1.0, 2.0, 3.0])
        
        # Out of bounds access should raise appropriate exception
        with pytest.raises((IndexError, pybind_demo.PyBindDemoException)):
            _ = container[10]
        
        with pytest.raises((IndexError, pybind_demo.PyBindDemoException)):
            container[10] = 5.0
    
    def test_type_conversion_errors(self):
        """Test errors during type conversion."""
        # Test functions that expect specific types
        try:
            # These should raise TypeError for wrong types
            funcs.process_list("not a list")
        except TypeError:
            pass  # Expected
        except pybind_demo.PyBindDemoException:
            pass  # Also acceptable if wrapped
        
        try:
            funcs.process_dict("not a dict")
        except TypeError:
            pass  # Expected
        except pybind_demo.PyBindDemoException:
            pass  # Also acceptable if wrapped


class TestExceptionInteroperability:
    """Test exception interoperability with Python exception handling."""
    
    def test_exception_in_try_except(self):
        """Test that C++ exceptions work properly in try/except blocks."""
        results = []
        
        test_cases = [(10, 2), (10, 0), (15, 3), (20, 0), (8, 4)]
        
        for a, b in test_cases:
            try:
                result = funcs.divide(a, b)
                results.append(result)
            except pybind_demo.PyBindDemoException as e:
                results.append(f"Error: {e}")
        
        # Should have successful results and error messages
        expected_results = [5, "Error: Division by zero", 5, "Error: Division by zero", 2]
        assert len(results) == len(expected_results)
        
        for actual, expected in zip(results, expected_results):
            if isinstance(expected, str):
                assert isinstance(actual, str)
                assert "Error:" in actual
                assert "Division by zero" in actual
            else:
                assert actual == expected
    
    def test_exception_with_finally(self):
        """Test that finally blocks execute properly with C++ exceptions."""
        cleanup_called = False
        
        def test_with_finally():
            nonlocal cleanup_called
            try:
                funcs.divide(10, 0)
            except pybind_demo.PyBindDemoException:
                pass
            finally:
                cleanup_called = True
        
        test_with_finally()
        assert cleanup_called
    
    def test_exception_re_raising(self):
        """Test re-raising C++ exceptions."""
        def inner_function():
            funcs.divide(5, 0)
        
        def outer_function():
            try:
                inner_function()
            except pybind_demo.PyBindDemoException:
                # Re-raise the exception
                raise
        
        with pytest.raises(pybind_demo.PyBindDemoException):
            outer_function()
    
    def test_exception_chaining(self):
        """Test exception chaining with C++ exceptions."""
        try:
            try:
                funcs.divide(10, 0)
            except pybind_demo.PyBindDemoException as e:
                # Raise a new exception that chains with the original
                raise ValueError("Processing failed") from e
        except ValueError as e:
            # Should have both the new exception and the original cause
            assert str(e) == "Processing failed"
            assert isinstance(e.__cause__, pybind_demo.PyBindDemoException)
            assert "Division by zero" in str(e.__cause__)
    
    def test_exception_with_context_managers(self):
        """Test exceptions work properly with context managers."""
        class TestContextManager:
            def __init__(self):
                self.entered = False
                self.exited = False
                self.exception_occurred = False
            
            def __enter__(self):
                self.entered = True
                return self
            
            def __exit__(self, exc_type, exc_val, exc_tb):
                self.exited = True
                if exc_type is not None:
                    self.exception_occurred = True
                return False  # Don't suppress the exception
        
        context_manager = TestContextManager()
        
        with pytest.raises(pybind_demo.PyBindDemoException):
            with context_manager:
                funcs.divide(1, 0)
        
        assert context_manager.entered
        assert context_manager.exited
        assert context_manager.exception_occurred


class TestExceptionPerformance:
    """Test performance aspects of exception handling."""
    
    def test_exception_overhead(self):
        """Test that exception handling doesn't add significant overhead to normal operations."""
        import time
        
        # Measure normal operations
        start_time = time.time()
        for i in range(1000):
            result = funcs.divide(100, 2)
        normal_time = time.time() - start_time
        
        # Measure operations with exception handling
        start_time = time.time()
        for i in range(1000):
            try:
                result = funcs.divide(100, 2)
            except pybind_demo.PyBindDemoException:
                pass
        exception_handling_time = time.time() - start_time
        
        # Exception handling shouldn't add significant overhead
        # when no exceptions are thrown
        overhead_ratio = exception_handling_time / normal_time
        assert overhead_ratio < 2.0  # Less than 2x slower
    
    @pytest.mark.slow
    def test_many_exceptions_performance(self):
        """Test performance when many exceptions are thrown."""
        import time
        
        # Measure time to handle many exceptions
        start_time = time.time()
        exception_count = 0
        
        for i in range(1000):
            try:
                funcs.divide(10, 0)
            except pybind_demo.PyBindDemoException:
                exception_count += 1
        
        elapsed_time = time.time() - start_time
        
        # Should have caught all exceptions
        assert exception_count == 1000
        
        # Should complete in reasonable time (less than 1 second)
        assert elapsed_time < 1.0


@pytest.mark.integration
class TestExceptionIntegration:
    """Integration tests for exception handling with other features."""
    
    def test_exceptions_with_containers(self):
        """Test exception handling combined with container operations."""
        # Create scenario where container operations might fail
        problematic_data = []
        
        # Process containers with potential for exceptions
        try:
            result = funcs.process_list(problematic_data)
            assert result == []  # Empty list should work
        except pybind_demo.PyBindDemoException:
            pass  # If empty lists cause issues, that's also valid
        
        # Test with data that should work
        good_data = [1, 2, 3, 4, 5]
        result = funcs.process_list(good_data)
        assert len(result) == 5
    
    def test_exceptions_with_class_hierarchy(self):
        """Test exception handling in inheritance hierarchy."""
        # Create shapes and test exception scenarios
        rect = pybind_demo.Rectangle(5.0, 3.0)
        circle = pybind_demo.Circle(2.0)
        
        # Normal operations should work
        assert rect.area() == 15.0
        assert abs(circle.area() - (3.14159 * 4.0)) < 0.1
        
        # Test with potentially problematic values
        try:
            rect.width = -1.0  # Negative width might be invalid
            area = rect.area()  # This might throw or return negative area
            # If it doesn't throw, negative area might be valid depending on implementation
        except pybind_demo.PyBindDemoException:
            # If negative dimensions are rejected, that's valid
            pass
    
    def test_exceptions_with_numpy_operations(self):
        """Test exception handling with NumPy operations."""
        import numpy as np
        
        # Test with valid arrays
        arr1 = np.array([1.0, 2.0, 3.0])
        arr2 = np.array([4.0, 5.0, 6.0])
        
        result = pybind_demo.numpy_demo.add_arrays(arr1, arr2)
        expected = arr1 + arr2
        np.testing.assert_allclose(result, expected)
        
        # Test with incompatible arrays
        arr3 = np.array([1.0, 2.0])  # Different size
        
        with pytest.raises((ValueError, RuntimeError, pybind_demo.PyBindDemoException)):
            pybind_demo.numpy_demo.add_arrays(arr1, arr3)
    
    def test_exception_state_consistency_across_modules(self):
        """Test that exception state is consistent across different modules."""
        # Use Calculator from main module
        calc = pybind_demo.Calculator(10.0)
        
        # Use NumPy operations
        arr = np.array([1.0, 2.0, 3.0])
        squared = pybind_demo.numpy_demo.square_array(arr)
        
        # Use container operations
        data = [1, 2, 3]
        processed = funcs.process_list(data)
        
        # Exception in one area shouldn't affect others
        with pytest.raises(pybind_demo.PyBindDemoException):
            calc.divide(0.0)
        
        # Other operations should still work
        squared2 = pybind_demo.numpy_demo.square_array(arr)
        np.testing.assert_allclose(squared, squared2)
        
        processed2 = funcs.process_list(data)
        assert processed == processed2
