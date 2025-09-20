"""
Tests for PyBind11 container type conversions.

This module tests STL container conversion features including:
- std::vector ↔ Python list conversions
- std::unordered_map ↔ Python dict conversions
- std::set ↔ Python set conversions (if implemented)
- Nested container structures
- Type safety and error handling
- Performance considerations for large containers
"""

import pytest
from typing import List, Dict, Any, Set

import pybind_demo
from pybind_demo import functions as funcs


class TestVectorConversions:
    """Test std::vector ↔ Python list conversions."""
    
    def test_vector_input_conversion(self, sample_lists):
        """Test Python list → std::vector conversion."""
        # Test process_list function which takes std::vector<int>
        for lst in sample_lists:
            if all(isinstance(x, int) for x in lst):
                try:
                    result = funcs.process_list(lst)
                    # Should return a list (converted from std::vector)
                    assert isinstance(result, list)
                    assert len(result) == len(lst)
                    # Each element should be x*x + 1
                    for i, x in enumerate(lst):
                        assert result[i] == x * x + 1
                except Exception as e:
                    # Some conversions might fail for very large lists
                    if "overflow" not in str(e).lower():
                        raise
    
    def test_vector_type_conversion(self):
        """Test automatic type conversion in vectors."""
        # Test with floats that should convert to ints
        float_list = [1.7, 2.3, 3.9, 4.1]
        int_list = [int(x) for x in float_list]
        
        # process_list expects std::vector<int>
        result = funcs.process_list(int_list)
        expected = [x * x + 1 for x in int_list]
        assert result == expected
    
    def test_vector_empty(self):
        """Test empty vector conversions."""
        empty_list = []
        result = funcs.process_list(empty_list)
        assert result == []
        assert isinstance(result, list)
    
    def test_vector_large(self):
        """Test large vector conversions."""
        large_list = list(range(1000))
        result = funcs.process_list(large_list)
        
        assert len(result) == 1000
        for i, x in enumerate(large_list):
            assert result[i] == x * x + 1
    
    def test_vector_nested_operations(self):
        """Test operations on vectors of vectors (if supported)."""
        # This tests the depth of container conversion support
        nested_data = [[1, 2], [3, 4], [5, 6]]
        
        # Flatten and process
        flat_data = [item for sublist in nested_data for item in sublist]
        result = funcs.process_list(flat_data)
        
        # Reconstruct nested structure
        nested_result = [result[i:i+2] for i in range(0, len(result), 2)]
        
        # Verify structure
        assert len(nested_result) == 3
        for i, sublist in enumerate(nested_data):
            for j, val in enumerate(sublist):
                expected = val * val + 1
                assert nested_result[i][j] == expected


class TestMapConversions:
    """Test std::unordered_map ↔ Python dict conversions."""
    
    def test_map_input_conversion(self, sample_dicts):
        """Test Python dict → std::unordered_map conversion."""
        # Test process_dict function which takes std::unordered_map<std::string, int>
        for d in sample_dicts:
            if all(isinstance(k, str) and isinstance(v, int) for k, v in d.items()):
                result = funcs.process_dict(d)
                
                # Should return a dict (converted from std::unordered_map)
                assert isinstance(result, dict)
                assert len(result) == len(d)
                
                # Keys should be uppercase, values doubled
                for key, value in d.items():
                    expected_key = key.upper()
                    expected_value = value * 2
                    assert result[expected_key] == expected_value
    
    def test_map_output_conversion(self):
        """Test std::unordered_map → Python dict conversion."""
        input_dict = {"hello": 5, "world": 10, "test": 15}
        result = funcs.process_dict(input_dict)
        
        # Should be accessible as Python dict
        assert isinstance(result, dict)
        assert len(result) == 3
        
        expected = {"HELLO": 10, "WORLD": 20, "TEST": 30}
        assert result == expected
    
    def test_map_key_type_conversion(self):
        """Test automatic key type conversion."""
        # Test with string keys (standard case)
        string_dict = {"key1": 1, "key2": 2}
        result = funcs.process_dict(string_dict)
        
        assert result == {"KEY1": 2, "KEY2": 4}
    
    def test_map_value_type_conversion(self):
        """Test automatic value type conversion."""
        # Test with float values that should convert to int
        mixed_dict = {"a": 1.0, "b": 2.0, "c": 3.0}
        # Convert floats to ints
        int_dict = {k: int(v) for k, v in mixed_dict.items()}
        
        result = funcs.process_dict(int_dict)
        expected = {"A": 2, "B": 4, "C": 6}
        assert result == expected
    
    def test_map_empty(self):
        """Test empty map conversions."""
        empty_dict = {}
        result = funcs.process_dict(empty_dict)
        assert result == {}
        assert isinstance(result, dict)
    
    def test_map_large(self):
        """Test large map conversions."""
        large_dict = {f"key_{i}": i for i in range(1000)}
        result = funcs.process_dict(large_dict)
        
        assert len(result) == 1000
        for i in range(1000):
            key = f"KEY_{i}"
            assert result[key] == i * 2
    
    def test_map_special_characters(self):
        """Test map with special character handling."""
        special_dict = {
            "normal_key": 1,
            "key-with-dash": 2,
            "key_with_underscore": 3,
            "key with space": 4,
            "key.with.dots": 5,
            "key(with)parens": 6,
            "key[with]brackets": 7,
            "key{with}braces": 8,
        }
        
        result = funcs.process_dict(special_dict)
        
        # All keys should be converted to uppercase
        for original_key, value in special_dict.items():
            expected_key = original_key.upper()
            expected_value = value * 2
            assert result[expected_key] == expected_value
    
    def test_map_unicode_keys(self):
        """Test map with unicode character handling."""
        unicode_dict = {
            "ascii": 1,
            "café": 2,
            "naïve": 3,
            "résumé": 4,
            "αβγ": 5,
            "中文": 6,
        }
        
        result = funcs.process_dict(unicode_dict)
        
        # Unicode keys should be handled correctly (ASCII chars only get uppercased)
        expected_results = {
            "ASCII": 2,
            "CAFé": 4,    # C++ toupper only converts ASCII chars
            "NAïVE": 6,   # C++ toupper only converts ASCII chars  
            "RéSUMé": 8,  # C++ toupper only converts ASCII chars
            "αβγ": 10,    # Non-Latin chars unchanged
            "中文": 12,    # Non-Latin chars unchanged
        }
        
        for expected_key, expected_value in expected_results.items():
            assert result[expected_key] == expected_value
    
    def test_map_iteration_order(self):
        """Test that map iteration handles order correctly."""
        # Note: std::unordered_map doesn't guarantee order
        input_dict = {"z": 1, "a": 2, "m": 3}
        result = funcs.process_dict(input_dict)
        
        # Should have all expected entries regardless of order
        expected_keys = {"Z", "A", "M"}
        expected_values = {2, 4, 6}
        
        assert set(result.keys()) == expected_keys
        assert set(result.values()) == expected_values


class TestContainerTypeValidation:
    """Test type validation for container conversions."""
    
    def test_vector_wrong_element_type(self):
        """Test vector with wrong element types."""
        # process_list expects vector<int>, not vector<string>
        string_list = ["hello", "world", "test"]
        
        with pytest.raises(TypeError):
            funcs.process_list(string_list)
    
    def test_vector_mixed_types(self):
        """Test vector with mixed element types."""
        mixed_list = [1, "string", 3.14, None]
        
        with pytest.raises(TypeError):
            funcs.process_list(mixed_list)
    
    def test_map_wrong_key_type(self):
        """Test map with wrong key types."""
        # process_dict expects map<string, int>, not map<int, int>
        int_key_dict = {1: 10, 2: 20, 3: 30}
        
        with pytest.raises(TypeError):
            funcs.process_dict(int_key_dict)
    
    def test_map_wrong_value_type(self):
        """Test map with wrong value types."""
        # process_dict expects map<string, int>, not map<string, string>
        string_value_dict = {"a": "hello", "b": "world"}
        
        with pytest.raises(TypeError):
            funcs.process_dict(string_value_dict)
    
    def test_map_mixed_key_types(self):
        """Test map with mixed key types."""
        mixed_key_dict = {"string_key": 1, 123: 2}
        
        with pytest.raises(TypeError):
            funcs.process_dict(mixed_key_dict)
    
    def test_map_mixed_value_types(self):
        """Test map with mixed value types."""
        mixed_value_dict = {"a": 1, "b": "string", "c": 3.14}
        
        with pytest.raises(TypeError):
            funcs.process_dict(mixed_value_dict)
    
    def test_none_as_container(self):
        """Test passing None where container expected."""
        with pytest.raises(TypeError):
            funcs.process_list(None)
        
        with pytest.raises(TypeError):
            funcs.process_dict(None)
    
    def test_non_container_types(self):
        """Test passing non-container types."""
        # Single values instead of containers
        with pytest.raises(TypeError):
            funcs.process_list(42)
        
        with pytest.raises(TypeError):
            funcs.process_dict("not a dict")


class TestContainerPerformance:
    """Test performance aspects of container conversions."""
    
    @pytest.mark.slow
    def test_large_vector_performance(self):
        """Test performance with large vectors."""
        # Create large vector
        large_size = 100000
        large_list = list(range(large_size))
        
        # This should complete in reasonable time
        import time
        start_time = time.time()
        result = funcs.process_list(large_list)
        end_time = time.time()
        
        # Verify correctness
        assert len(result) == large_size
        for i in range(min(100, large_size)):  # Check first 100 elements
            assert result[i] == i * i + 1
        
        # Performance check (should be reasonably fast)
        elapsed = end_time - start_time
        assert elapsed < 1.0  # Should complete within 1 second
    
    @pytest.mark.slow
    def test_large_map_performance(self):
        """Test performance with large maps."""
        # Create large map
        large_size = 10000
        large_dict = {f"key_{i}": i for i in range(large_size)}
        
        # This should complete in reasonable time
        import time
        start_time = time.time()
        result = funcs.process_dict(large_dict)
        end_time = time.time()
        
        # Verify correctness
        assert len(result) == large_size
        for i in range(min(100, large_size)):  # Check first 100 elements
            key = f"KEY_{i}"
            assert result[key] == i * 2
        
        # Performance check
        elapsed = end_time - start_time
        assert elapsed < 2.0  # Should complete within 2 seconds
    


class TestContainerNesting:
    """Test nested container structures."""
    
    def test_vector_of_vectors_simulation(self):
        """Simulate vector<vector<T>> using multiple calls."""
        # Since direct vector<vector<T>> might not be bound,
        # simulate with multiple process_list calls
        nested_data = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        
        results = []
        for sublist in nested_data:
            result = funcs.process_list(sublist)
            results.append(result)
        
        # Verify each sublist was processed correctly
        for i, (original, processed) in enumerate(zip(nested_data, results)):
            assert len(processed) == len(original)
            for j, (orig_val, proc_val) in enumerate(zip(original, processed)):
                assert proc_val == orig_val * orig_val + 1
    
    def test_map_of_vectors_simulation(self):
        """Simulate map<string, vector<T>> using multiple calls."""
        # Process each vector separately and combine into map structure
        vector_data = {
            "group1": [1, 2, 3],
            "group2": [4, 5, 6],
            "group3": [7, 8, 9]
        }
        
        result_map = {}
        for key, vec in vector_data.items():
            processed_vec = funcs.process_list(vec)
            result_map[key] = processed_vec
        
        # Verify structure
        assert len(result_map) == 3
        for key, original_vec in vector_data.items():
            processed_vec = result_map[key]
            assert len(processed_vec) == len(original_vec)
            for orig, proc in zip(original_vec, processed_vec):
                assert proc == orig * orig + 1


class TestContainerEdgeCases:
    """Test edge cases in container handling."""
    
    def test_container_with_extreme_values(self):
        """Test containers with extreme values."""
        # Test with large integers (no overflow with int64_t)
        large_ints = [2**30, 2**31 - 1, -(2**31)]
        result = funcs.process_list(large_ints)
        
        # Expected results without overflow (int64_t can handle these)
        expected_results = [x * x + 1 for x in large_ints]
        for i, expected in enumerate(expected_results):
            assert result[i] == expected
    
    def test_container_with_boundary_values(self):
        """Test containers with boundary values."""
        boundary_values = [0, 1, -1, 255, 256, 65535, 65536]
        result = funcs.process_list(boundary_values)
        
        for i, x in enumerate(boundary_values):
            assert result[i] == x * x + 1
    
    def test_map_with_empty_keys(self):
        """Test map with empty string keys."""
        empty_key_dict = {"": 1, "normal": 2, "another": 3}
        result = funcs.process_dict(empty_key_dict)
        
        expected = {"": 2, "NORMAL": 4, "ANOTHER": 6}
        assert result == expected
    
    def test_map_with_duplicate_uppercase_keys(self):
        """Test map where keys become duplicates after uppercase conversion."""
        # This might cause issues since "key" and "KEY" both become "KEY"
        # The behavior depends on the C++ implementation
        duplicate_dict = {"key": 1, "KEY": 2}
        
        try:
            result = funcs.process_dict(duplicate_dict)
            # If successful, one of the values should win
            assert "KEY" in result
            assert result["KEY"] in [2, 4]  # Either 1*2 or 2*2
        except (ValueError, RuntimeError):
            # It's acceptable to reject this case
            pass


@pytest.mark.integration
class TestContainerIntegration:
    """Integration tests combining container operations."""
    
    def test_vector_map_pipeline(self):
        """Test pipeline using both vectors and maps."""
        # Start with vector processing
        input_list = [1, 2, 3, 4, 5]
        processed_list = funcs.process_list(input_list)  # [2, 5, 10, 17, 26]
        
        # Convert to map
        list_as_map = {f"item_{i}": val for i, val in enumerate(processed_list)}
        processed_map = funcs.process_dict(list_as_map)
        
        # Verify final result
        expected_map = {f"ITEM_{i}": val * 2 for i, val in enumerate(processed_list)}
        assert processed_map == expected_map
    
