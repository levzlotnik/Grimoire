"""
PyTest configuration and fixtures for PyBind11 demo tests.

This module provides shared fixtures and configuration for testing
all PyBind11 features in the comprehensive template.
"""

import pytest
import numpy as np
import sys
from pathlib import Path

# Add the python package to the path for imports
sys.path.insert(0, str(Path(__file__).parent.parent / "python"))

try:
    import pybind_demo
    import pybind_demo.functions
    import pybind_demo.numpy_demo
except ImportError as e:
    pytest.skip(f"Could not import pybind_demo: {e}", allow_module_level=True)


@pytest.fixture
def sample_integers():
    """Provide sample integer values for testing."""
    return [1, 2, 3, 4, 5, -1, -2, 0, 100, -100]


@pytest.fixture
def sample_floats():
    """Provide sample float values for testing."""
    return [1.0, 2.5, 3.14, -1.5, 0.0, 1e6, 1e-6, float('inf'), -float('inf')]


@pytest.fixture
def sample_strings():
    """Provide sample string values for testing."""
    return ["hello", "world", "", "test string", "unicode: αβγ", "numbers: 123"]


@pytest.fixture
def sample_lists():
    """Provide sample list structures for testing."""
    return [
        [],
        [1, 2, 3],
        [1.0, 2.0, 3.0],
        ["a", "b", "c"],
        [1, "mixed", 3.14],
        list(range(100)),  # Large list
    ]


@pytest.fixture
def sample_dicts():
    """Provide sample dictionary structures for testing."""
    return [
        {},
        {"a": 1, "b": 2},
        {"x": 1.0, "y": 2.0, "z": 3.0},
        {"name": "test", "value": 42},
        {str(i): i**2 for i in range(10)},  # Larger dict
    ]


@pytest.fixture
def sample_numpy_arrays():
    """Provide sample NumPy arrays for testing."""
    return {
        "1d_int": np.array([1, 2, 3, 4, 5], dtype=np.int32),
        "1d_float": np.array([1.0, 2.0, 3.0, 4.0, 5.0], dtype=np.float64),
        "2d_int": np.array([[1, 2], [3, 4]], dtype=np.int32),
        "2d_float": np.array([[1.0, 2.0], [3.0, 4.0]], dtype=np.float64),
        "3d_float": np.random.rand(3, 4, 5).astype(np.float64),
        "large_1d": np.arange(1000, dtype=np.float64),
        "empty": np.array([], dtype=np.float64),
        "single": np.array([42.0], dtype=np.float64),
    }


@pytest.fixture
def sample_matrices():
    """Provide sample matrix data for testing."""
    return {
        "2x2": [[1.0, 2.0], [3.0, 4.0]],
        "3x3": [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]],  # Identity
        "2x3": [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]],
        "3x2": [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]],
        "large": [[float(i*j) for j in range(10)] for i in range(10)],
    }


@pytest.fixture
def calculator():
    """Provide a Calculator instance for testing."""
    return pybind_demo.Calculator(10.0, "test_calc")


@pytest.fixture
def shapes():
    """Provide sample Shape instances for testing."""
    return {
        "rectangle": pybind_demo.Rectangle(5.0, 3.0, "test_rect"),
        "circle": pybind_demo.Circle(2.0, "test_circle"),
        "rectangle_factory": pybind_demo.Shape.create_rectangle(4.0, 6.0),
        "circle_factory": pybind_demo.Shape.create_circle(1.5),
    }


@pytest.fixture
def data_container():
    """Provide a DataContainer instance for testing."""
    container = pybind_demo.DataContainer([1.0, 2.0, 3.0, 4.0, 5.0])
    container.label = "test_container"
    return container


@pytest.fixture
def resource_manager():
    """Provide a ResourceManager instance for testing."""
    manager = pybind_demo.ResourceManager("test_manager")
    manager.create_shared_resource(10, 1.5)
    return manager


@pytest.fixture
def matrix_instances():
    """Provide Matrix instances for testing."""
    return {
        "2x2": pybind_demo.numpy_demo.Matrix(2, 2, 1.0),
        "3x3_identity": pybind_demo.numpy_demo.Matrix([
            [1.0, 0.0, 0.0],
            [0.0, 1.0, 0.0],
            [0.0, 0.0, 1.0]
        ]),
        "2x3": pybind_demo.numpy_demo.Matrix([
            [1.0, 2.0, 3.0],
            [4.0, 5.0, 6.0]
        ]),
    }


@pytest.fixture
def image_instances():
    """Provide Image instances for testing."""
    return {
        "small": pybind_demo.numpy_demo.Image(4, 4, 1),
        "rgb": pybind_demo.numpy_demo.Image(8, 8, 3),
        "large": pybind_demo.numpy_demo.Image(100, 100, 1),
    }


# Markers for different test categories
def pytest_configure(config):
    """Configure custom pytest markers."""
    config.addinivalue_line(
        "markers", "slow: marks tests as slow (deselect with '-m \"not slow\"')"
    )
    config.addinivalue_line(
        "markers", "integration: marks tests as integration tests"
    )
    config.addinivalue_line(
        "markers", "numpy: marks tests that require numpy"
    )
    config.addinivalue_line(
        "markers", "scipy: marks tests that require scipy"
    )


# Skip tests if required packages are missing
def pytest_collection_modifyitems(config, items):
    """Modify test collection to handle missing dependencies."""
    skip_numpy = pytest.mark.skip(reason="NumPy not available")
    skip_scipy = pytest.mark.skip(reason="SciPy not available")
    
    # Check for NumPy
    try:
        import numpy
    except ImportError:
        numpy = None
    
    # Check for SciPy
    try:
        import scipy
    except ImportError:
        scipy = None
    
    for item in items:
        if "numpy" in item.keywords and numpy is None:
            item.add_marker(skip_numpy)
        if "scipy" in item.keywords and scipy is None:
            item.add_marker(skip_scipy)


# Custom assertions for floating point comparisons
class FloatComparison:
    """Helper class for floating point comparisons."""
    
    @staticmethod
    def assert_close(actual, expected, rtol=1e-7, atol=1e-10):
        """Assert that two floating point values are close."""
        if isinstance(expected, (list, tuple)):
            assert len(actual) == len(expected)
            for a, e in zip(actual, expected):
                FloatComparison.assert_close(a, e, rtol, atol)
        else:
            assert abs(actual - expected) <= atol + rtol * abs(expected)
    
    @staticmethod
    def assert_array_close(actual, expected, rtol=1e-7, atol=1e-10):
        """Assert that two NumPy arrays are close."""
        np.testing.assert_allclose(actual, expected, rtol=rtol, atol=atol)


@pytest.fixture
def float_comparison():
    """Provide floating point comparison utilities."""
    return FloatComparison
