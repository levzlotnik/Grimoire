"""
Pytest configuration for benchmarks.
"""

import pytest

def pytest_configure(config):
    """Configure pytest with benchmark-specific settings."""
    config.addinivalue_line("markers", "benchmark: marks tests as benchmarks")
    config.addinivalue_line("markers", "slow: marks tests as slow benchmarks")

# Configure pytest-benchmark if available
try:
    import pytest_benchmark
    
    # Custom benchmark configuration
    @pytest.fixture(scope="session")
    def benchmark_config():
        return {
            'min_rounds': 5,
            'min_time': 0.1,
            'max_time': 1.0,
            'warmup': True,
            'warmup_iterations': 3,
        }
        
except ImportError:
    # Fallback if pytest-benchmark is not available
    @pytest.fixture
    def benchmark():
        """Simple benchmark fixture that just runs the function."""
        def _benchmark(func, *args, **kwargs):
            return func(*args, **kwargs)
        return _benchmark