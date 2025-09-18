"""
Pytest configuration for PyTorch custom operations tests.
"""

import pytest
import torch

def pytest_configure(config):
    """Configure pytest with custom markers."""
    config.addinivalue_line("markers", "slow: marks tests as slow (deselect with '-m \"not slow\"')")
    config.addinivalue_line("markers", "gpu: marks tests as requiring GPU")

@pytest.fixture(scope="session")
def device():
    """Provide device for testing (CPU only in this template)."""
    return torch.device("cpu")

@pytest.fixture(scope="session", autouse=True)
def set_torch_deterministic():
    """Set PyTorch to deterministic mode for reproducible tests."""
    torch.manual_seed(42)
    torch.use_deterministic_algorithms(True, warn_only=True)

@pytest.fixture
def random_seed():
    """Provide a random seed for tests that need one."""
    return 12345
