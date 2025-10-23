"""
Pytest configuration and shared fixtures for grimoire tests
"""
import pytest
import os


# Ensure GRIMOIRE_ROOT is set for all tests
@pytest.fixture(scope="session", autouse=True)
def ensure_grimoire_root():
    """Ensure GRIMOIRE_ROOT is set, skip tests if not in nix develop"""
    if not os.getenv('GRIMOIRE_ROOT'):
        pytest.skip("GRIMOIRE_ROOT not set - must run in nix develop")


@pytest.fixture(scope="module")
def grimoire_client():
    """Shared Grimoire client instance for tests"""
    from grimoire import Grimoire
    return Grimoire()
