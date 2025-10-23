"""
Direct API tests for Grimoire client

Tests the core Grimoire class methods directly.
"""
import pytest
from grimoire import Grimoire, GrimoireError


class TestGrimoireClient:
    """Test suite for Grimoire Python client"""

    def test_initialization(self):
        """Test Grimoire client initializes successfully"""
        grimoire = Grimoire()
        assert grimoire is not None

    def test_entities(self, grimoire_client):
        """Test entities query"""
        result = grimoire_client.entities()
        assert len(result.entities) > 0
        assert 'system' in result.entities

    def test_component_types(self, grimoire_client):
        """Test component_types query"""
        result = grimoire_client.component_types('system')
        assert len(result.types) > 0

    def test_components(self, grimoire_client):
        """Test components query"""
        result = grimoire_client.components('system', 'self')
        # system/self can have multiple values
        assert result.is_unique or len(result.values) > 0

    def test_docstring(self, grimoire_client):
        """Test docstring query"""
        result = grimoire_client.docstring('system')
        assert len(result.docstring) > 0

    def test_system_instructions(self, grimoire_client):
        """Test system_instructions query"""
        result = grimoire_client.system_instructions()
        assert len(result.instructions) > 0

    def test_list_all_spells(self, grimoire_client):
        """Test list_all_spells helper"""
        spells = grimoire_client.list_all_spells()
        assert len(spells) > 0

    def test_exec(self, grimoire_client):
        """Test exec developer method"""
        solutions = grimoire_client.exec("member(X, [a, b, c])")
        assert len(solutions) == 3
