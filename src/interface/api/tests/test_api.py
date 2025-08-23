"""
Tests for FastAPI endpoints
"""

import pytest
from fastapi.testclient import TestClient
from unittest.mock import Mock, patch
import sys
from pathlib import Path

# Add the parent directory to the path so we can import our modules
sys.path.insert(0, str(Path(__file__).parent.parent))

from main import app
from grimoire_interface import GrimoireInterface


@pytest.fixture
def client():
    """Create test client"""
    return TestClient(app)


@pytest.fixture
def mock_grimoire():
    """Mock GrimoireInterface for testing"""
    with patch('main.grimoire') as mock:
        yield mock


class TestAPIEndpoints:
    """Test FastAPI endpoints"""
    
    def test_root_endpoint(self, client, mock_grimoire):
        """Test root endpoint returns API information"""
        mock_grimoire.get_system_info.return_value = {
            "grimoire_root": "/test/path",
            "janus_available": True,
            "prolog_initialized": True,
            "interface_version": "0.1.0"
        }
        
        response = client.get("/")
        
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert "api" in data["result"]
        assert data["result"]["api"] == "Grimoire Interface API"
        assert "endpoints" in data["result"]
        
    def test_health_check_healthy(self, client, mock_grimoire):
        """Test health check when system is healthy"""
        mock_grimoire.test_prolog_connection.return_value = {
            "janus_available": True,
            "prolog_initialized": True,
            "prolog_test": True
        }
        mock_grimoire.get_system_info.return_value = {
            "grimoire_root": "/test/path",
            "janus_available": True,
            "prolog_initialized": True
        }
        mock_grimoire.is_available.return_value = True
        
        response = client.get("/health")
        
        assert response.status_code == 200
        data = response.json()
        assert data["status"] == "healthy"
        assert data["janus_available"] is True
        assert data["api_version"] == "0.1.0"
        
    def test_health_check_unhealthy(self, client, mock_grimoire):
        """Test health check when janus is not available"""
        mock_grimoire.test_prolog_connection.return_value = {
            "janus_available": False,
            "error": "janus-swi not installed"
        }
        mock_grimoire.get_system_info.return_value = {
            "grimoire_root": "/test/path",
            "janus_available": False,
            "prolog_initialized": False
        }
        mock_grimoire.is_available.return_value = False
        
        response = client.get("/health")
        
        assert response.status_code == 200
        data = response.json()
        assert data["status"] == "unhealthy"
        assert data["janus_available"] is False
        
    def test_compt_endpoint(self, client, mock_grimoire):
        """Test component types endpoint"""
        mock_grimoire.call_interface_predicate.return_value = {
            "success": True,
            "result": ["type1", "type2", "type3"]
        }
        
        response = client.get("/compt")
        
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert data["result"] == ["type1", "type2", "type3"]
        mock_grimoire.call_interface_predicate.assert_called_once_with("compt")
        
    def test_compt_entity_endpoint(self, client, mock_grimoire):
        """Test component types for entity endpoint"""
        mock_grimoire.call_interface_predicate.return_value = {
            "success": True,
            "result": ["entity_type1", "entity_type2"]
        }
        
        response = client.get("/compt/test_entity")
        
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert data["result"] == ["entity_type1", "entity_type2"]
        mock_grimoire.call_interface_predicate.assert_called_once_with("compt", ["test_entity"])
        
    def test_comp_endpoint(self, client, mock_grimoire):
        """Test components listing endpoint"""
        mock_grimoire.call_interface_predicate.return_value = {
            "success": True,
            "result": ["comp1", "comp2"]
        }
        
        response = client.get("/comp/test_entity/test_type")
        
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert data["result"] == ["comp1", "comp2"]
        mock_grimoire.call_interface_predicate.assert_called_once_with("comp", ["test_entity", "test_type"])
        
    def test_doc_endpoint(self, client, mock_grimoire):
        """Test documentation endpoint"""
        mock_grimoire.call_interface_predicate.return_value = {
            "success": True,
            "result": "System documentation here"
        }
        
        response = client.get("/doc")
        
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert data["result"] == "System documentation here"
        mock_grimoire.call_interface_predicate.assert_called_once_with("doc")
        
    def test_doc_entity_endpoint(self, client, mock_grimoire):
        """Test entity documentation endpoint"""
        mock_grimoire.call_interface_predicate.return_value = {
            "success": True,
            "result": "Entity documentation here"
        }
        
        response = client.get("/doc/test_entity")
        
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert data["result"] == "Entity documentation here"
        mock_grimoire.call_interface_predicate.assert_called_once_with("doc", ["test_entity"])
        
    def test_perceive_endpoint(self, client, mock_grimoire):
        """Test perceive query endpoint"""
        mock_grimoire.call_perceive_query.return_value = {
            "success": True,
            "solutions": [{"X": "1"}, {"X": "2"}],
            "count": 2
        }
        
        response = client.get("/perceive?query=component(X,_,_)")
        
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert data["result"] == [{"X": "1"}, {"X": "2"}]
        mock_grimoire.call_perceive_query.assert_called_once_with("component(X,_,_)")
        
    def test_perceive_endpoint_with_session(self, client, mock_grimoire):
        """Test perceive query endpoint with session ID"""
        mock_grimoire.call_perceive_query.return_value = {
            "success": True,
            "solutions": [{"X": "test"}],
            "count": 1
        }
        
        response = client.get("/perceive?query=test&session_id=test_session")
        
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert data["session_id"] == "test_session"
        
    def test_conjure_endpoint(self, client, mock_grimoire):
        """Test conjure spell endpoint"""
        mock_grimoire.call_conjure_spell.return_value = {
            "success": True,
            "result": "spell_executed"
        }
        
        response = client.post("/conjure", json={"spell": "test_spell"})
        
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert data["result"] == "spell_executed"
        mock_grimoire.call_conjure_spell.assert_called_once_with("test_spell")
        
    def test_conjure_endpoint_with_session(self, client, mock_grimoire):
        """Test conjure spell endpoint with session ID"""
        mock_grimoire.call_conjure_spell.return_value = {
            "success": True,
            "result": "spell_executed"
        }
        
        response = client.post("/conjure", json={
            "spell": "test_spell",
            "session_id": "test_session"
        })
        
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert data["session_id"] == "test_session"
        
    def test_status_endpoint(self, client, mock_grimoire):
        """Test status endpoint"""
        mock_grimoire.call_interface_predicate.return_value = {
            "success": True,
            "result": {"status": "running", "session_count": 1}
        }
        
        response = client.get("/status")
        
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert data["result"]["status"] == "running"
        mock_grimoire.call_interface_predicate.assert_called_once_with("status")
        
    def test_error_handling(self, client, mock_grimoire):
        """Test error handling in endpoints"""
        mock_grimoire.call_interface_predicate.return_value = {
            "success": False,
            "error": "Test error message"
        }
        
        response = client.get("/compt")
        
        assert response.status_code == 200  # FastAPI doesn't return 500 for business logic errors
        data = response.json()
        assert data["success"] is False
        assert data["error"] == "Test error message"
        
    def test_health_check_with_exception(self, client, mock_grimoire):
        """Test health check when an exception occurs"""
        mock_grimoire.test_prolog_connection.side_effect = Exception("Connection failed")
        
        response = client.get("/health")
        
        assert response.status_code == 200
        data = response.json()
        assert data["status"] == "unhealthy"
        assert "Connection failed" in data["error"]
        assert data["janus_available"] is False


# Run with: pytest test_api.py
