"""
HTTP API tests for grimoire-http

Tests REST API using FastAPI TestClient (in-memory, no subprocess).
"""
import pytest
from fastapi.testclient import TestClient
from grimoire.http import app


@pytest.fixture(scope="module")
def client():
    """Create TestClient for HTTP tests"""
    return TestClient(app)


class TestHTTPAPI:
    """Test suite for Grimoire REST API"""

    def test_root_endpoint(self, client):
        """Test root endpoint"""
        response = client.get("/")
        assert response.status_code == 200
        data = response.json()
        assert "system_info" in data

    def test_health_endpoint(self, client):
        """Test health check endpoint"""
        response = client.get("/health")
        assert response.status_code == 200
        data = response.json()
        assert data["status"] == "healthy"

    def test_entities_endpoint(self, client):
        """Test entities endpoint"""
        response = client.get("/entities")
        assert response.status_code == 200
        data = response.json()
        assert data["success"] == True
        assert len(data["result"]["entities"]) > 0

    def test_component_types_endpoint(self, client):
        """Test component_types endpoint"""
        response = client.get("/component_types/system")
        assert response.status_code == 200
        data = response.json()
        assert data["success"] == True
        assert len(data["result"]["types"]) > 0

    def test_components_endpoint(self, client):
        """Test components endpoint"""
        response = client.get("/components/system/self")
        assert response.status_code == 200
        data = response.json()
        assert data["success"] == True

    def test_docstring_endpoint(self, client):
        """Test docstring endpoint"""
        response = client.get("/docstring/system")
        assert response.status_code == 200
        data = response.json()
        assert data["success"] == True
        assert len(data["result"]["docstring"]) > 0

    def test_spells_endpoint(self, client):
        """Test spells list endpoint"""
        response = client.get("/spells")
        assert response.status_code == 200
        data = response.json()
        assert data["success"] == True
        assert len(data["result"]["spells"]) > 0

    def test_perceive_endpoint(self, client):
        """Test perceive POST endpoint"""
        response = client.post(
            "/perceive",
            json={"query_sig": "interface(entities)", "args": {}}
        )
        assert response.status_code == 200
        data = response.json()
        assert data["success"] == True
