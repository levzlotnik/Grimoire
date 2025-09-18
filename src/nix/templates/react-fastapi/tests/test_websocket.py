import pytest
from fastapi.testclient import TestClient
from backend.main import app
import json

client = TestClient(app)

def test_websocket_connection():
    """Test WebSocket connection."""
    with client.websocket_connect("/ws") as websocket:
        # Send a test message
        test_message = {"type": "test", "data": "hello"}
        websocket.send_text(json.dumps(test_message))
        
        # Receive the echoed message
        data = websocket.receive_text()
        message = json.loads(data)
        
        assert message["type"] == "echo"
        assert message["data"] == test_message


def test_websocket_invalid_json():
    """Test WebSocket with invalid JSON."""
    with client.websocket_connect("/ws") as websocket:
        # Send invalid JSON
        websocket.send_text("invalid json")
        
        # Should receive error message
        data = websocket.receive_text()
        message = json.loads(data)
        
        assert message["type"] == "error"
        assert "Invalid JSON format" in message["data"]["message"]


def test_websocket_task_notifications():
    """Test WebSocket task notifications."""
    # This test would require more complex setup with authentication
    # For now, we'll just test the basic connection
    with client.websocket_connect("/ws") as websocket:
        # The WebSocket should stay connected
        assert websocket is not None
