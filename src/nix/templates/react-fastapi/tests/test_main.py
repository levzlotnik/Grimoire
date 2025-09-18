import pytest
from fastapi.testclient import TestClient
from unittest.mock import patch
import json
from backend.main import app

client = TestClient(app)

def test_health_check():
    """Test the health check endpoint."""
    response = client.get("/api/health")
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "healthy"
    assert "timestamp" in data


def test_register_user():
    """Test user registration."""
    user_data = {
        "username": "testuser",
        "email": "test@example.com",
        "password": "testpassword"
    }
    response = client.post("/api/register", json=user_data)
    assert response.status_code == 200
    data = response.json()
    assert data["username"] == "testuser"
    assert data["email"] == "test@example.com"
    assert "id" in data


def test_register_duplicate_username():
    """Test registration with duplicate username."""
    user_data = {
        "username": "testuser",
        "email": "test2@example.com",
        "password": "testpassword"
    }
    # First registration should succeed
    response1 = client.post("/api/register", json=user_data)
    assert response1.status_code == 200
    
    # Second registration with same username should fail
    response2 = client.post("/api/register", json=user_data)
    assert response2.status_code == 400
    assert "Username already registered" in response2.json()["detail"]


def test_login():
    """Test user login."""
    # First register a user
    user_data = {
        "username": "logintest",
        "email": "logintest@example.com",
        "password": "testpassword"
    }
    client.post("/api/register", json=user_data)
    
    # Then login
    login_data = {
        "username": "logintest",
        "password": "testpassword"
    }
    response = client.post("/api/login", params=login_data)
    assert response.status_code == 200
    data = response.json()
    assert "access_token" in data
    assert data["token_type"] == "bearer"


def test_login_invalid_credentials():
    """Test login with invalid credentials."""
    login_data = {
        "username": "nonexistent",
        "password": "wrongpassword"
    }
    response = client.post("/api/login", params=login_data)
    assert response.status_code == 401


def get_auth_headers(username="taskuser", email="taskuser@example.com", password="testpassword"):
    """Helper function to get authentication headers."""
    # Register user
    user_data = {
        "username": username,
        "email": email,
        "password": password
    }
    client.post("/api/register", json=user_data)
    
    # Login to get token
    login_data = {
        "username": username,
        "password": password
    }
    response = client.post("/api/login", params=login_data)
    token = response.json()["access_token"]
    
    return {"Authorization": f"Bearer {token}"}


def test_get_current_user():
    """Test getting current user info."""
    headers = get_auth_headers()
    response = client.get("/api/me", headers=headers)
    assert response.status_code == 200
    data = response.json()
    assert data["username"] == "taskuser"
    assert data["email"] == "taskuser@example.com"


def test_create_task():
    """Test creating a task."""
    headers = get_auth_headers(username="createtask", email="createtask@example.com")
    task_data = {
        "title": "Test Task",
        "description": "This is a test task"
    }
    response = client.post("/api/tasks", json=task_data, headers=headers)
    assert response.status_code == 200
    data = response.json()
    assert data["title"] == "Test Task"
    assert data["description"] == "This is a test task"
    assert data["completed"] is False
    assert "id" in data


def test_get_tasks():
    """Test getting user's tasks."""
    headers = get_auth_headers(username="gettasks", email="gettasks@example.com")
    
    # Create a task first
    task_data = {
        "title": "Test Task for Get",
        "description": "Description"
    }
    client.post("/api/tasks", json=task_data, headers=headers)
    
    # Get tasks
    response = client.get("/api/tasks", headers=headers)
    assert response.status_code == 200
    data = response.json()
    assert isinstance(data, list)
    assert len(data) >= 1
    assert data[0]["title"] == "Test Task for Get"


def test_update_task():
    """Test updating a task."""
    headers = get_auth_headers(username="updatetask", email="updatetask@example.com")
    
    # Create a task first
    task_data = {
        "title": "Original Title",
        "description": "Original Description"
    }
    create_response = client.post("/api/tasks", json=task_data, headers=headers)
    task_id = create_response.json()["id"]
    
    # Update the task
    update_data = {
        "title": "Updated Title",
        "completed": True
    }
    response = client.put(f"/api/tasks/{task_id}", json=update_data, headers=headers)
    assert response.status_code == 200
    data = response.json()
    assert data["title"] == "Updated Title"
    assert data["completed"] is True


def test_delete_task():
    """Test deleting a task."""
    headers = get_auth_headers(username="deletetask", email="deletetask@example.com")
    
    # Create a task first
    task_data = {
        "title": "Task to Delete",
        "description": "This will be deleted"
    }
    create_response = client.post("/api/tasks", json=task_data, headers=headers)
    task_id = create_response.json()["id"]
    
    # Delete the task
    response = client.delete(f"/api/tasks/{task_id}", headers=headers)
    assert response.status_code == 200
    assert "message" in response.json()
    
    # Verify task is deleted
    get_response = client.get(f"/api/tasks/{task_id}", headers=headers)
    assert get_response.status_code == 404


def test_task_access_control():
    """Test that users can only access their own tasks."""
    # Create two users
    headers1 = get_auth_headers(username="user1", email="user1@example.com")
    headers2 = get_auth_headers(username="user2", email="user2@example.com")
    
    # User 1 creates a task
    task_data = {
        "title": "User 1 Task",
        "description": "Only user 1 should see this"
    }
    create_response = client.post("/api/tasks", json=task_data, headers=headers1)
    task_id = create_response.json()["id"]
    
    # User 2 tries to access user 1's task
    response = client.get(f"/api/tasks/{task_id}", headers=headers2)
    assert response.status_code == 404


def test_unauthorized_access():
    """Test that endpoints require authentication."""
    # Try to access protected endpoints without authentication
    response = client.get("/api/me")
    assert response.status_code == 401
    
    response = client.get("/api/tasks")
    assert response.status_code == 401
    
    response = client.post("/api/tasks", json={"title": "Test"})
    assert response.status_code == 401


@pytest.fixture(autouse=True)
def cleanup_database():
    """Clean up database before each test."""
    from backend.main import engine, Base
    # Drop all tables and recreate them
    Base.metadata.drop_all(bind=engine)
    Base.metadata.create_all(bind=engine)
    yield
    # Cleanup after test if needed