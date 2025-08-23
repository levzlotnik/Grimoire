# Python REST API Template

A FastAPI-based REST API template for Grimoire, providing HTTP endpoints for data processing, perception queries, and conjuration spells.

## Features

- **FastAPI Framework**: Modern, fast web framework for building APIs
- **Nix Integration**: Reproducible development environment and deployment
- **Session Management**: Optional session tracking across API calls
- **Standardized Responses**: Consistent APIResponse model for all endpoints
- **Example Endpoints**: GET and POST examples for common patterns

## API Endpoints

### Core Endpoints
- `GET /` - Root endpoint with API information
- `GET /hello` - Simple greeting endpoint
- `GET /health` - Health check endpoint

### Data Processing
- `POST /data` - Process arbitrary JSON data with transformation

### Grimoire Integration
- `GET /perceive?query=<query>&session_id=<id>` - Execute perception queries
- `POST /conjure` - Execute conjuration spells with session tracking

## Development

### Using Nix (Recommended)

```bash
# Enter development shell
nix develop

# Run the API server
nix run .#run
# or
uvicorn main:app --reload

# Run tests
nix run .#test
# or  
pytest
```

### Manual Setup

```bash
# Install dependencies
pip install -e .

# Run server
python main.py
# or
uvicorn main:app --reload

# Run tests
pytest
```

## API Documentation

Once running, visit:
- API docs: http://localhost:8000/docs
- ReDoc: http://localhost:8000/redoc

## Session Management

The API supports optional session management through:
- Query parameter `session_id` for GET requests
- JSON field `session_id` for POST requests
- Response field `session_id` in APIResponse model

## Response Format

All endpoints return responses in the standardized format:

```json
{
  "success": true,
  "result": { ... },
  "session_id": "optional-session-id"
}
```

## Testing

The template includes comprehensive tests for:
- API endpoint functionality
- Request/response models
- Session management
- Error handling

Run tests with `pytest` or `nix run .#test`.

## Integration with Grimoire

This template is designed to integrate with the Grimoire knowledge-based OS by:
1. Providing HTTP access to Grimoire functionality
2. Supporting perception and conjuration operations
3. Maintaining session state across API calls
4. Following Grimoire's semantic patterns