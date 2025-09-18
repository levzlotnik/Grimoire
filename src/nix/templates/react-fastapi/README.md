# React + FastAPI Fullstack Template

A modern, production-ready fullstack web application template combining React (TypeScript) frontend with FastAPI (Python) backend. Features JWT authentication, real-time WebSocket communication, comprehensive testing, and multiple deployment options.

## 🚀 Features

### Frontend (React + TypeScript)
- **Modern Stack**: React 18, TypeScript, Vite
- **UI Framework**: Tailwind CSS with custom components
- **State Management**: Zustand for client state
- **Routing**: React Router v6 with protected routes
- **Forms**: React Hook Form with Zod validation
- **HTTP Client**: Axios with interceptors
- **Real-time**: WebSocket integration with auto-reconnection
- **Testing**: Vitest + React Testing Library

### Backend (FastAPI + Python)
- **Web Framework**: FastAPI with async/await
- **Database**: SQLAlchemy ORM with SQLite
- **Authentication**: JWT bearer token auth
- **Password Security**: bcrypt hashing
- **Validation**: Pydantic models
- **Real-time**: WebSocket support
- **Testing**: pytest with TestClient
- **API Docs**: Automatic OpenAPI/Swagger docs

### Infrastructure
- **Build System**: Nix flakes for reproducible builds
- **Containerization**: Docker + Docker Compose
- **Development**: Hot reload for both frontend and backend
- **Production**: Optimized multi-stage Docker builds

## 📋 Prerequisites

Choose one of the following approaches:

### Option A: Nix (Recommended)
- [Nix](https://nixos.org/download.html) with flakes enabled
- [Direnv](https://direnv.net/) (optional, for automatic environment loading)

### Option B: Docker
- [Docker](https://docs.docker.com/get-docker/)
- [Docker Compose](https://docs.docker.com/compose/install/)

### Option C: Manual Setup
- Python 3.11+ with pip
- Node.js 18+ with npm
- Git

## 🛠️ Quick Start

### Using Nix (Recommended)

```bash
# Clone or navigate to the template directory
cd react-fastapi-template

# Enter development environment
nix develop

# Start development servers (frontend on :3000, backend on :8000)
nix run .#dev

# Or start production app (unified on :8000)
nix run .#run

# Run all tests
nix run .#test
```

### Using Docker Compose

```bash
# Development mode (separate frontend and backend containers)
docker-compose up

# Production mode (unified container)
docker-compose --profile production up app

# Run tests
docker-compose exec backend pytest
docker-compose exec frontend npm test
```

### Manual Setup

```bash
# Backend setup
cd backend
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate
pip install -e .

# Frontend setup
cd ../frontend
npm install

# Start backend (in backend directory)
uvicorn main:app --reload

# Start frontend (in frontend directory, new terminal)
npm run dev
```

## 📁 Project Structure

```
react-fastapi-template/
├── backend/                 # FastAPI backend
│   ├── main.py             # FastAPI application
│   ├── pyproject.toml      # Python dependencies
│   └── Dockerfile          # Backend container
├── frontend/               # React frontend
│   ├── src/
│   │   ├── components/     # Reusable UI components
│   │   ├── pages/          # Page components
│   │   ├── stores/         # Zustand state stores
│   │   ├── hooks/          # Custom React hooks
│   │   ├── lib/            # Utilities and API client
│   │   └── test/           # Frontend tests
│   ├── package.json        # Node.js dependencies
│   ├── vite.config.ts      # Vite configuration
│   └── Dockerfile          # Frontend container
├── shared/                 # Shared TypeScript types
│   └── types.ts            # API type definitions
├── tests/                  # Backend tests
│   ├── test_main.py        # API endpoint tests
│   └── test_websocket.py   # WebSocket tests
├── flake.nix              # Nix build configuration
├── docker-compose.yml     # Container orchestration
├── Dockerfile.production  # Production container
├── semantics.pl          # Grimoire semantics
├── semantics.plt         # Grimoire tests
└── README.md            # This file
```

## 🔧 Development

### Available Commands (Nix)

```bash
nix run .#dev              # Start development servers
nix run .#run              # Start production app
nix run .#test             # Run all tests
nix run .#test-backend     # Run backend tests only
nix run .#test-frontend    # Run frontend tests only
nix develop               # Enter development shell
```

### Available Commands (Docker)

```bash
docker-compose up                    # Development mode
docker-compose --profile production up app  # Production mode
docker-compose exec backend pytest          # Backend tests
docker-compose exec frontend npm test       # Frontend tests
```

### Manual Commands

```bash
# Backend (in backend/ directory)
uvicorn main:app --reload           # Development server
pytest ../tests/                   # Run tests
black .                            # Format code
mypy .                             # Type checking

# Frontend (in frontend/ directory)
npm run dev                        # Development server
npm run build                      # Production build
npm test                          # Run tests
npm run lint                      # Lint code
```

## 🔐 Authentication Flow

1. **Registration**: POST `/api/register` with username, email, password
2. **Login**: POST `/api/login` returns JWT access token
3. **Protected Routes**: Include `Authorization: Bearer <token>` header
4. **Token Refresh**: Tokens expire in 30 minutes (configurable)

Example usage:

```bash
# Register user
curl -X POST "http://localhost:8000/api/register" \
  -H "Content-Type: application/json" \
  -d '{"username": "testuser", "email": "test@example.com", "password": "password123"}'

# Login
curl -X POST "http://localhost:8000/api/login" \
  -d "username=testuser&password=password123"

# Use token
curl -H "Authorization: Bearer <token>" "http://localhost:8000/api/me"
```

## 📡 WebSocket Integration

Real-time features using WebSocket connection at `/ws`:

- **Task Creation**: Broadcasts to all connected clients
- **Task Updates**: Real-time status changes
- **Task Deletion**: Immediate removal notifications
- **Auto-reconnection**: Handles connection drops gracefully

Frontend automatically connects and handles reconnection with exponential backoff.

## 🧪 Testing

### Backend Tests
- **Unit Tests**: Individual component testing
- **Integration Tests**: API endpoint testing with TestClient
- **Authentication Tests**: JWT token validation
- **WebSocket Tests**: Real-time communication testing

### Frontend Tests
- **Component Tests**: React component testing with RTL
- **Store Tests**: State management testing
- **Hook Tests**: Custom hook testing
- **Integration Tests**: End-to-end user flows

Run tests:
```bash
# All tests
nix run .#test

# Backend only
nix run .#test-backend
pytest tests/

# Frontend only
nix run .#test-frontend
npm test
```

## 🚀 Deployment

### Production Deployment (Docker)

```bash
# Build production image
docker build -f Dockerfile.production -t react-fastapi-app .

# Run production container
docker run -p 8000:8000 \
  -e SECRET_KEY="your-production-secret-key" \
  react-fastapi-app
```

### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `SECRET_KEY` | `your-secret-key-here-change-in-production` | JWT signing key |
| `ACCESS_TOKEN_EXPIRE_MINUTES` | `30` | Token expiration time |
| `DATABASE_URL` | `sqlite:///./tasks.db` | Database connection string |

### Production Checklist

- [ ] Set secure `SECRET_KEY` environment variable
- [ ] Configure proper CORS origins
- [ ] Set up proper database (PostgreSQL recommended for production)
- [ ] Configure reverse proxy (nginx/traefik)
- [ ] Set up SSL/TLS certificates
- [ ] Configure logging and monitoring
- [ ] Set up backup strategy

## 🏗️ Architecture

### Frontend Architecture
```
React App
├── Routing (React Router)
├── State Management (Zustand)
├── HTTP Client (Axios)
├── WebSocket Client
└── UI Components (Tailwind CSS)
```

### Backend Architecture
```
FastAPI App
├── Authentication (JWT)
├── Database (SQLAlchemy + SQLite)
├── WebSocket Manager
├── API Routes
└── Static File Serving
```

### Data Flow
1. Frontend sends HTTP requests to `/api/*`
2. Backend validates JWT tokens
3. Database operations via SQLAlchemy ORM
4. Real-time updates via WebSocket
5. Frontend updates UI state

## 🎛️ Configuration

### Frontend Configuration (`frontend/vite.config.ts`)
```typescript
export default defineConfig({
  server: {
    proxy: {
      '/api': 'http://localhost:8000',  // Backend API
      '/ws': {                          // WebSocket
        target: 'ws://localhost:8000',
        ws: true,
      },
    },
  },
});
```

### Backend Configuration (`backend/main.py`)
```python
app.add_middleware(
    CORSMiddleware,
    allow_origins=["http://localhost:3000"],  # Frontend dev server
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)
```

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch: `git checkout -b feature-name`
3. Make changes and add tests
4. Run tests: `nix run .#test`
5. Commit changes: `git commit -am 'Add feature'`
6. Push to branch: `git push origin feature-name`
7. Submit a Pull Request

## 📚 Additional Resources

- [FastAPI Documentation](https://fastapi.tiangolo.com/)
- [React Documentation](https://react.dev/)
- [TypeScript Handbook](https://www.typescriptlang.org/docs/)
- [Tailwind CSS](https://tailwindcss.com/docs)
- [Nix Manual](https://nixos.org/manual/nix/stable/)
- [Docker Documentation](https://docs.docker.com/)

## 📄 License

This template is provided under the MIT License. See the LICENSE file for details.

---

**🎯 Ready to build modern web applications!** Start with `nix run .#dev` and visit `http://localhost:3000` to see your app in action.