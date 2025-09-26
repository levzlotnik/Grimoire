# React + FastAPI Fullstack Template

A simple fullstack web application template combining React (TypeScript) frontend with FastAPI (Python) backend.

## 🚀 Features

### Frontend (React + TypeScript)
- **Modern Stack**: React 19, TypeScript, Vite
- **UI Framework**: Tailwind CSS 4.x with custom components
- **Routing**: React Router with multiple pages
- **Charts**: Recharts for data visualization
- **HTTP Client**: Type-safe API client with fetch
- **State Management**: React useState/useEffect

### Backend (FastAPI + Python)
- **Web Framework**: FastAPI with async/await
- **Database**: SQLAlchemy ORM with SQLite
- **Validation**: Pydantic models
- **API Docs**: Automatic OpenAPI/Swagger docs
- **Dashboard APIs**: Sample data endpoints

### Infrastructure
- **Build System**: Nix flakes for reproducible builds
- **Containerization**: Docker + Docker Compose
- **Hot Reload**: Development servers for both frontend and backend

## 📋 Prerequisites

Choose one of the following:

### Nix (Recommended)
- [Nix](https://nixos.org/download.html) with flakes enabled

### Docker
- [Docker](https://docs.docker.com/get-docker/)
- [Docker Compose](https://docs.docker.com/compose/install/)

## 🛠️ Quick Start

### Using Nix (Recommended)

```bash
# Clone or navigate to the template directory
cd react-fastapi-template

# Enter development environment
nix develop

# Start both frontend and backend together (default)
nix run        # Frontend on :3000, Backend on :8000

# Or use specific commands:
nix run .#dev  # Development servers with hot reload
nix run .#run  # Same as default

# Run all tests
nix run .#test
```

### Using Docker Compose

```bash
# Development mode (separate frontend and backend containers)
docker-compose up

# Run tests
docker-compose exec backend pytest
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
│   │   ├── pages/          # Page components (Landing, Dashboard, etc.)
│   │   ├── services/       # API client
│   │   └── types/          # TypeScript type definitions
│   ├── package.json        # Node.js dependencies
│   ├── vite.config.ts      # Vite configuration
│   └── Dockerfile          # Frontend container
├── tests/                  # Backend tests
├── flake.nix              # Nix build configuration
├── docker-compose.yml     # Container orchestration
├── semantics.pl          # Grimoire semantics
└── README.md            # This file
```

## 🧪 Testing

### Available Commands (Nix)

```bash
nix run                    # Start both services (default)
nix run .#dev              # Start development servers with hot reload
nix run .#run              # Same as default
nix run .#test             # Run all tests
nix run .#test-backend     # Run backend tests only
nix run .#test-frontend    # Run frontend tests only
nix develop                # Enter development shell
```

### Available Commands (Docker)

```bash
docker-compose up                    # Development mode
docker-compose exec backend pytest  # Backend tests
```

## 🎛️ Configuration

### Frontend Configuration (`frontend/vite.config.ts`)
```typescript
export default defineConfig({
  server: {
    proxy: {
      '/api': 'http://localhost:8000',  // Backend API
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

## 📚 Additional Resources

- [FastAPI Documentation](https://fastapi.tiangolo.com/)
- [React Documentation](https://react.dev/)
- [TypeScript Handbook](https://www.typescriptlang.org/docs/)
- [Tailwind CSS](https://tailwindcss.com/docs)
- [Nix Manual](https://nixos.org/manual/nix/stable/)

---

**🎯 Ready to build web applications!** Start with `nix run` and visit `http://localhost:3000` to see your app in action.