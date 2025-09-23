import { CodeWithOutputBlock } from '../../components/CodeWithOutputBlock'
import { CommandWithOutputBlock } from '../../components/CommandWithOutputBlock'

export function Deployment() {
  return (
    <div className="space-y-8">
      <section id="deployment">
        <h2 className="text-2xl font-bold text-gray-900 mb-4">Deployment Guide</h2>
        <p className="text-gray-600 mb-6">
          Multiple deployment options available for different environments and requirements.
        </p>

        <div className="space-y-8">
          <div>
            <h3 className="text-xl font-semibold text-gray-900 mb-4">Development Setup</h3>
            
            <div className="space-y-6">
              <CommandWithOutputBlock
                command={`# Option 1: Using Nix (Recommended)
# Enter development environment
nix develop

# Start development servers (frontend on :3000, backend on :8000)
nix run .#dev

# Run all tests
nix run .#test`}
                language="bash"
                output={`warning: Git tree '/home/user/react-fastapi-template' is dirty
• Welcome to React FastAPI Development Environment
• Node.js v20.10.0, Python 3.11.7, npm 10.2.3

Starting development servers...
Frontend server starting on http://localhost:3000
Backend server starting on http://localhost:8000

✓ Frontend ready in 2.3s
✓ Backend ready in 1.8s
✓ WebSocket connection established

All tests passed (24/24)
✓ Frontend tests: 12 passed
✓ Backend tests: 12 passed`}
                isExampleOutput={true}
              />

              <CommandWithOutputBlock
                command={`# Option 2: Using Docker Compose
# Start all services in development mode
docker-compose up

# Run in background
docker-compose up -d

# Stop all services
docker-compose down`}
                language="bash"
                output={`Creating network "react-fastapi_default" with the default driver
Creating react-fastapi_db_1 ... done
Creating react-fastapi_backend_1 ... done
Creating react-fastapi_frontend_1 ... done
Attaching to react-fastapi_db_1, react-fastapi_backend_1, react-fastapi_frontend_1

backend_1   | INFO:     Started server process [1]
backend_1   | INFO:     Waiting for application startup.
backend_1   | INFO:     Application startup complete.
backend_1   | INFO:     Uvicorn running on http://0.0.0.0:8000
frontend_1  | 
frontend_1  |   VITE v5.0.8  ready in 1247 ms
frontend_1  | 
frontend_1  |   ➜  Local:   http://localhost:3000/
frontend_1  |   ➜  Network: http://172.20.0.4:3000/`}
                isExampleOutput={true}
              />

              <CommandWithOutputBlock
                command={`# Option 3: Manual Setup
# Backend setup
cd backend
python -m venv venv
source venv/bin/activate  # On Windows: venv\\Scripts\\activate
pip install -e .

# Frontend setup (in new terminal)
cd frontend
npm install

# Start backend (in backend directory)
uvicorn main:app --reload

# Start frontend (in frontend directory)
npm run dev`}
                language="bash"
                output={`Successfully created virtual environment
Collecting fastapi
Collecting uvicorn[standard]
Collecting sqlalchemy
...
Successfully installed fastapi-0.104.1 uvicorn-0.24.0

added 1342 packages in 23s
npm audit found 0 vulnerabilities

INFO:     Will watch for changes in these directories: ['/app/backend']
INFO:     Uvicorn running on http://127.0.0.1:8000 (Press CTRL+C to quit)
INFO:     Started reloader process [28720]
INFO:     Started server process [28722]

  VITE v5.0.8  ready in 891 ms
  ➜  Local:   http://localhost:3000/`}
                isExampleOutput={true}
              />
            </div>
          </div>

          <div>
            <h3 className="text-xl font-semibold text-gray-900 mb-4">Production Deployment</h3>
            
            <div className="space-y-6">
              <CommandWithOutputBlock
                command={`# Build production Docker image
docker build -f Dockerfile.production -t react-fastapi-app .

# Run production container
docker run -p 8000:8000 \\
  -e SECRET_KEY="your-production-secret-key-here" \\
  -e DATABASE_URL="postgresql://user:pass@host:5432/db" \\
  react-fastapi-app

# Or using docker-compose for production
docker-compose --profile production up app`}
                language="bash"
                output={`[+] Building 142.7s (18/18) FINISHED
 => [internal] load build definition from Dockerfile.production
 => => transferring dockerfile: 1.23kB
 => [internal] load .dockerignore
 => => transferring context: 2B
 => [internal] load metadata for node:18-alpine
 => [internal] load metadata for python:3.11-slim
 => [build-frontend 1/6] FROM node:18-alpine
 => [build-backend 1/7] FROM python:3.11-slim
 => CACHED [build-frontend 2/6] WORKDIR /app
 => [build-frontend 3/6] COPY frontend/package*.json ./
 => [build-frontend 4/6] RUN npm ci --only=production
 => [build-frontend 5/6] COPY frontend/ .
 => [build-frontend 6/6] RUN npm run build
 => [build-backend 2/7] WORKDIR /app
 => [build-backend 3/7] COPY backend/pyproject.toml ./
 => [build-backend 4/7] RUN pip install .
 => [build-backend 5/7] COPY backend/ .
 => [final 1/3] FROM python:3.11-slim
 => [final 2/3] COPY --from=build-backend /app /app
 => [final 3/3] COPY --from=build-frontend /app/dist /app/static
 => exporting to image
Successfully tagged react-fastapi-app:latest

Starting react-fastapi-app...
INFO:     Started server process [1]
INFO:     Waiting for application startup.
INFO:     Application startup complete.
INFO:     Uvicorn running on http://0.0.0.0:8000`}
                isExampleOutput={true}
              />

              <CommandWithOutputBlock
                command={`# Nix production build
nix build .

# Run production app
nix run .#run

# Or build and copy to production server
nix build .
cp -r result/* /opt/react-fastapi-app/`}
                language="bash"
                output={`warning: Git tree '/home/user/react-fastapi-template' is dirty
building the system...
these 127 derivations will be built:
  /nix/store/abc123-react-fastapi-frontend.drv
  /nix/store/def456-react-fastapi-backend.drv
  /nix/store/ghi789-react-fastapi-app.drv
...
building '/nix/store/abc123-react-fastapi-frontend.drv'...
npm run build
> frontend@0.1.0 build
> vite build
✓ 1247 modules transformed.
dist/index.html                   0.46 kB │ gzip:  0.30 kB
dist/assets/index-abc123.js       142.34 kB │ gzip: 46.87 kB
dist/assets/index-def456.css      8.23 kB │ gzip:  2.10 kB
✓ built in 3.42s

building '/nix/store/def456-react-fastapi-backend.drv'...
Building backend application...
✓ Backend build complete

/nix/store/jkl012-react-fastapi-app

Production server starting on http://0.0.0.0:8000
INFO:     Application startup complete.`}
                isExampleOutput={true}
              />
            </div>
          </div>

          <div>
            <h3 className="text-xl font-semibold text-gray-900 mb-4">Environment Configuration</h3>
            
            <div className="space-y-6">
              <CodeWithOutputBlock
                code={`# Production environment variables
SECRET_KEY=your-super-secret-key-change-this-in-production
ACCESS_TOKEN_EXPIRE_MINUTES=30
DATABASE_URL=postgresql://username:password@localhost:5432/myapp
CORS_ORIGINS=https://yourdomain.com,https://www.yourdomain.com

# Optional configurations
ENVIRONMENT=production
LOG_LEVEL=info
SENTRY_DSN=https://your-sentry-dsn@sentry.io/project-id`}
                language="bash"
                filePath=".env"
                output={`Environment configuration loaded successfully:
✓ SECRET_KEY: Set (64 characters)
✓ ACCESS_TOKEN_EXPIRE_MINUTES: 30
✓ DATABASE_URL: postgresql://***:***@localhost:5432/myapp
✓ CORS_ORIGINS: 2 origins configured
✓ ENVIRONMENT: production
✓ LOG_LEVEL: info
✓ SENTRY_DSN: Configured

All required environment variables are set.
Production mode enabled.`}
                isExampleOutput={true}
              />

              <CodeWithOutputBlock
                code={`# Nginx reverse proxy configuration
server {
    listen 80;
    server_name yourdomain.com;
    return 301 https://$server_name$request_uri;
}

server {
    listen 443 ssl;
    server_name yourdomain.com;
    
    ssl_certificate /path/to/cert.pem;
    ssl_certificate_key /path/to/key.pem;
    
    location / {
        proxy_pass http://localhost:8000;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
    
    # WebSocket support
    location /ws {
        proxy_pass http://localhost:8000;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
    }
}`}
                language="nginx"
                filePath="nginx.conf"
                output={`nginx: the configuration file /etc/nginx/nginx.conf syntax is ok
nginx: configuration file /etc/nginx/nginx.conf test is successful

Reloading nginx configuration...
✓ SSL certificate valid (expires: 2025-03-15)
✓ HTTP to HTTPS redirect configured
✓ Reverse proxy to localhost:8000 configured
✓ WebSocket support enabled
✓ Security headers configured

Nginx successfully reloaded with new configuration.`}
                isExampleOutput={true}
              />
            </div>
          </div>

          <div>
            <h3 className="text-xl font-semibold text-gray-900 mb-4">Cloud Deployment</h3>
            
            <div className="space-y-6">
              <CodeWithOutputBlock
                code={`# Docker Compose for cloud deployment
version: '3.8'

services:
  app:
    build:
      context: .
      dockerfile: Dockerfile.production
    ports:
      - "8000:8000"
    environment:
      - SECRET_KEY=\${SECRET_KEY}
      - DATABASE_URL=\${DATABASE_URL}
      - CORS_ORIGINS=\${CORS_ORIGINS}
    depends_on:
      - db
    restart: unless-stopped

  db:
    image: postgres:15
    environment:
      - POSTGRES_DB=myapp
      - POSTGRES_USER=\${POSTGRES_USER}
      - POSTGRES_PASSWORD=\${POSTGRES_PASSWORD}
    volumes:
      - postgres_data:/var/lib/postgresql/data
    restart: unless-stopped

  nginx:
    image: nginx:alpine
    ports:
      - "80:80"
      - "443:443"
    volumes:
      - ./nginx.conf:/etc/nginx/nginx.conf
      - ./ssl:/etc/ssl
    depends_on:
      - app
    restart: unless-stopped

volumes:
  postgres_data:`}
                language="yaml"
                filePath="docker-compose.prod.yml"
                output={`Creating network "react-fastapi_default" with driver "bridge"
Creating volume "react-fastapi_postgres_data" with default driver
Pulling db (postgres:15)...
15: Pulling from library/postgres
a2abf6c4d29d: Pull complete
b4c1b58b3e2a: Pull complete
...
Status: Downloaded newer image for postgres:15
Building app
[+] Building 156.3s (18/18) FINISHED
Creating react-fastapi_db_1 ... done
Creating react-fastapi_app_1 ... done
Creating react-fastapi_nginx_1 ... done

app_1    | INFO:     Started server process [1]
app_1    | INFO:     Application startup complete.
db_1     | PostgreSQL init process complete; ready for start up.
db_1     | LOG:  database system is ready to accept connections
nginx_1  | /docker-entrypoint.sh: Configuration complete; ready for start up

✓ All services started successfully
✓ Application available at https://yourdomain.com
✓ Database connection established
✓ SSL/TLS configured`}
                isExampleOutput={true}
              />
            </div>
          </div>

          <div className="bg-amber-50 border border-amber-200 rounded-lg p-6">
            <h3 className="font-semibold text-amber-900 mb-3">📋 Production Checklist</h3>
            <ul className="space-y-2 text-amber-800">
              <li>✅ Set secure SECRET_KEY environment variable</li>
              <li>✅ Configure proper CORS origins</li>
              <li>✅ Set up production database (PostgreSQL recommended)</li>
              <li>✅ Configure reverse proxy (nginx/traefik)</li>
              <li>✅ Set up SSL/TLS certificates</li>
              <li>✅ Configure logging and monitoring</li>
              <li>✅ Set up backup strategy</li>
              <li>✅ Configure error tracking (Sentry)</li>
              <li>✅ Set up health checks</li>
              <li>✅ Configure auto-restart policies</li>
            </ul>
          </div>
        </div>
      </section>
    </div>
  )
}