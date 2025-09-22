import { useState } from 'react'
import { clsx } from 'clsx'
import { ChevronRight, Code, FileText, Book, Play } from 'lucide-react'
import { CodeBlock } from '../components/CodeBlock'

interface TabContent {
  id: string
  label: string
  icon: React.ComponentType<{ className?: string }>
}

const tabs: TabContent[] = [
  { id: 'overview', label: 'Overview', icon: Book },
  { id: 'api', label: 'API Reference', icon: Code },
  { id: 'examples', label: 'Code Examples', icon: Play },
  { id: 'deployment', label: 'Deployment', icon: FileText },
]

const sidebarLinks = [
  { title: 'Quick Start', href: '#quick-start' },
  { title: 'Authentication', href: '#authentication' },
  { title: 'API Endpoints', href: '#api-endpoints' },
  { title: 'WebSocket', href: '#websocket' },
  { title: 'Deployment', href: '#deployment' },
]

const tips = [
  'Use JWT tokens for API authentication',
  'WebSocket connection auto-reconnects',
  'All API responses include error details',
  'Use Nix for reproducible development',
]

export function Documentation() {
  const [activeTab, setActiveTab] = useState('overview')

  const scrollToSection = (href: string) => {
    const element = document.querySelector(href)
    if (element) {
      element.scrollIntoView({ behavior: 'smooth' })
    }
  }

  return (
    <div className="min-h-screen bg-gray-50">
      {/* Header */}
      <div className="bg-white border-b border-gray-200">
        <div className="mx-auto max-w-7xl px-4 sm:px-6 lg:px-8 py-8">
          <div className="flex items-center space-x-2 text-sm text-gray-500 mb-4">
            <span>Documentation</span>
            <ChevronRight className="h-4 w-4" />
            <span className="text-gray-900 font-medium">React FastAPI Template</span>
          </div>
          <h1 className="text-4xl font-bold text-gray-900 mb-4">
            React FastAPI Template Documentation
          </h1>
          <p className="text-xl text-gray-600 max-w-3xl">
            A comprehensive guide to building modern fullstack applications with React TypeScript frontend 
            and FastAPI Python backend. Complete with authentication, real-time features, and deployment options.
          </p>
        </div>
      </div>

      <div className="mx-auto max-w-7xl px-4 sm:px-6 lg:px-8 py-8">
        <div className="flex flex-col lg:flex-row gap-8">
          {/* Sidebar */}
          <div className="lg:w-64 flex-shrink-0">
            <div className="sticky top-8 space-y-6">
              {/* Quick Links */}
              <div className="bg-white rounded-lg border border-gray-200 p-4">
                <h3 className="font-semibold text-gray-900 mb-3">Quick Links</h3>
                <nav className="space-y-2">
                  {sidebarLinks.map((link) => (
                    <button
                      key={link.href}
                      onClick={() => scrollToSection(link.href)}
                      className="block w-full text-left text-sm text-gray-600 hover:text-gray-900 transition-colors"
                    >
                      {link.title}
                    </button>
                  ))}
                </nav>
              </div>

              {/* Tips */}
              <div className="bg-blue-50 rounded-lg border border-blue-200 p-4">
                <h3 className="font-semibold text-blue-900 mb-3">💡 Tips</h3>
                <ul className="space-y-2">
                  {tips.map((tip, index) => (
                    <li key={index} className="text-sm text-blue-800">
                      {tip}
                    </li>
                  ))}
                </ul>
              </div>
            </div>
          </div>

          {/* Main Content */}
          <div className="flex-1 min-w-0">
            {/* Tab Navigation */}
            <div className="bg-white rounded-lg border border-gray-200 mb-8">
              <div className="border-b border-gray-200">
                <nav className="flex space-x-8 px-6">
                  {tabs.map((tab) => {
                    const Icon = tab.icon
                    return (
                      <button
                        key={tab.id}
                        onClick={() => setActiveTab(tab.id)}
                        className={clsx(
                          'flex items-center space-x-2 py-4 border-b-2 font-medium text-sm transition-colors',
                          activeTab === tab.id
                            ? 'border-indigo-500 text-indigo-600'
                            : 'border-transparent text-gray-500 hover:text-gray-700 hover:border-gray-300'
                        )}
                      >
                        <Icon className="h-4 w-4" />
                        <span>{tab.label}</span>
                      </button>
                    )
                  })}
                </nav>
              </div>

              {/* Tab Content */}
              <div className="p-6">
                {activeTab === 'overview' && <OverviewTab />}
                {activeTab === 'api' && <ApiReferenceTab />}
                {activeTab === 'examples' && <CodeExamplesTab />}
                {activeTab === 'deployment' && <DeploymentTab />}
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  )
}

function OverviewTab() {
  return (
    <div className="space-y-8">
      <section id="quick-start">
        <h2 className="text-2xl font-bold text-gray-900 mb-4">Project Overview</h2>
        <p className="text-gray-600 mb-6">
          This template provides a production-ready fullstack web application combining React with TypeScript 
          for the frontend and FastAPI with Python for the backend. It includes JWT authentication, real-time 
          WebSocket communication, comprehensive testing, and multiple deployment options.
        </p>

        <div className="grid md:grid-cols-2 gap-6 mb-8">
          <div className="bg-blue-50 border border-blue-200 rounded-lg p-4">
            <h3 className="font-semibold text-blue-900 mb-2">Frontend Features</h3>
            <ul className="text-sm text-blue-800 space-y-1">
              <li>• React 18 with TypeScript</li>
              <li>• Tailwind CSS for styling</li>
              <li>• React Router for navigation</li>
              <li>• Axios for HTTP requests</li>
              <li>• WebSocket integration</li>
              <li>• Responsive design</li>
            </ul>
          </div>
          <div className="bg-green-50 border border-green-200 rounded-lg p-4">
            <h3 className="font-semibold text-green-900 mb-2">Backend Features</h3>
            <ul className="text-sm text-green-800 space-y-1">
              <li>• FastAPI with async/await</li>
              <li>• SQLAlchemy ORM with SQLite</li>
              <li>• JWT authentication</li>
              <li>• Pydantic validation</li>
              <li>• WebSocket support</li>
              <li>• Automatic API docs</li>
            </ul>
          </div>
        </div>

        <h3 className="text-xl font-semibold text-gray-900 mb-4">Project Structure</h3>
        <CodeBlock
          code={`react-fastapi-template/
├── backend/                 # FastAPI backend
│   ├── main.py             # FastAPI application
│   ├── pyproject.toml      # Python dependencies
│   └── Dockerfile          # Backend container
├── frontend/               # React frontend
│   ├── src/
│   │   ├── components/     # Reusable UI components
│   │   ├── pages/          # Page components
│   │   ├── types/          # TypeScript types
│   │   └── utils/          # Utilities and helpers
│   ├── package.json        # Node.js dependencies
│   └── vite.config.ts      # Vite configuration
├── shared/                 # Shared TypeScript types
├── tests/                  # Backend tests
├── flake.nix              # Nix build configuration
├── docker-compose.yml     # Container orchestration
└── README.md              # Documentation`}
          language="bash"
          title="Project Structure"
        />

        <h3 className="text-xl font-semibold text-gray-900 mb-4 mt-8">Architecture</h3>
        <p className="text-gray-600 mb-4">
          The application follows a clean separation between frontend and backend with well-defined APIs:
        </p>
        <ul className="list-disc list-inside text-gray-600 space-y-2">
          <li><strong>Frontend:</strong> React SPA with TypeScript, handling UI state and user interactions</li>
          <li><strong>Backend:</strong> FastAPI REST API with WebSocket support for real-time features</li>
          <li><strong>Database:</strong> SQLite with SQLAlchemy ORM for development, easily configurable for production</li>
          <li><strong>Authentication:</strong> JWT bearer tokens with secure password hashing</li>
          <li><strong>Real-time:</strong> WebSocket connection for live updates and notifications</li>
        </ul>
      </section>
    </div>
  )
}

function ApiReferenceTab() {
  const endpoints = [
    {
      method: 'POST',
      path: '/api/register',
      description: 'Register a new user account',
      auth: false,
    },
    {
      method: 'POST',
      path: '/api/login',
      description: 'Login and receive JWT access token',
      auth: false,
    },
    {
      method: 'GET',
      path: '/api/me',
      description: 'Get current user information',
      auth: true,
    },
    {
      method: 'GET',
      path: '/api/tasks',
      description: 'Get all tasks for current user',
      auth: true,
    },
    {
      method: 'POST',
      path: '/api/tasks',
      description: 'Create a new task',
      auth: true,
    },
    {
      method: 'GET',
      path: '/api/tasks/{task_id}',
      description: 'Get specific task by ID',
      auth: true,
    },
    {
      method: 'PUT',
      path: '/api/tasks/{task_id}',
      description: 'Update existing task',
      auth: true,
    },
    {
      method: 'DELETE',
      path: '/api/tasks/{task_id}',
      description: 'Delete task by ID',
      auth: true,
    },
    {
      method: 'GET',
      path: '/api/dashboard/stats',
      description: 'Get dashboard statistics',
      auth: false,
    },
    {
      method: 'GET',
      path: '/api/health',
      description: 'Health check endpoint',
      auth: false,
    },
  ]

  const getMethodColor = (method: string) => {
    const colors: Record<string, string> = {
      GET: 'bg-green-100 text-green-800',
      POST: 'bg-blue-100 text-blue-800',
      PUT: 'bg-yellow-100 text-yellow-800',
      DELETE: 'bg-red-100 text-red-800',
    }
    return colors[method] || 'bg-gray-100 text-gray-800'
  }

  return (
    <div className="space-y-8">
      <section id="api-endpoints">
        <h2 className="text-2xl font-bold text-gray-900 mb-4">API Reference</h2>
        <p className="text-gray-600 mb-6">
          Complete reference for all available API endpoints. All endpoints return JSON responses.
        </p>

        <div className="space-y-4">
          {endpoints.map((endpoint, index) => (
            <div key={index} className="bg-white border border-gray-200 rounded-lg p-4">
              <div className="flex items-center justify-between mb-2">
                <div className="flex items-center space-x-3">
                  <span className={clsx(
                    'inline-flex items-center px-2 py-1 rounded text-xs font-medium',
                    getMethodColor(endpoint.method)
                  )}>
                    {endpoint.method}
                  </span>
                  <code className="text-sm font-mono text-gray-900">{endpoint.path}</code>
                </div>
                {endpoint.auth && (
                  <span className="inline-flex items-center px-2 py-1 rounded text-xs font-medium bg-orange-100 text-orange-800">
                    🔒 Auth Required
                  </span>
                )}
              </div>
              <p className="text-gray-600">{endpoint.description}</p>
            </div>
          ))}
        </div>

        <div className="bg-yellow-50 border border-yellow-200 rounded-lg p-4 mt-6">
          <h3 className="font-semibold text-yellow-900 mb-2">Authentication</h3>
          <p className="text-yellow-800 text-sm">
            Endpoints marked with 🔒 require a valid JWT token in the Authorization header: 
            <code className="bg-yellow-100 px-1 rounded ml-1">Authorization: Bearer &lt;token&gt;</code>
          </p>
        </div>

        <div className="bg-blue-50 border border-blue-200 rounded-lg p-4 mt-4">
          <h3 className="font-semibold text-blue-900 mb-2">Base URL</h3>
          <p className="text-blue-800 text-sm">
            Development: <code className="bg-blue-100 px-1 rounded">http://localhost:8000</code><br />
            All API endpoints are prefixed with <code className="bg-blue-100 px-1 rounded">/api</code>
          </p>
        </div>
      </section>
    </div>
  )
}

function CodeExamplesTab() {
  return (
    <div className="space-y-8">
      <section>
        <h2 className="text-2xl font-bold text-gray-900 mb-4">Code Examples</h2>
        <p className="text-gray-600 mb-6">
          Interactive code examples showing how to use the API and implement common patterns.
        </p>

        <div className="space-y-8">
          <div id="authentication">
            <h3 className="text-xl font-semibold text-gray-900 mb-4">Authentication Examples</h3>
            
            <div className="space-y-6">
              <CodeBlock
                code={`// Register a new user
const registerUser = async (userData) => {
  try {
    const response = await fetch('http://localhost:8000/api/register', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        username: userData.username,
        email: userData.email,
        password: userData.password
      })
    });
    
    if (!response.ok) {
      throw new Error('Registration failed');
    }
    
    const user = await response.json();
    console.log('User registered:', user);
    return user;
  } catch (error) {
    console.error('Registration error:', error);
    throw error;
  }
};`}
                language="javascript"
                title="User Registration"
                executable={true}
              />

              <CodeBlock
                code={`// Login and get JWT token
const loginUser = async (username, password) => {
  try {
    const formData = new FormData();
    formData.append('username', username);
    formData.append('password', password);
    
    const response = await fetch('http://localhost:8000/api/login', {
      method: 'POST',
      body: formData
    });
    
    if (!response.ok) {
      throw new Error('Login failed');
    }
    
    const tokenData = await response.json();
    
    // Store token in localStorage
    localStorage.setItem('access_token', tokenData.access_token);
    
    return tokenData;
  } catch (error) {
    console.error('Login error:', error);
    throw error;
  }
};`}
                language="javascript"
                title="User Login"
                executable={true}
              />

              <CodeBlock
                code={`// Make authenticated API requests
const makeAuthenticatedRequest = async (url, options = {}) => {
  const token = localStorage.getItem('access_token');
  
  if (!token) {
    throw new Error('No authentication token found');
  }
  
  const response = await fetch(url, {
    ...options,
    headers: {
      'Authorization': \`Bearer \${token}\`,
      'Content-Type': 'application/json',
      ...options.headers
    }
  });
  
  if (response.status === 401) {
    // Token expired or invalid
    localStorage.removeItem('access_token');
    throw new Error('Authentication failed');
  }
  
  return response;
};

// Example: Get current user
const getCurrentUser = async () => {
  const response = await makeAuthenticatedRequest('http://localhost:8000/api/me');
  return response.json();
};`}
                language="javascript"
                title="Authenticated Requests"
                executable={true}
              />
            </div>
          </div>

          <div>
            <h3 className="text-xl font-semibold text-gray-900 mb-4">Task Management Examples</h3>
            
            <div className="space-y-6">
              <CodeBlock
                code={`// Create a new task
const createTask = async (taskData) => {
  const response = await makeAuthenticatedRequest('http://localhost:8000/api/tasks', {
    method: 'POST',
    body: JSON.stringify({
      title: taskData.title,
      description: taskData.description
    })
  });
  
  return response.json();
};

// Get all tasks
const getTasks = async () => {
  const response = await makeAuthenticatedRequest('http://localhost:8000/api/tasks');
  return response.json();
};

// Update task
const updateTask = async (taskId, updates) => {
  const response = await makeAuthenticatedRequest(\`http://localhost:8000/api/tasks/\${taskId}\`, {
    method: 'PUT',
    body: JSON.stringify(updates)
  });
  
  return response.json();
};`}
                language="javascript"
                title="Task CRUD Operations"
                executable={true}
              />
            </div>
          </div>

          <div id="websocket">
            <h3 className="text-xl font-semibold text-gray-900 mb-4">WebSocket Connection</h3>
            
            <div className="space-y-6">
              <CodeBlock
                code={`// WebSocket connection with auto-reconnection
class WebSocketManager {
  constructor(url) {
    this.url = url;
    this.ws = null;
    this.reconnectAttempts = 0;
    this.maxReconnectAttempts = 5;
    this.reconnectInterval = 1000;
    this.messageHandlers = new Map();
  }
  
  connect() {
    try {
      this.ws = new WebSocket(this.url);
      
      this.ws.onopen = () => {
        console.log('WebSocket connected');
        this.reconnectAttempts = 0;
      };
      
      this.ws.onmessage = (event) => {
        try {
          const message = JSON.parse(event.data);
          this.handleMessage(message);
        } catch (error) {
          console.error('Failed to parse WebSocket message:', error);
        }
      };
      
      this.ws.onclose = () => {
        console.log('WebSocket disconnected');
        this.reconnect();
      };
      
      this.ws.onerror = (error) => {
        console.error('WebSocket error:', error);
      };
      
    } catch (error) {
      console.error('Failed to create WebSocket connection:', error);
      this.reconnect();
    }
  }
  
  reconnect() {
    if (this.reconnectAttempts < this.maxReconnectAttempts) {
      this.reconnectAttempts++;
      console.log(\`Reconnecting... Attempt \${this.reconnectAttempts}\`);
      
      setTimeout(() => {
        this.connect();
      }, this.reconnectInterval * this.reconnectAttempts);
    }
  }
  
  handleMessage(message) {
    const handler = this.messageHandlers.get(message.type);
    if (handler) {
      handler(message.data);
    }
  }
  
  onMessage(type, handler) {
    this.messageHandlers.set(type, handler);
  }
  
  send(message) {
    if (this.ws && this.ws.readyState === WebSocket.OPEN) {
      this.ws.send(JSON.stringify(message));
    }
  }
}

// Usage
const wsManager = new WebSocketManager('ws://localhost:8000/ws');

// Handle different message types
wsManager.onMessage('task_created', (data) => {
  console.log('New task created:', data);
});

wsManager.onMessage('task_updated', (data) => {
  console.log('Task updated:', data);
});

wsManager.connect();`}
                language="javascript"
                title="WebSocket Integration"
                executable={true}
              />
            </div>
          </div>

          <div>
            <h3 className="text-xl font-semibold text-gray-900 mb-4">React Component Examples</h3>
            
            <div className="space-y-6">
              <CodeBlock
                code={`import { useState, useEffect } from 'react';

function TaskList() {
  const [tasks, setTasks] = useState([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);

  useEffect(() => {
    loadTasks();
  }, []);

  const loadTasks = async () => {
    try {
      setLoading(true);
      const response = await makeAuthenticatedRequest('/api/tasks');
      const tasksData = await response.json();
      setTasks(tasksData);
    } catch (err) {
      setError(err.message);
    } finally {
      setLoading(false);
    }
  };

  const handleTaskToggle = async (taskId, completed) => {
    try {
      const response = await makeAuthenticatedRequest(\`/api/tasks/\${taskId}\`, {
        method: 'PUT',
        body: JSON.stringify({ completed: !completed })
      });
      
      const updatedTask = await response.json();
      
      setTasks(tasks.map(task => 
        task.id === taskId ? updatedTask : task
      ));
    } catch (err) {
      console.error('Failed to update task:', err);
    }
  };

  if (loading) return <div>Loading tasks...</div>;
  if (error) return <div>Error: {error}</div>;

  return (
    <div className="space-y-2">
      {tasks.map(task => (
        <div key={task.id} className="flex items-center space-x-2 p-2 border rounded">
          <input
            type="checkbox"
            checked={task.completed}
            onChange={() => handleTaskToggle(task.id, task.completed)}
          />
          <span className={task.completed ? 'line-through' : ''}>
            {task.title}
          </span>
        </div>
      ))}
    </div>
  );
}`}
                language="tsx"
                title="React Task Component"
                executable={true}
              />
            </div>
          </div>
        </div>
      </section>
    </div>
  )
}

function DeploymentTab() {
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
              <CodeBlock
                code={`# Option 1: Using Nix (Recommended)
# Enter development environment
nix develop

# Start development servers (frontend on :3000, backend on :8000)
nix run .#dev

# Run all tests
nix run .#test`}
                language="bash"
                title="Nix Development Setup"
                executable={true}
              />

              <CodeBlock
                code={`# Option 2: Using Docker Compose
# Start all services in development mode
docker-compose up

# Run in background
docker-compose up -d

# Stop all services
docker-compose down`}
                language="bash"
                title="Docker Development Setup"
                executable={true}
              />

              <CodeBlock
                code={`# Option 3: Manual Setup
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
                title="Manual Development Setup"
                executable={true}
              />
            </div>
          </div>

          <div>
            <h3 className="text-xl font-semibold text-gray-900 mb-4">Production Deployment</h3>
            
            <div className="space-y-6">
              <CodeBlock
                code={`# Build production Docker image
docker build -f Dockerfile.production -t react-fastapi-app .

# Run production container
docker run -p 8000:8000 \\
  -e SECRET_KEY="your-production-secret-key-here" \\
  -e DATABASE_URL="postgresql://user:pass@host:5432/db" \\
  react-fastapi-app

# Or using docker-compose for production
docker-compose --profile production up app`}
                language="bash"
                title="Docker Production Deployment"
                executable={true}
              />

              <CodeBlock
                code={`# Nix production build
nix build .

# Run production app
nix run .#run

# Or build and copy to production server
nix build .
cp -r result/* /opt/react-fastapi-app/`}
                language="bash"
                title="Nix Production Build"
                executable={true}
              />
            </div>
          </div>

          <div>
            <h3 className="text-xl font-semibold text-gray-900 mb-4">Environment Configuration</h3>
            
            <div className="space-y-6">
              <CodeBlock
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
                title="Environment Variables (.env)"
              />

              <CodeBlock
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
                title="Nginx Configuration"
              />
            </div>
          </div>

          <div>
            <h3 className="text-xl font-semibold text-gray-900 mb-4">Cloud Deployment</h3>
            
            <div className="space-y-6">
              <CodeBlock
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
                title="Production Docker Compose"
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