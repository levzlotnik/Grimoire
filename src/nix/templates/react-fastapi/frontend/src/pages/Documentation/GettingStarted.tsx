import { CodeWithOutputBlock } from '../../components/CodeWithOutputBlock'
import { CommandWithOutputBlock } from '../../components/CommandWithOutputBlock'

export function GettingStarted() {
  return (
    <div className="space-y-8">
      <section>
        <h2 className="text-2xl font-bold text-gray-900 mb-4">Getting Started</h2>
        <p className="text-gray-600 mb-6">
          This guide will help you set up and run the React FastAPI template locally. 
          Choose your preferred development environment from the options below.
        </p>

        <div className="bg-blue-50 border border-blue-200 rounded-lg p-4 mb-8">
          <h3 className="font-semibold text-blue-900 mb-2">📋 Prerequisites</h3>
          <ul className="text-blue-800 text-sm space-y-1">
            <li>• <strong>For Nix:</strong> Nix package manager with flakes enabled</li>
            <li>• <strong>For Docker:</strong> Docker and Docker Compose</li>
            <li>• <strong>For Manual:</strong> Node.js 18+, Python 3.11+, and npm/yarn</li>
          </ul>
        </div>

        <div className="space-y-8">
          {/* Option 1: Nix (Recommended) */}
          <div>
            <h3 className="text-xl font-semibold text-gray-900 mb-4">Option 1: Using Nix (Recommended)</h3>
            <p className="text-gray-600 mb-4">
              The fastest way to get started. Nix provides a completely reproducible development environment.
            </p>
            
            <div className="space-y-6">
              <CommandWithOutputBlock
                command={`# Clone the repository
git clone <your-repo-url>
cd react-fastapi-template

# Enter the development environment
nix develop`}
                language="bash"
                output={`🎉 Welcome to React FastAPI Template Development Environment!

Available commands:
  - nix run .#dev     # Start development servers
  - nix run .#test    # Run all tests  
  - nix run .#build   # Build for production
  - nix run .#check   # Run linting and type checking

Frontend: http://localhost:3000
Backend: http://localhost:8000
API Docs: http://localhost:8000/docs

Happy coding! 🚀`}
                isExampleOutput={true}
              />

              <CommandWithOutputBlock
                command={`# Start both frontend and backend development servers
nix run .#dev`}
                language="bash"
                output={`Starting React FastAPI Template development servers...

🚀 Backend starting on http://localhost:8000
🎨 Frontend starting on http://localhost:3000

Backend logs:
INFO:     Uvicorn running on http://localhost:8000 (Press CTRL+C to quit)
INFO:     Started reloader process [12345] using WatchFiles
INFO:     Started server process [12346]
INFO:     Waiting for application startup.
INFO:     Application startup complete.

Frontend logs:
  VITE v5.0.0  ready in 342 ms

  ➜  Local:   http://localhost:3000/
  ➜  Network: use --host to expose
  ➜  press h + enter to show help

✅ Both servers are running!`}
                isExampleOutput={true}
              />

              <CommandWithOutputBlock
                command={`# Run tests to verify everything works
nix run .#test`}
                language="bash"
                output={`Running React FastAPI Template test suite...

Backend tests:
================================== test session starts ==================================
platform linux -- Python 3.11.6, pytest-7.4.3, pluggy-1.3.0
rootdir: /home/user/react-fastapi-template
collected 12 items

tests/test_auth.py ....                                               [ 33%]
tests/test_tasks.py ....                                              [ 66%]
tests/test_websocket.py ....                                          [100%]

================================== 12 passed in 2.34s ==================================

Frontend tests:
 ✓ src/components/Button.test.tsx (2)
 ✓ src/stores/authStore.test.ts (3)

Test Files  2 passed (2)
     Tests  5 passed (5)
  Start at  14:32:15
  Duration  1.89s

✅ All tests passed!`}
                isExampleOutput={true}
              />
            </div>
          </div>

          {/* Option 2: Docker */}
          <div>
            <h3 className="text-xl font-semibold text-gray-900 mb-4">Option 2: Using Docker</h3>
            <p className="text-gray-600 mb-4">
              Great for containerized development and testing deployment configurations.
            </p>
            
            <div className="space-y-6">
              <CommandWithOutputBlock
                command={`# Clone and navigate to project
git clone <your-repo-url>
cd react-fastapi-template

# Start all services with docker-compose
docker-compose up`}
                language="bash"
                output={`[+] Running 3/3
 ✔ Network react-fastapi-template_default  Created
 ✔ Container react-fastapi-template-db-1       Started
 ✔ Container react-fastapi-template-backend-1  Started
 ✔ Container react-fastapi-template-frontend-1 Started

Attaching to react-fastapi-template-backend-1, react-fastapi-template-frontend-1
react-fastapi-template-backend-1   | INFO:     Uvicorn running on http://0.0.0.0:8000
react-fastapi-template-frontend-1  | 
react-fastapi-template-frontend-1  |   VITE v5.0.0  ready in 1.2s
react-fastapi-template-frontend-1  | 
react-fastapi-template-frontend-1  |   ➜  Local:   http://localhost:3000/
react-fastapi-template-frontend-1  |   ➜  Network: http://172.18.0.4:3000/

🌐 Frontend: http://localhost:3000
🚀 Backend: http://localhost:8000
📚 API Docs: http://localhost:8000/docs`}
                isExampleOutput={true}
              />

              <CommandWithOutputBlock
                command={`# Run in background (detached mode)
docker-compose up -d

# View logs
docker-compose logs -f

# Stop all services
docker-compose down`}
                language="bash"
                output={`# Starting detached:
[+] Running 3/3
 ✔ Container react-fastapi-template-db-1       Started
 ✔ Container react-fastapi-template-backend-1  Started  
 ✔ Container react-fastapi-template-frontend-1 Started

# Viewing logs:
react-fastapi-template-backend-1   | INFO:     Application startup complete.
react-fastapi-template-frontend-1  | ready in 1.2s

# Stopping services:
[+] Running 4/4
 ✔ Container react-fastapi-template-frontend-1 Removed
 ✔ Container react-fastapi-template-backend-1  Removed
 ✔ Container react-fastapi-template-db-1       Removed
 ✔ Network react-fastapi-template_default      Removed`}
                isExampleOutput={true}
              />
            </div>
          </div>

          {/* Option 3: Manual Setup */}
          <div>
            <h3 className="text-xl font-semibold text-gray-900 mb-4">Option 3: Manual Setup</h3>
            <p className="text-gray-600 mb-4">
              For developers who prefer managing dependencies manually or need custom configurations.
            </p>
            
            <div className="space-y-6">
              <CommandWithOutputBlock
                command={`# Clone the repository
git clone <your-repo-url>
cd react-fastapi-template

# Setup backend
cd backend
python -m venv venv
source venv/bin/activate  # On Windows: venv\\Scripts\\activate
pip install -e .`}
                language="bash"
                output={`Cloning into 'react-fastapi-template'...
remote: Counting objects: 145, done.
remote: Compressing objects: 100% (89/89), done.
remote: Total 145 (delta 45), reused 134 (delta 34)
Receiving objects: 100% (145/145), 1.2 MiB | 2.5 MiB/s, done.
Resolving deltas: 100% (45/45), done.

Creating virtual environment...
Installing dependencies...
Successfully installed fastapi-0.104.1 uvicorn-0.24.0 sqlalchemy-2.0.23 
pydantic-2.5.0 python-multipart-0.0.6 passlib-1.7.4 python-jose-3.3.0 
bcrypt-4.0.1 pytest-7.4.3 httpx-0.25.2

✅ Backend setup complete!`}
                isExampleOutput={true}
              />

              <CommandWithOutputBlock
                command={`# Setup frontend (in a new terminal)
cd frontend
npm install`}
                language="bash"
                output={`added 1247 packages, and audited 1248 packages in 15s

284 packages are looking for funding
  run \`npm fund\` for details

found 0 vulnerabilities

Dependencies installed:
- react: ^18.2.0
- typescript: ^5.0.2  
- vite: ^5.0.0
- tailwindcss: ^3.3.0
- @types/react: ^18.2.15
- axios: ^1.6.0
- react-router-dom: ^6.8.0

✅ Frontend setup complete!`}
                isExampleOutput={true}
              />

              <CommandWithOutputBlock
                command={`# Start backend (from backend directory)
uvicorn main:app --reload --host 0.0.0.0 --port 8000

# In another terminal, start frontend (from frontend directory)  
npm run dev`}
                language="bash"
                output={`Backend server:
INFO:     Will watch for changes in these directories: ['/path/to/backend']
INFO:     Uvicorn running on http://0.0.0.0:8000 (Press CTRL+C to quit)
INFO:     Started reloader process [12345] using WatchFiles
INFO:     Started server process [12346]
INFO:     Waiting for application startup.
INFO:     Application startup complete.

Frontend server:
  VITE v5.0.0  ready in 423 ms

  ➜  Local:   http://localhost:3000/
  ➜  Network: use --host to expose
  ➜  press h + enter to show help

🎯 Ready! Open http://localhost:3000 to view the app`}
                isExampleOutput={true}
              />
            </div>
          </div>

          {/* Next Steps */}
          <div className="bg-green-50 border border-green-200 rounded-lg p-6">
            <h3 className="text-xl font-semibold text-green-900 mb-4">🎉 You're Ready!</h3>
            <p className="text-green-800 mb-4">
              Your development environment is now running. Here's what you can access:
            </p>
            
            <div className="grid md:grid-cols-2 gap-4 mb-6">
              <div className="bg-white border border-green-200 rounded p-4">
                <h4 className="font-semibold text-green-900 mb-2">Frontend</h4>
                <ul className="text-green-800 text-sm space-y-1">
                  <li>🌐 <a href="http://localhost:3000" className="underline">http://localhost:3000</a></li>
                  <li>📱 Responsive React application</li>
                  <li>🎨 Tailwind CSS styling</li>
                  <li>⚡ Hot reload enabled</li>
                </ul>
              </div>
              
              <div className="bg-white border border-green-200 rounded p-4">
                <h4 className="font-semibold text-green-900 mb-2">Backend</h4>
                <ul className="text-green-800 text-sm space-y-1">
                  <li>🚀 <a href="http://localhost:8000" className="underline">http://localhost:8000</a></li>
                  <li>📚 <a href="http://localhost:8000/docs" className="underline">API Documentation</a></li>
                  <li>🔄 Auto-reload on changes</li>
                  <li>🗄️ SQLite database</li>
                </ul>
              </div>
            </div>

            <div className="text-green-800">
              <h4 className="font-semibold mb-2">Next Steps:</h4>
              <ol className="list-decimal list-inside space-y-1 text-sm">
                <li>Open <code className="bg-green-100 px-1 rounded">http://localhost:3000</code> to see the app</li>
                <li>Visit <code className="bg-green-100 px-1 rounded">http://localhost:8000/docs</code> for API documentation</li>
                <li>Try registering a new user account</li>
                <li>Explore the code in <code className="bg-green-100 px-1 rounded">frontend/src/</code> and <code className="bg-green-100 px-1 rounded">backend/</code></li>
                <li>Check out the other documentation tabs for API reference and examples</li>
              </ol>
            </div>
          </div>

          {/* Troubleshooting */}
          <div className="bg-yellow-50 border border-yellow-200 rounded-lg p-6">
            <h3 className="text-lg font-semibold text-yellow-900 mb-3">🔧 Troubleshooting</h3>
            
            <div className="space-y-4 text-yellow-800">
              <div>
                <h4 className="font-medium mb-1">Port already in use (3000 or 8000)</h4>
                <p className="text-sm">Kill existing processes: <code className="bg-yellow-100 px-1 rounded">lsof -ti:3000 | xargs kill -9</code></p>
              </div>
              
              <div>
                <h4 className="font-medium mb-1">Database connection issues</h4>
                <p className="text-sm">Delete <code className="bg-yellow-100 px-1 rounded">backend/app.db</code> to reset the database</p>
              </div>
              
              <div>
                <h4 className="font-medium mb-1">Nix flakes not enabled</h4>
                <p className="text-sm">Add to <code className="bg-yellow-100 px-1 rounded">~/.config/nix/nix.conf</code>: <code className="bg-yellow-100 px-1 rounded">experimental-features = nix-command flakes</code></p>
              </div>
              
              <div>
                <h4 className="font-medium mb-1">Docker permission denied</h4>
                <p className="text-sm">Add user to docker group: <code className="bg-yellow-100 px-1 rounded">sudo usermod -aG docker $USER</code> (requires logout/login)</p>
              </div>
            </div>
          </div>
        </div>
      </section>
    </div>
  )
}