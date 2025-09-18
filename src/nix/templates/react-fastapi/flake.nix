{
  description = "React + FastAPI Fullstack Template";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        # Node.js environment for frontend
        nodejs = pkgs.nodejs_20;
        
        # Python environment for backend
        python = pkgs.python312;
        
        # Backend Python package
        backendPackage = pkgs.python312Packages.buildPythonPackage {
          pname = "react-fastapi-backend";
          version = "1.0.0";
          src = ./backend;
          format = "pyproject";
          
          nativeBuildInputs = with pkgs.python312Packages; [
            setuptools
            wheel
          ];
          
          propagatedBuildInputs = with pkgs.python312Packages; [
            fastapi
            uvicorn
            sqlalchemy
            pydantic
            python-jose
            passlib
            python-multipart
          ];
          
          # Skip tests during build - we'll run them separately
          doCheck = false;
        };
        
        # Python environment with all dependencies for development
        pythonEnv = python.withPackages (ps: with ps; [
          # Backend dependencies
          fastapi
          uvicorn
          sqlalchemy
          pydantic
          python-jose
          passlib
          python-multipart
          # Development and testing dependencies
          pytest
          pytest-asyncio
          httpx
          pytest-cov
          black
          flake8
          mypy
        ]);
        
        # Frontend build
        frontendBuild = pkgs.stdenv.mkDerivation {
          pname = "react-fastapi-frontend";
          version = "1.0.0";
          src = ./frontend;
          
          nativeBuildInputs = [ nodejs pkgs.nodePackages.npm ];
          
          buildPhase = ''
            # Create writable node_modules
            export HOME=$(mktemp -d)
            npm ci --legacy-peer-deps
            
            # Build the frontend
            npm run build
          '';
          
          installPhase = ''
            mkdir -p $out
            cp -r dist/* $out/
          '';
        };
        
        # Combined application script
        appScript = pkgs.writeShellScriptBin "react-fastapi-app" ''
          set -e
          
          echo "🚀 Starting React + FastAPI Application"
          echo ""
          
          # Set environment variables
          export FRONTEND_DIST_PATH="${frontendBuild}"
          export PYTHONPATH="${./backend}:$PYTHONPATH"
          
          # Start the FastAPI server with static file serving
          echo "Starting FastAPI server on http://localhost:8000"
          echo "Frontend will be served at http://localhost:8000"
          echo ""
          
          cd ${./backend}
          ${pythonEnv}/bin/uvicorn main:app --host 0.0.0.0 --port 8000
        '';
        
        # Development script that starts both frontend and backend separately
        devScript = pkgs.writeShellScriptBin "react-fastapi-dev" ''
          set -e
          
          echo "🚀 Starting React + FastAPI Development Environment"
          echo ""
          echo "Backend will start on http://localhost:8000"
          echo "Frontend will start on http://localhost:3000"
          echo ""
          echo "Press Ctrl+C to stop all services"
          echo ""
          
          # Function to kill all background processes
          cleanup() {
            echo "Stopping all services..."
            jobs -p | xargs -r kill
            exit 0
          }
          
          trap cleanup INT TERM
          
          # Start backend
          echo "Starting FastAPI backend..."
          cd ${./backend}
          PYTHONPATH=${./backend} ${pythonEnv}/bin/uvicorn main:app --reload --host 0.0.0.0 --port 8000 &
          BACKEND_PID=$!
          
          # Wait a moment for backend to start
          sleep 2
          
          # Start frontend
          echo "Starting React frontend..."
          cd ${./frontend}
          ${nodejs}/bin/npm run dev &
          FRONTEND_PID=$!
          
          # Wait for both processes
          wait $BACKEND_PID $FRONTEND_PID
        '';
        
        # Backend test script
        backendTestScript = pkgs.writeShellScriptBin "test-backend" ''
          echo "🧪 Running Backend Tests"
          cd ${./backend}
          PYTHONPATH=${./backend} ${pythonEnv}/bin/pytest ${./tests} -v
        '';
        
        # Frontend test script
        frontendTestScript = pkgs.writeShellScriptBin "test-frontend" ''
          echo "🧪 Running Frontend Tests"
          cd ${./frontend}
          ${nodejs}/bin/npm test
        '';
        
        # Combined test script
        testScript = pkgs.writeShellScriptBin "test-all" ''
          echo "🧪 Running All Tests"
          echo ""
          
          echo "Running backend tests..."
          ${backendTestScript}/bin/test-backend
          
          echo ""
          echo "Running frontend tests..."
          ${frontendTestScript}/bin/test-frontend
          
          echo ""
          echo "✅ All tests completed!"
        '';

      in
      {
        # Development environment
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Backend
            pythonEnv
            # Frontend
            nodejs
            nodePackages.npm
            # Development tools
            git
            curl
            jq
            # Grimoire integration
            swipl
          ];
          
          shellHook = ''
            echo "🚀 React + FastAPI Development Environment"
            echo ""
            echo "Commands available:"
            echo "  nix run .#dev      - Start development servers"
            echo "  nix run .#run      - Start production app"
            echo "  nix run .#test     - Run all tests"
            echo "  nix run .#test-backend  - Run backend tests only"
            echo "  nix run .#test-frontend - Run frontend tests only"
            echo ""
            echo "Development setup:"
            echo "  Backend: cd backend && uvicorn main:app --reload"
            echo "  Frontend: cd frontend && npm run dev"
            echo "  Tests: pytest tests/"
            echo ""
            echo "Frontend: React + TypeScript + Vite + Tailwind CSS"
            echo "Backend: FastAPI + SQLAlchemy + JWT Auth + WebSocket"
            echo ""
          '';
        };
        
        # Packages
        packages = {
          default = appScript;
          backend = backendPackage;
          frontend = frontendBuild;
          app = appScript;
        };
        
        # Applications
        apps = {
          default = {
            type = "app";
            program = "${appScript}/bin/react-fastapi-app";
          };
          
          run = {
            type = "app";
            program = "${appScript}/bin/react-fastapi-app";
          };
          
          dev = {
            type = "app";
            program = "${devScript}/bin/react-fastapi-dev";
          };
          
          test = {
            type = "app";
            program = "${testScript}/bin/test-all";
          };
          
          test-backend = {
            type = "app";
            program = "${backendTestScript}/bin/test-backend";
          };
          
          test-frontend = {
            type = "app";
            program = "${frontendTestScript}/bin/test-frontend";
          };
        };
      });
}
