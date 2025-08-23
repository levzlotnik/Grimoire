{
  description = "Python REST API with FastAPI";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Build the Python package
        pythonPackage = pkgs.python312Packages.buildPythonPackage {
          pname = "python-rest-api";
          version = "0.1.0";
          src = ./.;
          format = "pyproject";

          nativeBuildInputs = with pkgs.python312Packages; [
            setuptools
            wheel
          ];

          propagatedBuildInputs = with pkgs.python312Packages; [
            fastapi
            uvicorn
            pydantic
          ];
        };

        pythonEnv = pkgs.python312.withPackages (ps: with ps; [
          pip
          build
          setuptools
          wheel
          fastapi
          uvicorn
          pydantic
          httpx  # For testing
          pytest
          pytest-asyncio
        ]);

        # Script to run the API server
        runScript = pkgs.writeShellScriptBin "run-api" ''
          cd ${./.}
          ${pythonEnv}/bin/uvicorn main:app --reload --host 0.0.0.0 --port 8000
        '';

      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            pythonEnv
            git
            swipl  # For testing Prolog integration
          ];

          shellHook = ''
            echo "🚀 Python REST API Development Environment"
            echo "Python: $(python --version)"
            echo ""
            echo "Run API server: uvicorn main:app --reload"
            echo "Run tests: pytest"
            echo "API docs: http://localhost:8000/docs"
          '';
        };

        packages = {
          default = pythonPackage;
          python-package = pythonPackage;
        };

        apps = {
          default = {
            type = "app";
            program = "${runScript}/bin/run-api";
          };
          run = {
            type = "app";
            program = "${runScript}/bin/run-api";
          };
          test = {
            type = "app";
            program = "${pkgs.writeShellScriptBin "test-api" ''
              cd ${./.}
              ${pythonEnv}/bin/pytest
            ''}/bin/test-api";
          };
        };
      });
}