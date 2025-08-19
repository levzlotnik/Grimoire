{
  description = "Python development environment";

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
          pname = "template-python-project";
          version = "0.1.0";
          src = ./.;
          format = "pyproject";

          nativeBuildInputs = with pkgs.python312Packages; [
            setuptools
            wheel
          ];

          # Add runtime dependencies here
          propagatedBuildInputs = with pkgs.python312Packages; [
            # Add your package dependencies here
          ];
        };

        pythonEnv = pkgs.python312.withPackages (ps: with ps; [
          pip
          build
          setuptools
          wheel
          pythonPackage
          # Add your dependencies here
        ]);

        # Script to run the main.py
        runScript = pkgs.writeShellScriptBin "run-python-template" ''
          cd ${./.}
          ${pythonEnv}/bin/python main.py
        '';

      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            pythonEnv
            git
          ];

          shellHook = ''
            echo "🐍 Python Development Environment"
            echo "Python: $(python --version)"
            echo ""
            echo "Run main script: python main.py"
            echo "Build package: python -m build"
            echo "Install: pip install -e ."
          '';
        };

        packages = {
          default = pythonPackage;
          python-package = pythonPackage;
        };

        apps = {
          default = {
            type = "app";
            program = "${runScript}/bin/run-python-template";
          };
          run = {
            type = "app";
            program = "${runScript}/bin/run-python-template";
          };
        };
      });
}
