{
  description = "Grimoire API Server Python Package";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    grimoire-flake.url = "path:../../..";
  };

  outputs = { self, nixpkgs, flake-utils, grimoire-flake }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Get grimoireEnv from parent flake
        grimoireEnv = grimoire-flake.lib.mkGrimoireEnv pkgs;

        # Build the Python package
        grimoireApiPackage = pkgs.python313Packages.buildPythonPackage {
          pname = "grimoire-api";
          version = "0.1.0";
          src = ./.;
          format = "pyproject";

          nativeBuildInputs = with pkgs.python313Packages; [
            setuptools
            wheel
          ];

          propagatedBuildInputs = with pkgs.python313Packages; [
            fastapi
            uvicorn
            pydantic
            mcp
            # Include janus-swi from grimoireEnv
            grimoireEnv.janus-swi
          ];

          # Set build environment
          SWIPL_BIN = "${grimoireEnv.swipl}/bin/swipl";
          LLM_DB_SCHEMA_PATH = grimoireEnv.env.LLM_DB_SCHEMA_PATH;
        };


      in
      {
        packages = {
          default = grimoireApiPackage;
          python-package = grimoireApiPackage;
        };

        checks = {
          tests = pkgs.runCommand "grimoire-api-tests" (grimoireEnv.env // {
            buildInputs = grimoireEnv.buildInputs;
          }) ''
            cd ${./.}
            ${grimoireEnv.python}/bin/pytest tests/ -v
            touch $out
          '';
        };

        apps = {
          test = {
            type = "app";
            program = "${grimoireEnv.mkGrimoireExecutable {
              name = "test-api";
              script = ''
                cd ${./.}
                exec ${grimoireEnv.python}/bin/pytest tests/ -v
              '';
            }}/bin/test-api";
          };
        };
      });
}
