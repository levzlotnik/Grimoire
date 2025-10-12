{
  description = "Grimoire API Server Python Package";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    grimoire-flake.url = "path:../../..";
  };

  outputs = { self, nixpkgs, flake-utils, grimoire-flake }:
    let
      # Provide overlay for parent flake to consume
      overlay = final: prev: {
        pythonPackagesExtensions = prev.pythonPackagesExtensions ++ [
          (python-final: python-prev: {
            grimoire-api = python-final.callPackage ({
              buildPythonPackage,
              setuptools,
              wheel,
              fastapi,
              uvicorn,
              pydantic,
              pyyaml,
              mcp,
              janus-swi
            }: buildPythonPackage {
              pname = "grimoire-api";
              version = "0.1.0";
              src = ./.;
              format = "pyproject";

              nativeBuildInputs = [ setuptools wheel ];

              propagatedBuildInputs = [
                fastapi
                uvicorn
                pydantic
                pyyaml
                mcp
                janus-swi
              ];

              # Set build environment
              SWIPL_BIN = "${(grimoire-flake.lib.getGrimoireEnv final.system).swipl}/bin/swipl";
              LLM_DB_SCHEMA_PATH = (grimoire-flake.lib.getGrimoireEnv final.system).env.LLM_DB_SCHEMA_PATH;
            }) {
              janus-swi = (grimoire-flake.lib.getGrimoireEnv final.system).janus-swi;
            };
          })
        ];
      };
    in
    {
      # Expose overlay for consumption by other flakes
      overlays.default = overlay;
    } // flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Get grimoireEnv from parent flake - this has the extended Python environment
        grimoireEnv = grimoire-flake.lib.getGrimoireEnv system;
        
        # Use the Python from grimoireEnv which already has all packages including MCP
        python = grimoireEnv.python;

        # Build the Python package using the grimoire Python environment
        grimoireApiPackage = python.pkgs.buildPythonPackage {
          pname = "grimoire-api";
          version = "0.1.0";
          src = ./.;
          format = "pyproject";

          nativeBuildInputs = with python.pkgs; [
            setuptools
            wheel
          ];

          propagatedBuildInputs = with python.pkgs; [
            fastapi
            uvicorn
            pydantic
            pyyaml
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
            ${python}/bin/pytest tests/ -v
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
                exec ${python}/bin/pytest tests/ -v
              '';
            }}/bin/test-api";
          };
        };
      });
}
