{
  description = "Grimoire Python Package";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    grimoire-flake.url = "path:../../..";
  };

  outputs = { self, nixpkgs, flake-utils, grimoire-flake }:
    let
      # Provide overlay for parent flake to consume (production package only)
      overlay = final: prev: {
        pythonPackagesExtensions = prev.pythonPackagesExtensions ++ [
          (python-final: python-prev:
            let
              packages = python-final.callPackage ./package.nix {
                grimoire-flake = grimoire-flake;
                system = final.system;
                fastmcp = python-final.fastmcp;
                janus-swi = (grimoire-flake.lib.getGrimoireEnv final.system).janus-swi;
              };
            in
            {
              # Production package in overlay
              grimoire-py = packages.grimoirePy;
              # Dev package also available
              grimoire-py-dev = packages.grimoirePyDev;
            }
          )
        ];
      };
    in
    {
      # Expose overlay for consumption by other flakes
      overlays.default = overlay;
    } // flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Get grimoireEnv from parent flake
        grimoireEnv = grimoire-flake.lib.getGrimoireEnv system;

        # Use the Python from grimoireEnv
        python = grimoireEnv.python;

        # Import both packages from package.nix
        packages = python.pkgs.callPackage ./package.nix {
          grimoire-flake = grimoire-flake;
          system = system;
          fastmcp = python.pkgs.fastmcp;
        };

      in
      {
        packages = {
          default = packages.grimoirePy;
          grimoire-py = packages.grimoirePy;
          grimoire-py-dev = packages.grimoirePyDev;
        };

        devShells = {
          # Production shell - immutable package
          production = pkgs.mkShell {
            buildInputs = [
              (python.withPackages (ps: [ packages.grimoirePy ]))
            ];

            shellHook = ''
              echo "✓ Grimoire API production environment"
              echo "  Using immutable grimoire-py package"
            '';
          };

          # Development shell - editable package
          dev = pkgs.mkShell {
            buildInputs = [
              (python.withPackages (ps: [ packages.grimoirePyDev ]))
            ];

            # Set environment variable for editable root
            GRIMOIRE_PY_ROOT = toString ./.;

            shellHook = ''
              echo "✓ Grimoire API development environment"
              echo "  GRIMOIRE_PY_ROOT=$GRIMOIRE_PY_ROOT"
              echo "  Using editable grimoire-py package"
              echo "  Changes to source will be reflected immediately"
            '';
          };

          # Default to dev
          default = self.devShells.${system}.dev;
        };

        checks = {
          tests = pkgs.runCommand "grimoire-py-tests" (grimoireEnv.env // {
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
