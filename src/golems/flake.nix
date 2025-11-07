{
  description = "Grimoire Golems AI Agent Framework";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    grimoire.url = "path:../../../..";
  };

  outputs = { self, nixpkgs, grimoire, ... }:
  let
    systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
    forAllSystems = nixpkgs.lib.genAttrs systems;
  in
  {
    # Provide overlay for parent flake to consume (production packages)
    overlays.default = final: prev: {
      pythonPackagesExtensions = prev.pythonPackagesExtensions ++ [
        (python-final: python-prev:
          let
            packages = python-final.callPackage ./package.nix {
              janus-swi = (grimoire.lib.getGrimoireEnv final.system).janus-swi;
              pydantic-ai = (grimoire.lib.getGrimoireEnv final.system).pydantic-ai;
              grimoire-py = python-final.grimoire-py;
              grimoire-py-dev = python-final.grimoire-py-dev;  # Use dev version for dev package
            };
          in
          {
            # Production package in overlay
            grimoire-golems = packages.grimoireGolems;
            # Dev package also available
            grimoire-golems-dev = packages.grimoireGolemsDev;
          }
        )
      ];
    };

    # For backwards compatibility, still provide packages
    packages = forAllSystems (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          grimoire.inputs.grimoire-py.overlays.default
          self.overlays.default
        ];
      };
    in
    {
      default = pkgs.python313Packages.grimoire-golems;
      grimoire-golems = pkgs.python313Packages.grimoire-golems;
      grimoire-golems-dev = pkgs.python313Packages.grimoire-golems-dev;
    });

    devShells = forAllSystems (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          grimoire.inputs.grimoire-py.overlays.default
          self.overlays.default
        ];
      };
    in
    {
      # Production shell - immutable packages
      production = pkgs.mkShell {
        buildInputs = [
          (pkgs.python313.withPackages (ps: [
            ps.grimoire-py
            ps.grimoire-golems
          ]))
        ];

        shellHook = ''
          echo "✓ Grimoire Golems production environment"
          echo "  Using immutable grimoire-golems package"
        '';
      };

      # Development shell - editable packages
      dev = pkgs.mkShell {
        buildInputs = [
          (pkgs.python313.withPackages (ps: [
            ps.grimoire-py-dev
            ps.grimoire-golems-dev
          ]))
        ];

        # Set environment variables for editable roots
        GRIMOIRE_PY_ROOT = toString ../../interface/api;
        GRIMOIRE_GOLEMS_ROOT = toString ./python;

        shellHook = ''
          echo "✓ Grimoire Golems development environment"
          echo "  GRIMOIRE_PY_ROOT=$GRIMOIRE_PY_ROOT"
          echo "  GRIMOIRE_GOLEMS_ROOT=$GRIMOIRE_GOLEMS_ROOT"
          echo "  Using editable packages for both grimoire-py and grimoire-golems"
          echo "  Changes to source will be reflected immediately"
        '';
      };

      # Default to dev
      default = self.devShells.${system}.dev;
    });

    checks = forAllSystems (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          grimoire.inputs.grimoire-py.overlays.default
          self.overlays.default
        ];
      };
    in
    {
      # Python package check - just verify it builds
      python-package = pkgs.runCommand "python-package-check" {
        buildInputs = [
          (pkgs.python313.withPackages (ps: [ ps.grimoire-golems ]))
        ];
      } ''
        python -c "import grimoire_golems; print('grimoire-golems package imported successfully')"
        touch $out
      '';
    });
  };
}