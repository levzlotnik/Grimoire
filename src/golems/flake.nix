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
    # Provide overlay for parent flake to consume
    overlays.default = final: prev: {
      pythonPackagesExtensions = prev.pythonPackagesExtensions ++ [
        (python-final: python-prev: {
          grimoire-golems = python-final.callPackage ./grimoire-golems.nix {
            janus-swi = (grimoire.lib.getGrimoireEnv final.system).janus-swi;
          };
        })
      ];
    };

    # For backwards compatibility, still provide packages  
    packages = forAllSystems (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ self.overlays.default ];
      };
    in
    {
      grimoire-golems = pkgs.python313Packages.grimoire-golems;
      default = pkgs.python313Packages.grimoire-golems;
    });

    devShells = forAllSystems (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ self.overlays.default ];
      };
    in
    {
      default = pkgs.mkShell {
        buildInputs = [ 
          (pkgs.python313.withPackages (ps: [ ps.grimoire-golems ]))
        ];

        shellHook = ''
          echo "Grimoire Golems AI Agent Framework Development Environment"
          echo "Use main Grimoire flake for full development environment"
        '';
      };
    });

    checks = forAllSystems (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ self.overlays.default ];
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