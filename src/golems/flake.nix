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
    packages = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      grimoireEnv = grimoire.lib.getGrimoireEnv system;

      # Build our Python package - include janus-swi from grimoireEnv as dependency
      grimoire-golems = pkgs.callPackage ./grimoire-golems.nix {
        python3Packages = pkgs.python313Packages;
        janus-swi = grimoireEnv.janus-swi;
      };

    in
    {
      grimoire-golems = grimoire-golems;
      default = grimoire-golems;
    });

    devShells = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      grimoireEnv = grimoire.lib.getGrimoireEnv system;
    in
    {
      default = pkgs.mkShell {
        buildInputs = [ 
          grimoireEnv.python
          grimoireEnv.swipl
          self.packages.${system}.grimoire-golems
        ];
        
        inherit (grimoireEnv.env) SWIPL_BIN LLM_DB_SCHEMA_PATH;

        shellHook = ''
          echo "Grimoire Golems AI Agent Framework Development Environment"
          echo "Use main Grimoire flake for full development environment"
        '';
      };
    });

    checks = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      # Run Prolog tests using Grimoire's main executable
      bridge-tests = pkgs.runCommand "bridge-tests" {
        buildInputs = [
          grimoire.packages.${system}.grimoire
          self.packages.${system}.pythonEnv
        ];
      } ''
        cd ${./.}
        ${grimoire.packages.${system}.grimoire}/bin/grimoire exec -g "run_tests" -t "halt" semantics.plt
        touch $out
      '';
    });
  };
}