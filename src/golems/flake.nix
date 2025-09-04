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
    lib = {
      getGrimoireEnv = system: self.packages.${system}.grimoireEnv;
    };

    packages = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      grimoireEnv = grimoire.lib.getGrimoireEnv system;

      # Build our Python package with janus-swi from Grimoire
      grimoire-golems = pkgs.callPackage ./grimoire-golems.nix {
        python3Packages = pkgs.python313Packages;
      };

      # Create unified Python environment
      pythonEnv = grimoireEnv.python.withPackages (ps: 
        grimoireEnv.pythonPackages ++ [
          grimoireEnv.janus-swi
          grimoire-golems
        ]
      );

    in
    {
      grimoireEnv = grimoireEnv;
      grimoire-golems = grimoire-golems;
      pythonEnv = pythonEnv;
      default = grimoire-golems;
    });

    devShells = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      grimoireEnv = self.lib.getGrimoireEnv system;
    in
    {
      default = pkgs.mkShell (grimoireEnv.env // {
        buildInputs = [
          self.packages.${system}.pythonEnv
          grimoireEnv.swipl
        ];

        # Ensure janus-swi uses the same Python environment
        PYTHONPATH = "${self.packages.${system}.pythonEnv}/${self.packages.${system}.pythonEnv.sitePackages}";
        PYTHON_EXECUTABLE = "${self.packages.${system}.pythonEnv}/bin/python";
        PATH = "${self.packages.${system}.pythonEnv}/bin:$PATH";


        shellHook = ''
          echo "Grimoire Golems AI Agent Framework Development Environment"
          echo "SWI-Prolog: ${grimoireEnv.swipl}/bin/swipl"
          echo "Python with janus-swi: ${self.packages.${system}.pythonEnv}/bin/python"
          echo ""
          echo "Run 'grimoire exec semantics.pl' to test the golems bridge"
          echo "Run 'grimoire test golems' to run tests"
        '';
      });
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