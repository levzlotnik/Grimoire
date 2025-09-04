{
  description = "Python-Prolog Bridge Pattern Template";

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
      grimoireEnv = grimoire.lib.mkGrimoireEnv pkgs;

      # Build our Python package with janus-swi from Grimoire
      bridge-domain = pkgs.python3Packages.buildPythonPackage {
        pname = "bridge-domain";
        version = "0.1.0";
        src = ./python;
        format = "pyproject";
        propagatedBuildInputs = with pkgs.python3Packages; [
          pydantic
          setuptools
          grimoireEnv.janus-swi  # janus-swi from Grimoire
        ];
      };


    in
    {
      bridge-domain = bridge-domain;
      default = bridge-domain;
    });

    devShells = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      grimoireEnv = grimoire.lib.mkGrimoireEnv pkgs;
      selfPackages = self.packages.${system};

      pythonEnv = grimoireEnv.python.withPackages (p:
        [ selfPackages.bridge-domain ]
      );
    in
    {
      default = pkgs.mkShell (grimoireEnv.env // {
        buildInputs = grimoireEnv.buildInputs ++ [ pythonEnv grimoire.packages.${system}.grimoire ];

        shellHook = ''
          echo "Python-Prolog Bridge Pattern Development Environment"
          echo "SWI-Prolog: ${grimoireEnv.swipl}/bin/swipl"
          echo "Python with janus-swi: ${pythonEnv}/bin/python"
          echo ""
          echo "Run 'grimoire exec semantics.pl' to test the bridge"
          echo "Run 'grimoire test -- semantics.plt' to run tests"
        '';
      });
    });

    checks = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      pythonEnv = grimoire.lib.mkGrimoireEnv(pkgs).python.withPackages (p:
        grimoire.lib.mkGrimoireEnv(pkgs).python.pkgs ++ [self.packages.${system}.bridge-domain]
      );
    in
    {
      # Run Prolog tests using Grimoire's main executable
      bridge-tests = pkgs.runCommand "bridge-tests" {
        buildInputs = [ grimoire.packages.${system}.grimoire ];
      } ''
        cd ${./.}
        export PYTHONPATH="${pythonEnv}/${pkgs.python3.sitePackages}:$PYTHONPATH"
        ${grimoire.packages.${system}.grimoire}/bin/grimoire exec -g "run_tests" -t "halt" semantics.plt
        touch $out
      '';
    });
  };
}