{
  description = "Python-Prolog Bridge Pattern Template";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    grimoire.url = "github:your-org/grimoire";
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
      
      # Build internal Python package
      bridge-domain = pkgs.python3Packages.buildPythonPackage {
        pname = "bridge-domain";
        version = "0.1.0";
        src = ./python;
        format = "pyproject";
        propagatedBuildInputs = with pkgs.python3Packages; [
          janus-swi pydantic
        ];
      };

      # Python environment with bridge package
      pythonEnv = grimoireEnv.python.withPackages (p:
        grimoireEnv.python.pkgs ++ [bridge-domain]
      );
    in
    {
      # Main executable
      bridge-domain = grimoireEnv.mkGrimoireExecutable {
        name = "bridge-domain";
        script = ''
          cd ${./.}
          export GRIMOIRE_ROOT=''${GRIMOIRE_ROOT:-${./.}}
          export PYTHONPATH="${pythonEnv}/${pkgs.python3.sitePackages}:$PYTHONPATH"
          exec ${grimoireEnv.swipl}/bin/swipl \
            -g "ensure_loaded('semantics.pl')" \
            -t "halt" \
            -- "$@"
        '';
      };
    });

    devShells = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      grimoireEnv = grimoire.lib.mkGrimoireEnv pkgs;
    in
    {
      default = pkgs.mkShell (grimoireEnv.env // {
        buildInputs = grimoireEnv.buildInputs ++ [
          self.packages.${system}.bridge-domain
        ];
      });
    });

    checks = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      # Run Prolog tests
      bridge-tests = pkgs.runCommand "bridge-tests" {
        buildInputs = [ self.packages.${system}.bridge-domain ];
      } ''
        cd ${./.}
        ${self.packages.${system}.bridge-domain}/bin/bridge-domain test
        touch $out
      '';
    });
  };
}