{
  description = "Grimoire: A Knowledge-Based Operating System";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs, ... }@inputs:
  let
    systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
    forAllSystems = nixpkgs.lib.genAttrs systems;
  in rec
  {
    # Expose grimoire environment builder for dependent flakes
    lib = {
      mkGrimoireEnv = pkgs: import ./deps/grimoire.nix { inherit pkgs; };
    };

    # Packages
    packages = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      grimoireEnv = self.lib.mkGrimoireEnv pkgs;
    in
    {
      # Main Grimoire executable - include all source files
      grimoire = pkgs.stdenv.mkDerivation {
        name = "grimoire";
        src = ./.;
        buildInputs = [ grimoireEnv.swipl ];

        installPhase = ''
          # Copy everything to output - maintaining structure
          cp -r . $out

          # Substitute swipl path in the grimoire script (keep GRIMOIRE_ROOT dynamic)
          substituteInPlace $out/grimoire \
            --replace "exec swipl" "exec ${grimoireEnv.swipl}/bin/swipl"

          # Ensure the grimoire script is executable
          chmod +x $out/grimoire

          # Create bin directory and symlink
          mkdir -p $out/bin
          ln -s $out/grimoire $out/bin/grimoire

          # Create setup hook to export GRIMOIRE_ROOT for any environment that includes this package
          mkdir -p $out/nix-support
          cat >> $out/nix-support/setup-hook << EOF
export GRIMOIRE_ROOT="\''${GRIMOIRE_ROOT:-$out}"
EOF
        '';
      };

    });

    # Development shells
    devShells = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      grimoireEnv = import ./deps/grimoire.nix { inherit pkgs; };
    in
    {
      default = pkgs.mkShell {
        buildInputs = grimoireEnv.buildInputs ++ [ self.packages.${system}.grimoire ];
        
        # Set GRIMOIRE_ROOT as a direct environment variable (works with -c)
        GRIMOIRE_ROOT = toString self.packages.${system}.grimoire;
        
        # Inherit other environment variables from grimoireEnv
        inherit (grimoireEnv.env) SWIPL_BIN LLM_DB_SCHEMA_PATH;
        
        shellHook = ''
          echo "Grimoire development environment loaded"
          echo "GRIMOIRE_ROOT=$GRIMOIRE_ROOT"
        '';
      };
    });

    # Apps for running Grimoire
    apps = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      # Launch interactive Grimoire shell
      grimoire = {
        type = "app";
        program = "${self.packages.${system}.grimoire}/bin/grimoire";
      };

      # Run all system tests
      test = {
        type = "app";
        program = "${self.packages.${system}.grimoire}/bin/grimoire test";
      };
    });

    # Checks for nix flake check
    checks = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      grimoireEnv = import ./deps/grimoire.nix { inherit pkgs; };
    in
    {
      # Run Grimoire system tests
      grimoire-tests = pkgs.runCommand "grimoire-tests" {
        buildInputs = grimoireEnv.buildInputs;
      } ''
        cd ${./.}
        export SWIPL_BIN="${grimoireEnv.swipl}/bin/swipl"
        export LLM_DB_SCHEMA_PATH="${grimoireEnv.env.LLM_DB_SCHEMA_PATH}"
        ${self.packages.${system}.grimoire}/bin/grimoire test
        touch $out
      '';

      # Run API tests - delegate to API flake
      api-tests = pkgs.runCommand "api-tests" {
        buildInputs = grimoireEnv.buildInputs;
      } ''
        cd ${./.}/src/interface/api
        export SWIPL_BIN="${grimoireEnv.swipl}/bin/swipl"
        export LLM_DB_SCHEMA_PATH="${grimoireEnv.env.LLM_DB_SCHEMA_PATH}"
        # Note: This requires API flake to be built separately as it has its own dependencies
        echo "API tests should be run via: nix flake check ./src/interface/api/"
        touch $out
      '';
    });
  };
}
