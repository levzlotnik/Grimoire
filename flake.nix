{
  description = "Grimoire: A Knowledge-Based Operating System";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    grimoire-templates.url = "github:levzlotnik/grimoire-templates";
    golems = {
      url = "path:./src/golems";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.grimoire.follows = "";
    };
    grimoire-py = {
      url = "path:./src/interface/api";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.grimoire-flake.follows = "";
    };
  };

  outputs = { self, nixpkgs, flake-utils, golems, grimoire-py, grimoire-templates, ... }@inputs:
  let
    systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
    forAllSystems = nixpkgs.lib.genAttrs systems;
  in rec
  {
    # Expose grimoire environment for dependent flakes (top-level, not in packages)
    grimoireEnv = forAllSystems (system:
      let
        # Apply overlays to get pkgs with grimoire-golems and grimoire-py available
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            golems.overlays.default
            grimoire-py.overlays.default
          ];
        };
      in
      # Create grimoire environment with base packages + grimoire-golems and grimoire-py
      import ./deps/grimoire.nix {
        inherit pkgs;
        extraPythonPackages = ps: with ps; [
          grimoire-golems
          grimoire-py
        ];
      }
    );

    # Expose grimoire environment for dependent flakes
    lib = {
      getGrimoireEnv = system: self.grimoireEnv.${system};
    };

    # Packages
    packages = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      grimoireEnv = self.grimoireEnv.${system};
    in
    {
      # Grimoire templates wrapper scripts
      grimoire-templates-tools = pkgs.stdenv.mkDerivation {
        name = "grimoire-templates-tools";

        getGrimoireTemplates = pkgs.writeShellScript "getGrimoireTemplates" ''
          ${pkgs.nix}/bin/nix flake show ${grimoire-templates} --json
        '';

        initGrimoireTemplate = pkgs.writeShellScript "initGrimoireTemplate" ''
          TEMPLATE_ID="$1"
          PROJECT_DIR="$2"

          if [ -z "$TEMPLATE_ID" ] || [ -z "$PROJECT_DIR" ]; then
            echo "Usage: initGrimoireTemplate <TEMPLATE_ID> <PROJECT_DIR>" >&2
            exit 1
          fi

          ${pkgs.nix}/bin/nix flake new -t ${grimoire-templates}#"$TEMPLATE_ID" "$PROJECT_DIR"
        '';

        buildCommand = ''
          mkdir -p $out/bin
          cp $getGrimoireTemplates $out/bin/getGrimoireTemplates
          cp $initGrimoireTemplate $out/bin/initGrimoireTemplate
          chmod +x $out/bin/getGrimoireTemplates
          chmod +x $out/bin/initGrimoireTemplate
        '';
      };

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
      grimoireEnv = self.grimoireEnv.${system};

      # Apply overlays to get pkgs with dev packages available
      pkgsWithOverlays = import nixpkgs {
        inherit system;
        overlays = [
          grimoire-py.overlays.default
          golems.overlays.default
        ];
      };

      # Development environment with base packages + dev versions of grimoire packages
      devGrimoireEnv = import ./deps/grimoire.nix {
        pkgs = pkgsWithOverlays;
        extraPythonPackages = ps: [
          ps.grimoire-py-dev
          ps.grimoire-golems-dev
        ];
      };
    in
    {
      # Production shell - fully hardened with immutable grimoire packages (for Docker/deployment)
      production = pkgs.mkShell {
        buildInputs = [
          grimoireEnv.swipl
          grimoireEnv.python
          grimoireEnv.sqlite
          self.packages.${system}.grimoire
          self.packages.${system}.grimoire-templates-tools
        ];

        # Set GRIMOIRE_ROOT as a direct environment variable (works with -c)
        GRIMOIRE_ROOT = toString self.packages.${system}.grimoire;

        # Set templates tools path
        GRIMOIRE_TEMPLATES_TOOLS = "${self.packages.${system}.grimoire-templates-tools}/bin";

        # Set Python executable to production environment
        PYTHON_EXECUTABLE = "${grimoireEnv.python}/bin/python";

        # Inherit other environment variables from grimoireEnv
        inherit (grimoireEnv.env) SWIPL_BIN LLM_DB_SCHEMA_PATH;

        shellHook = ''
          # Load .env if it exists
          if [ -f .env ]; then
            echo "Loading environment variables from .env..."
            set -a  # automatically export all variables
            source .env
            set +a
          fi

          echo "✓ Grimoire production environment loaded"
          echo "  GRIMOIRE_ROOT=$GRIMOIRE_ROOT"
          echo "  Python with immutable grimoire packages"
        '';
      };

      # Development shell - editable grimoire packages (default)
      dev = pkgs.mkShell {
        buildInputs = [
          devGrimoireEnv.swipl
          devGrimoireEnv.python
          devGrimoireEnv.sqlite
          self.packages.${system}.grimoire
          self.packages.${system}.grimoire-templates-tools
        ];

        # Set GRIMOIRE_ROOT as a direct environment variable (works with -c)
        GRIMOIRE_ROOT = toString self.packages.${system}.grimoire;

        # Set templates tools path
        GRIMOIRE_TEMPLATES_TOOLS = "${self.packages.${system}.grimoire-templates-tools}/bin";

        # Set Python executable to dev environment
        PYTHON_EXECUTABLE = "${devGrimoireEnv.python}/bin/python";

        # Inherit other environment variables from devGrimoireEnv
        inherit (devGrimoireEnv.env) SWIPL_BIN LLM_DB_SCHEMA_PATH;

        shellHook = ''
          # Load .env if it exists
          if [ -f .env ]; then
            echo "Loading environment variables from .env..."
            set -a  # automatically export all variables
            source .env
            set +a
          fi

          # Set editable package roots to actual working directory (mutable!)
          export GRIMOIRE_PY_ROOT="$PWD/src/interface/api"
          export GRIMOIRE_GOLEMS_ROOT="$PWD/src/golems/python"

          echo "✓ Grimoire development environment loaded"
          echo "  GRIMOIRE_ROOT=$GRIMOIRE_ROOT"
          echo "  GRIMOIRE_PY_ROOT=$GRIMOIRE_PY_ROOT"
          echo "  GRIMOIRE_GOLEMS_ROOT=$GRIMOIRE_GOLEMS_ROOT"
          echo "  Python packages: grimoire-py and grimoire-golems are editable"
          echo "  Changes to Python source will be reflected immediately"
        '';
      };

      # Default to dev shell
      default = self.devShells.${system}.dev;
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
      grimoireEnv = self.grimoireEnv.${system};
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
