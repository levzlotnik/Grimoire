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
    # Expose grimoire environment for dependent flakes
    lib = {
      getGrimoireEnv = system: self.packages.${system}.grimoireEnv;
    };

    # Packages
    packages = forAllSystems (system:
    let
      # First get base pkgs without overlay
      basePkgs = nixpkgs.legacyPackages.${system};

      # Then apply overlays to get pkgs with grimoire-golems and grimoire-py available
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          golems.overlays.default
          grimoire-py.overlays.default
        ];
      };

      # Create base grimoire environment with overlay-enabled pkgs
      baseGrimoireEnv = import ./deps/grimoire.nix { inherit pkgs; };

      # Extend the Python environment to include grimoire-golems and grimoire-py while preserving all base packages
      # We need to extract the packages from the base environment and add our new ones
      extendedPython = pkgs.python313.withPackages (ps: with ps; [
        # Core Python packages (from deps/grimoire.nix)
        requests
        python-dotenv
        gitpython
        baseGrimoireEnv.janus-swi
        baseGrimoireEnv.pydantic-ai
        # API/Server packages
        fastapi
        uvicorn
        pydantic
        fastmcp  # FastMCP - Pythonic MCP client and server
        # Testing packages
        httpx
        pytest
        pytest-asyncio
        # Development packages
        black
        # Web framework
        flask
        # YAML support (needed by MCP server)
        pyyaml
        # Scientific computing
        numpy
        scipy
        # ML/Generative models
        huggingface-hub
        # Additional grimoire packages
        grimoire-golems
        grimoire-py
      ]);

      # Create extended grimoire environment
      grimoireEnv = baseGrimoireEnv // {
        python = extendedPython;
        buildInputs = baseGrimoireEnv.buildInputs ++ [ extendedPython ];
      };
    in
    {
      # Export the grimoireEnv for use by devShells and dependent flakes
      inherit grimoireEnv;

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
      grimoireEnv = self.packages.${system}.grimoireEnv;
    in
    {
      default = pkgs.mkShell {
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

        # Set Python executable to our extended environment
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

          echo "Grimoire development environment loaded"
          echo "GRIMOIRE_ROOT=$GRIMOIRE_ROOT"
          echo "Python with grimoire-golems: ${grimoireEnv.python}/bin/python"
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
