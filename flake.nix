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
      # Main Grimoire executable
      grimoire = grimoireEnv.mkGrimoireExecutable {
        name = "grimoire";
        script = ''
          cd ${./.}
          exec ${grimoireEnv.swipl}/bin/swipl \
            -g "ensure_loaded('src/grimoire.pl')" \
            -t "main" \
            "src/interface/cli.pl" \
            -- "$@"
        '';
      };

      # Grimoire REST API server executable
      grimoire-server = grimoireEnv.mkGrimoireExecutable {
        name = "grimoire-server";
        script = ''
          cd ${./.}
          export PYTHONPATH="${./.}/src/interface/api:$PYTHONPATH"
          exec ${grimoireEnv.python}/bin/python ${./.}/src/interface/api/rest_api.py "$@"
        '';
      };

      # Grimoire MCP server executable
      grimoire-mcp-server = grimoireEnv.mkGrimoireExecutable {
        name = "grimoire-mcp-server";
        script = ''
          cd ${./.}
          export PYTHONPATH="${./.}/src/interface/api:$PYTHONPATH"
          exec ${grimoireEnv.python}/bin/python ${./.}/src/interface/api/mcp_server.py "$@"
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
      default = pkgs.mkShell (grimoireEnv.env // {
        buildInputs = grimoireEnv.buildInputs;
        packages = with self.packages.${system}; [ grimoire grimoire-server grimoire-mcp-server ];
      });
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

      # REST API server
      grimoire-server = {
        type = "app";
        program = "${self.packages.${system}.grimoire-server}/bin/grimoire-server";
      };

      # MCP server
      grimoire-mcp-server = {
        type = "app";
        program = "${self.packages.${system}.grimoire-mcp-server}/bin/grimoire-mcp-server";
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
