{
  description = "Rust development environment with Cargo";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Rust toolchain
            rustc
            cargo
            rustfmt
            clippy
            rust-analyzer

            # Development tools
            pkg-config
            openssl
            git
            curl

            # For cross-compilation (optional)
            gcc
          ];

          shellHook = ''
            echo "🦀 Rust Development Environment"
            echo "Rust: $(rustc --version)"
            echo "Cargo: $(cargo --version)"
            echo ""
            echo "Available commands:"
            echo "  cargo run           - Run the project"
            echo "  cargo test          - Run tests"
            echo "  cargo build         - Build project"
            echo "  cargo fmt           - Format code"
            echo "  cargo clippy        - Lint code"
            echo "  cargo doc --open    - Generate and open docs"
            echo ""

            # Initialize Cargo project if Cargo.toml doesn't exist
            if [[ ! -f Cargo.toml ]]; then
              echo "Initializing Cargo project..."
              cargo init --name "$(basename $(pwd))"
            fi
          '';
        };

        packages.default = pkgs.rustPlatform.buildRustPackage {
          pname = "template-rust";
          version = "0.1.0";
          src = ./.;
          cargoLock.lockFile = ./Cargo.lock;
        };

        apps = {
          default = {
            type = "app";
            program = "${self.packages.${system}.default}/bin/template-rust";
          };
          run = {
            type = "app";
            program = "${pkgs.cargo}/bin/cargo";
            args = [ "run" ];
          };
          build = {
            type = "app";
            program = "${pkgs.cargo}/bin/cargo";
            args = [ "build" ];
          };
          test = {
            type = "app";
            program = "${pkgs.cargo}/bin/cargo";
            args = [ "test" ];
          };
          check = {
            type = "app";
            program = "${pkgs.cargo}/bin/cargo";
            args = [ "check" ];
          };
          clippy = {
            type = "app";
            program = "${pkgs.cargo}/bin/cargo";
            args = [ "clippy" ];
          };
          fmt = {
            type = "app";
            program = "${pkgs.cargo}/bin/cargo";
            args = [ "fmt" ];
          };
        };
      });
}
