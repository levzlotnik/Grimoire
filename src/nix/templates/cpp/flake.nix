{
  description = "C++ development environment";

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
            gcc
            cmake
            gdb
            valgrind
            git
          ];

          shellHook = ''
            echo "⚡ C++ Development Environment (CMake)"
            echo "GCC: $(gcc --version | head -n1)"
            echo "CMake: $(cmake --version | head -n1)"
            echo ""
            echo "Build: mkdir -p build && cd build && cmake .. && cmake --build ."
            echo "Run: ./build/cpp-template"
          '';
        };

        packages.default = pkgs.stdenv.mkDerivation {
          pname = "cpp-template";
          version = "0.1.0";
          src = ./.;

          nativeBuildInputs = with pkgs; [ cmake ];
          buildInputs = with pkgs; [ gcc ];

          configurePhase = ''
            mkdir -p build
            cd build
            cmake ..
          '';

          buildPhase = ''
            cmake --build .
          '';

          installPhase = ''
            mkdir -p $out/bin
            cp cpp-template $out/bin/
          '';
        };

        apps = {
          default = {
            type = "app";
            program = "${self.packages.${system}.default}/bin/cpp-template";
          };
          run = {
            type = "app";
            program = "${pkgs.bash}/bin/bash";
            args = [ "-c" "mkdir -p build && cd build && cmake .. && cmake --build . && ./cpp-template" ];
          };
        };
      });
}
