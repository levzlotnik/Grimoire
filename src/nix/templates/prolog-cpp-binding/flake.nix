{
  description = "Prolog-C++ Binding Template with PlCxx Integration";

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

      # C++ binding library
      prolog-cpp-binding = pkgs.stdenv.mkDerivation {
        pname = "prolog-cpp-binding";
        version = "1.0.0";
        
        src = ./.;
        
        nativeBuildInputs = with pkgs; [
          cmake
          pkg-config
          gcc13  # Modern C++ compiler with C++20 support
        ];
        
        buildInputs = [
          grimoireEnv.swipl
          pkgs.gtest  # For potential C++ unit tests
        ];

        # Configure CMake with proper flags
        cmakeFlags = [
          "-DCMAKE_BUILD_TYPE=Release"
          "-DCMAKE_CXX_STANDARD=20"
          "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
        ];

        # Ensure SWI-Prolog can find our library
        postInstall = ''
          # Create a wrapper script that sets up library paths
          mkdir -p $out/bin
          cat > $out/bin/swipl-with-cpp <<EOF
#!/bin/bash
export LD_LIBRARY_PATH="$out/lib:$LD_LIBRARY_PATH"
export SWI_HOME_DIR="${grimoireEnv.swipl}/lib/swipl"
exec "${grimoireEnv.swipl}/bin/swipl" "\$@"
EOF
          chmod +x $out/bin/swipl-with-cpp

          # Copy Prolog files to output
          cp *.pl *.plt $out/lib/ 2>/dev/null || true
        '';

        meta = with pkgs.lib; {
          description = "Advanced Prolog-C++ binding template using PlCxx";
          license = licenses.mit;
          maintainers = [ ];
          platforms = platforms.unix;
        };
      };

      # Development environment with all tools
      devEnv = pkgs.mkShell (grimoireEnv.env // {
        buildInputs = with pkgs; [
          # Core development tools
          grimoireEnv.swipl
          cmake
          pkg-config
          gcc13
          gdb
          valgrind
          
          # C++ development
          clang-tools  # clang-format, clangd
          cppcheck
          doxygen
          
          # Build system
          ninja
          ccache
          
          # Testing
          gtest
          catch2_3
          
          # Documentation
          graphviz
          plantuml
        ];

        # Environment setup
        CMAKE_EXPORT_COMPILE_COMMANDS = "ON";
        CMAKE_CXX_STANDARD = "20";
        
        # SWI-Prolog configuration
        SWI_HOME_DIR = "${grimoireEnv.swipl}/lib/swipl";
        
        shellHook = ''
          echo "Prolog-C++ Binding Development Environment"
          echo "=========================================="
          echo "SWI-Prolog: ${grimoireEnv.swipl}/bin/swipl"
          echo "C++ Compiler: ${pkgs.gcc13}/bin/g++"
          echo "CMake: ${pkgs.cmake}/bin/cmake"
          echo ""
          echo "Available commands:"
          echo "  mkdir build && cd build"
          echo "  cmake .."
          echo "  make -j$(nproc)"
          echo "  make prolog_tests"
          echo ""
          echo "For Grimoire integration:"
          echo "  grimoire exec semantics.pl"
          echo "  grimoire test -- semantics.plt"
          echo ""
          echo "C++ standard: C++20"
          echo "Build type: Debug (with sanitizers)"
        '';
      });

    in
    {
      grimoireEnv = grimoireEnv;
      prolog-cpp-binding = prolog-cpp-binding;
      devEnv = devEnv;
      default = prolog-cpp-binding;
    });

    devShells = forAllSystems (system: {
      default = self.packages.${system}.devEnv;
    });

    apps = forAllSystems (system: {
      # App to build the C++ library
      build = {
        type = "app";
        program = "${pkgs.writeScript "build-cpp-binding" ''
          #!/bin/bash
          set -e
          mkdir -p build
          cd build
          ${pkgs.cmake}/bin/cmake .. -DCMAKE_BUILD_TYPE=Debug
          ${pkgs.gnumake}/bin/make -j$(nproc)
          echo "Build complete. Library: build/libprolog_cpp_binding.so"
        ''}";
      };

      # App to run tests
      test = {
        type = "app";
        program = "${pkgs.writeScript "test-cpp-binding" ''
          #!/bin/bash
          set -e
          if [ ! -f build/libprolog_cpp_binding.so ]; then
            echo "Building library first..."
            nix run .#build
          fi
          cd build
          export LD_LIBRARY_PATH=".:$LD_LIBRARY_PATH"
          echo "Running Prolog tests..."
          ${self.packages.${system}.grimoireEnv.swipl}/bin/swipl -g "run_tests" -t "halt" semantics.plt
        ''}";
      };

      # App to run with Grimoire
      grimoire = {
        type = "app";
        program = "${pkgs.writeScript "grimoire-cpp-binding" ''
          #!/bin/bash
          set -e
          if [ ! -f build/libprolog_cpp_binding.so ]; then
            echo "Building library first..."
            nix run .#build
          fi
          export LD_LIBRARY_PATH="build:$LD_LIBRARY_PATH"
          ${grimoire.packages.${system}.grimoire}/bin/grimoire exec semantics.pl
        ''}";
      };
    });

    checks = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      # Build check
      build = self.packages.${system}.prolog-cpp-binding;
      
      # Prolog tests check
      prolog-tests = pkgs.runCommand "prolog-cpp-binding-tests" {
        buildInputs = [
          self.packages.${system}.prolog-cpp-binding
          self.packages.${system}.grimoireEnv.swipl
        ];
      } ''
        cd ${./.}
        export LD_LIBRARY_PATH="${self.packages.${system}.prolog-cpp-binding}/lib:$LD_LIBRARY_PATH"
        export SWI_HOME_DIR="${self.packages.${system}.grimoireEnv.swipl}/lib/swipl"
        ${self.packages.${system}.grimoireEnv.swipl}/bin/swipl -g "run_tests" -t "halt" semantics.plt
        touch $out
      '';
    });
  };
}