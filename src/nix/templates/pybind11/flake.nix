{
  description = "PyBind11 Comprehensive Template for Grimoire";

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

      # Build our PyBind11 package
      pybind11-demo = pkgs.callPackage ./pybind11-demo.nix {
        python3Packages = grimoireEnv.python.pkgs;
      };

      # Create Python environment with our package
      pythonEnv = grimoireEnv.python.withPackages (ps: with ps; [
        # Core dependencies
        pybind11
        numpy
        pytest
        pytest-cov
        scipy
        matplotlib
        # Our package
        pybind11-demo
      ]);

    in
    {
      grimoireEnv = grimoireEnv;
      pybind11-demo = pybind11-demo;
      pythonEnv = pythonEnv;
      default = pybind11-demo;
    });

    devShells = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      grimoireEnv = self.lib.getGrimoireEnv system;
    in
    {
      default = pkgs.mkShell (grimoireEnv.env // {
        buildInputs = with pkgs; [
          # Development tools
          cmake
          ninja
          gdb
          valgrind
          clang-tools
          
          # Python environment with our package
          self.packages.${system}.pythonEnv
          grimoireEnv.swipl
          
          # Build dependencies
          eigen
          openblas
          pkg-config
        ];
        
        # Environment setup
        PYTHONPATH = "${self.packages.${system}.pythonEnv}/${self.packages.${system}.pythonEnv.sitePackages}";
        PYTHON_EXECUTABLE = "${self.packages.${system}.pythonEnv}/bin/python";
        PATH = "${self.packages.${system}.pythonEnv}/bin:$PATH";
        
        # CMake configuration
        CMAKE_PREFIX_PATH = "${pkgs.eigen}/include/eigen3:${pkgs.openblas}";
        
        shellHook = ''
          echo "PyBind11 Comprehensive Template Development Environment"
          echo "Python with PyBind11: ${self.packages.${system}.pythonEnv}/bin/python"
          echo "SWI-Prolog: ${grimoireEnv.swipl}/bin/swipl"
          echo ""
          echo "Available commands:"
          echo "  nix build                    - Build the PyBind11 package"
          echo "  nix run .#test-cpp          - Run C++ tests"
          echo "  nix run .#test-python       - Run Python tests"
          echo "  nix run .#demo              - Run interactive demo"
          echo "  grimoire test               - Run Prolog tests"
          echo ""
          echo "Development workflow:"
          echo "  1. Edit C++ code in src/ and include/"
          echo "  2. Build with: nix build"
          echo "  3. Test with: nix run .#test-python"
          echo "  4. Run Grimoire tests: grimoire test"
        '';
      });
    });

    apps = forAllSystems (system: {
      test-cpp = {
        type = "app";
        program = "${pkgs.writeShellScript "test-cpp" ''
          cd ${./.}
          if [ -d build ]; then rm -rf build; fi
          mkdir build && cd build
          cmake .. -GNinja -DCMAKE_BUILD_TYPE=Debug
          ninja
          echo "C++ tests would run here (placeholder for future native tests)"
        ''}";
      };
      
      test-python = {
        type = "app";
        program = "${pkgs.writeShellScript "test-python" ''
          cd ${./.}
          export PYTHONPATH="${self.packages.${system}.pythonEnv}/${self.packages.${system}.pythonEnv.sitePackages}"
          ${self.packages.${system}.pythonEnv}/bin/python -m pytest tests/ -v --cov=pybind_demo --cov-report=term-missing
        ''}";
      };
      
      demo = {
        type = "app";
        program = "${pkgs.writeShellScript "demo" ''
          cd ${./.}
          export PYTHONPATH="${self.packages.${system}.pythonEnv}/${self.packages.${system}.pythonEnv.sitePackages}"
          ${self.packages.${system}.pythonEnv}/bin/python -c "
import pybind_demo
print('PyBind11 Demo Module Loaded Successfully!')
print('Available submodules:', dir(pybind_demo))

# Quick demo
calc = pybind_demo.Calculator(42.0, 'demo')
print(f'Calculator: {calc}')

# Function demo
result = pybind_demo.functions.add(10, 20)
print(f'Functions.add(10, 20) = {result}')

# NumPy demo
import numpy as np
arr = np.array([1, 2, 3, 4, 5], dtype=float)
squared = pybind_demo.numpy_demo.square_array(arr)
print(f'Squared array: {squared}')
          "
        ''}";
      };
    });

    checks = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      # Run Python tests
      python-tests = pkgs.runCommand "python-tests" {
        buildInputs = [
          self.packages.${system}.pythonEnv
        ];
      } ''
        cd ${./.}
        export PYTHONPATH="${self.packages.${system}.pythonEnv}/${self.packages.${system}.pythonEnv.sitePackages}"
        ${self.packages.${system}.pythonEnv}/bin/python -m pytest tests/ -v
        touch $out
      '';
      
      # Run Prolog tests using Grimoire
      prolog-tests = pkgs.runCommand "prolog-tests" {
        buildInputs = [
          grimoire.packages.${system}.grimoire
          self.packages.${system}.pythonEnv
        ];
      } ''
        cd ${./.}
        export PYTHONPATH="${self.packages.${system}.pythonEnv}/${self.packages.${system}.pythonEnv.sitePackages}"
        ${grimoire.packages.${system}.grimoire}/bin/grimoire exec -g "run_tests" -t "halt" semantics.plt
        touch $out
      '';
    });
  };
}