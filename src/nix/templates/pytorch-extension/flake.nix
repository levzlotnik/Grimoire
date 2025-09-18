{
  description = "PyTorch C++ Extension Template";

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

      # Python environment with PyTorch and development tools
      pythonEnv = pkgs.python3.withPackages (ps: with ps; [
        torch
        numpy
        pytest
        pytest-benchmark
        setuptools
        wheel
        pybind11
      ]);

      # C++ build environment
      cppEnv = pkgs.stdenv.mkDerivation {
        name = "pytorch-cpp-env";
        buildInputs = with pkgs; [
          cmake
          pkg-config
          openmp
          pythonEnv
        ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
          glibc.dev
        ];
      };

      # Custom PyTorch extension package
      pytorch-custom-ops = pkgs.stdenv.mkDerivation {
        name = "pytorch-custom-ops";
        src = ./.;
        
        buildInputs = [
          pythonEnv
          pkgs.cmake
          pkgs.openmp
        ];

        buildPhase = ''
          export PYTHONPATH=${pythonEnv}/${pythonEnv.sitePackages}
          ${pythonEnv}/bin/python setup.py build_ext --inplace
        '';

        installPhase = ''
          mkdir -p $out
          cp -r . $out/
        '';
      };

    in
    {
      grimoireEnv = grimoireEnv;
      pythonEnv = pythonEnv;
      pytorch-custom-ops = pytorch-custom-ops;
      default = pytorch-custom-ops;
    });

    devShells = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      grimoireEnv = self.lib.getGrimoireEnv system;
    in
    {
      default = pkgs.mkShell (grimoireEnv.env // {
        buildInputs = [ 
          self.packages.${system}.pythonEnv
          grimoireEnv.swipl
          pkgs.cmake
          pkgs.pkg-config
          pkgs.openmp
          pkgs.gdb
          pkgs.valgrind
        ];
        
        PYTHONPATH = "${self.packages.${system}.pythonEnv}/${self.packages.${system}.pythonEnv.sitePackages}";
        PYTHON_EXECUTABLE = "${self.packages.${system}.pythonEnv}/bin/python";
        PATH = "${self.packages.${system}.pythonEnv}/bin:$PATH";
        
        # Enable OpenMP for CPU parallelization
        OMP_NUM_THREADS = "4";
        
        shellHook = ''
          echo "PyTorch C++ Extension Development Environment"
          echo "Python with PyTorch: ${self.packages.${system}.pythonEnv}/bin/python"
          echo "SWI-Prolog: ${grimoireEnv.swipl}/bin/swipl"
          echo ""
          echo "Build extension: python setup.py build_ext --inplace"
          echo "Run tests: python -m pytest tests/"
          echo "Run benchmarks: python -m pytest benchmarks/ --benchmark-only"
          echo "Test semantics: grimoire test -- semantics.plt"
        '';
      });
    });

    checks = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      # Build and test the extension
      pytorch-tests = pkgs.runCommand "pytorch-tests" {
        buildInputs = [
          self.packages.${system}.pythonEnv
          pkgs.cmake
          pkgs.openmp
        ];
      } ''
        cd ${./.}
        export PYTHONPATH=${self.packages.${system}.pythonEnv}/${self.packages.${system}.pythonEnv.sitePackages}
        ${self.packages.${system}.pythonEnv}/bin/python setup.py build_ext --inplace
        ${self.packages.${system}.pythonEnv}/bin/python -m pytest tests/ -v
        touch $out
      '';

      # Run Prolog tests
      prolog-tests = pkgs.runCommand "prolog-tests" {
        buildInputs = [
          grimoire.packages.${system}.grimoire
          self.packages.${system}.pythonEnv
        ];
      } ''
        cd ${./.}
        ${grimoire.packages.${system}.grimoire}/bin/grimoire exec -g "run_tests" -t "halt" semantics.plt
        touch $out
      '';
    });
  };
}