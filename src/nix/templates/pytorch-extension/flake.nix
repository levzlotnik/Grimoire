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
      getGrimoireEnv = system: grimoire.lib.getGrimoireEnv system;
    };

    packages = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      grimoireEnv = grimoire.lib.getGrimoireEnv system;
    in rec
    {
      # Import the custom PyTorch extension Python package as a separate derivation
      pytorch-custom-ops = import ./pytorch-custom-ops.nix { inherit pkgs; };

      default = pytorch-custom-ops;

    });

    devShells = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      grimoireEnv = self.lib.getGrimoireEnv system;
      pythonEnv = pkgs.python313.withPackages (ps: with ps; [
        numpy
        setuptools
        wheel
        cmake
        pytest
        pytest-benchmark
        torch
        torchvision
        torchaudio
        # Include the custom ops package in the environment
        self.packages.${system}.pytorch-custom-ops
      ]);
    in rec
    {
      default = pkgs.mkShell (grimoireEnv.env // {
        buildInputs = [
          grimoireEnv.swipl
          pkgs.pkg-config
          pkgs.llvmPackages.openmp
          pkgs.gdb
          pkgs.valgrind
        ];

        packages = with pkgs; [
          pythonEnv
        ];

        # Enable OpenMP for CPU parallelization
        OMP_NUM_THREADS = "4";

        shellHook = ''
          echo "PyTorch C++ Extension Development Environment"
          echo "Python with PyTorch: ${pythonEnv}/bin/python"
          echo "SWI-Prolog: ${grimoireEnv.swipl}/bin/swipl"
          echo ""
          echo "Extension is pre-built and available in Python environment"
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
          pkgs.llvmPackages.openmp
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
