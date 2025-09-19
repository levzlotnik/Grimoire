{ pkgs }:

pkgs.python3.pkgs.buildPythonPackage {
  pname = "pytorch-custom-ops";
  version = "0.1.0";
  src = ./.;
  format = "pyproject";

  buildInputs = with pkgs; [
    llvmPackages.openmp
  ];

  propagatedBuildInputs = with pkgs.python3.pkgs; [
    torch
    numpy
    setuptools
  ];

  nativeBuildInputs = with pkgs; [
    pkg-config
  ] ++ (with pkgs.python3.pkgs; [
    setuptools
    wheel
  ]);

  # # Disable cmake since we use torch.utils.cpp_extension
  # dontUseCmakeConfigure = true;
  # dontConfigure = true;

  # Build with proper environment
  preBuild = ''
    export OMP_NUM_THREADS=4
  '';
}