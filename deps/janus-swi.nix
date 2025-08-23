{
  swipl,
  python3Packages,
  pkgs ? import <nixpkgs> {}
}:

python3Packages.buildPythonPackage rec {
  pname = "janus-swi";
  version = "main";
  
  pyproject = true;

  src = pkgs.fetchFromGitHub {
    owner = "SWI-Prolog";
    repo = "packages-swipy";
    rev = "a751822dc7eae86af873ddba41b255ae820698fe";
    sha256 = "sha256-pVe+by/VQQM3dfrLvhCzF4H0FwMO7GSHi/VrqQlni4s="; # This needs to be updated
  };
  
  build-system = with python3Packages; [
    setuptools
    setuptools-scm
  ];

  nativeBuildInputs = with pkgs; [
    pkg-config
    swipl
  ];

  buildInputs = with pkgs; [
    swipl
  ];

  # Skip tests for now as they may require specific Prolog setup
  doCheck = false;

  meta = with pkgs.lib; {
    description = "Janus - SWI-Prolog Python interface";
    homepage = "https://github.com/SWI-Prolog/packages-swipy";
    license = licenses.bsd2;
    maintainers = [ ];
  };
}