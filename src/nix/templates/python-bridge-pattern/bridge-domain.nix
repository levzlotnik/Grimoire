{ lib, python3Packages, janus-swi }:

python3Packages.buildPythonPackage rec {
  pname = "bridge-domain";
  version = "0.1.0";
  
  src = ./python;
  
  pyproject = true;
  build-system = with python3Packages; [ setuptools ];
  
  dependencies = with python3Packages; [
    pydantic
    janus-swi
  ];
  
  meta = with lib; {
    description = "Python-Prolog bridge pattern template";
    license = licenses.mit;
  };
}