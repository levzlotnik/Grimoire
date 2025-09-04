{ lib, python3Packages, janus-swi }:

python3Packages.buildPythonPackage rec {
  pname = "grimoire-golems";
  version = "0.1.0";
  
  src = ./python;
  
  pyproject = true;
  build-system = with python3Packages; [ setuptools ];
  
  dependencies = with python3Packages; [
    pydantic
    openai
    anthropic
    groq
    janus-swi
  ];

  meta = with lib; {
    description = "Grimoire Golems AI Agent Framework";
    license = licenses.mit;
  };
}