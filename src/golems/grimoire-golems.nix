{ lib, buildPythonPackage, setuptools, pydantic, openai, anthropic, groq, janus-swi }:

buildPythonPackage rec {
  pname = "grimoire-golems";
  version = "0.1.0";
  
  src = ./python;
  
  pyproject = true;
  build-system = [ setuptools ];
  
  dependencies = [
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