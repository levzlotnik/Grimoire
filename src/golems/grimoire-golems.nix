{ lib, buildPythonPackage, setuptools, pydantic, pydantic-ai, openai, anthropic, groq, janus-swi, grimoire-py }:

buildPythonPackage rec {
  pname = "grimoire-golems";
  version = "0.1.0";
  
  src = ./python;
  
  pyproject = true;
  build-system = [ setuptools ];
  
  dependencies = [
    pydantic
    pydantic-ai
    openai
    anthropic
    groq
    janus-swi
    grimoire-py
  ];

  meta = with lib; {
    description = "Grimoire Golems AI Agent Framework";
    license = licenses.mit;
  };
}