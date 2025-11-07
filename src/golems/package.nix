{ lib
, buildPythonPackage
, mkPythonEditablePackage
, setuptools
, pydantic
, pydantic-ai
, openai
, anthropic
, groq
, janus-swi
, grimoire-py
, grimoire-py-dev
}:

let
  pyproject = lib.importTOML ./python/pyproject.toml;

  # Common attributes shared between production and dev packages
  commonAttrs = {
    pname = "grimoire-golems";
    version = "0.1.0";
  };
in
{
  # Production package - immutable, in /nix/store
  grimoireGolems = buildPythonPackage (commonAttrs // {
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
  });

  # Development package - editable, points to $GRIMOIRE_GOLEMS_ROOT
  grimoireGolemsDev = mkPythonEditablePackage {
    pname = "grimoire-golems-dev";  # Different name to avoid conflicts
    inherit (commonAttrs) version;

    # Environment variable expansion - set in shell
    root = "$GRIMOIRE_GOLEMS_ROOT";

    # No scripts (library only) - could be empty {} or omit
    scripts = {};

    # Explicit dependencies (from pyproject.toml + extras)
    dependencies = [
      pydantic
      pydantic-ai
      openai
      anthropic
      groq
      janus-swi
      grimoire-py-dev
    ];
  };
}
