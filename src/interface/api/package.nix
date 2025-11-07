{ lib
, buildPythonPackage
, mkPythonEditablePackage
, setuptools
, wheel
, fastapi
, uvicorn
, pydantic
, pyyaml
, fastmcp
, janus-swi
, pytest
, pytest-asyncio
, httpx
, grimoire-flake
, system
}:

let
  pyproject = lib.importTOML ./pyproject.toml;

  # Common attributes shared between production and dev packages
  commonAttrs = {
    pname = "grimoire-py";
    version = "0.1.0";

    # Build environment
    SWIPL_BIN = "${(grimoire-flake.lib.getGrimoireEnv system).swipl}/bin/swipl";
    LLM_DB_SCHEMA_PATH = (grimoire-flake.lib.getGrimoireEnv system).env.LLM_DB_SCHEMA_PATH;
  };
in
{
  # Production package - immutable, in /nix/store
  grimoirePy = buildPythonPackage (commonAttrs // {
    src = ./.;
    format = "pyproject";

    nativeBuildInputs = [ setuptools wheel ];

    propagatedBuildInputs = [
      fastapi
      uvicorn
      pydantic
      pyyaml
      fastmcp
      janus-swi
    ];

    checkInputs = [
      pytest
      pytest-asyncio
      httpx
    ];

    # Skip tests during build (run separately)
    doCheck = false;
  });

  # Development package - editable, points to $GRIMOIRE_PY_ROOT
  grimoirePyDev = mkPythonEditablePackage {
    pname = "grimoire-py-dev";  # Different name to avoid conflicts
    inherit (commonAttrs) version;

    # Environment variable expansion - set in shell
    root = "$GRIMOIRE_PY_ROOT";

    # Entry points from pyproject.toml
    scripts = pyproject.project.scripts;

    # Explicit dependencies (from pyproject.toml)
    dependencies = [
      fastapi
      uvicorn
      pydantic
      pyyaml
      fastmcp
      janus-swi
    ];

    # Environment variables as derivation arguments
    derivationArgs = {
      inherit (commonAttrs) SWIPL_BIN LLM_DB_SCHEMA_PATH;
    };
  };
}
