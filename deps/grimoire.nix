{ pkgs }:

let
  # Import janus-swi Python package
  python = pkgs.python313;
  python3Packages = pkgs.python313Packages;

  # Import SWI-Prolog with packs system, using the same Python
  swipl = import ./swipl.nix { inherit pkgs python; };

  # Create SWI-Prolog environment with selected packs
  swiplEnv = swipl.withPacks (p: with p; [
    # No packs needed - using sqlite3 CLI directly
  ]);

  janus-swi = import ./janus-swi.nix {
    swipl = swiplEnv;
    inherit pkgs;
    inherit python3Packages;
  };

  # Create Python environment with all packages
  pythonEnv = python.withPackages (
    p: with p; [
      # Core Python packages
      requests
      python-dotenv
      gitpython
      janus-swi
      # API/Server packages
      fastapi
      uvicorn
      pydantic
      mcp  # Model Context Protocol SDK
      # Testing packages
      httpx
      pytest
      pytest-asyncio
      # Development packages
      black
      # LLM packages
      openai
      anthropic
      groq
      # Web framework
      flask
      # YAML support
      pyyaml
    ]
  );

  # Schema file path
  schemaFile = ../src/db/schema.sql;

in
{
  # Core components
  swipl = swiplEnv;
  python = pythonEnv;
  janus-swi = janus-swi;
  sqlite = pkgs.sqlite;
  
  # Environment variables
  env = {
    SWIPL_BIN = "${swiplEnv}/bin/swipl";
    LLM_DB_SCHEMA_PATH = toString schemaFile;
  };

  # All build inputs for shells/derivations
  buildInputs = [
    swiplEnv
    pythonEnv
    pkgs.sqlite
  ];

  # Helper function to create Grimoire executable
  mkGrimoireExecutable = { name, script }: pkgs.writeShellScriptBin name ''
    export SWIPL_BIN="${swiplEnv}/bin/swipl"
    export LLM_DB_SCHEMA_PATH="${toString schemaFile}"
    ${script}
  '';
}
