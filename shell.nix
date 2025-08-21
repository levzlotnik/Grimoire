{ pkgs ? import <nixpkgs> { } }:

let
  pyswip = import ./deps/pyswip.nix { inherit pkgs; };
  swipl = import ./deps/swipl.nix { inherit pkgs; };
  swiplEnv = swipl.withPrologPacks [
    "prosqlite"
    "http"
    "ssl"
  ];
  python = pkgs.python313;
  pythonEnv = python.withPackages (
    p: with p; [
      flask
      requests
      pydantic
      black
      openai
      pyswip
      anthropic
      groq
      python-dotenv
      gitpython
    ]
  );

  schemaFile = ./src/db/schema.sql;

in
pkgs.mkShell
{
  packages = [
    swiplEnv
    pythonEnv
    pkgs.sqlite
  ];

  SWIPL_BIN = "${swiplEnv}/bin/swipl";
  LLM_DB_SCHEMA_PATH = toString schemaFile;
}
