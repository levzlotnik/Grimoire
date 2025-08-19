{ pkgs ? import <nixpkgs> { } }:

let
  pyswip = import ./deps/pyswip.nix { inherit pkgs; };
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
    pkgs.swi-prolog
    pythonEnv
    pkgs.sqlite
  ];

  SWIPL_BIN = "${pkgs.swi-prolog}/bin/swipl";
  LLM_DB_SCHEMA_PATH = toString schemaFile;
}
