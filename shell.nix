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

in
pkgs.mkShell
{
  packages = [
    pkgs.swi-prolog
    pythonEnv
    pkgs.sqlite
  ];

  SWIPL_BIN = "${pkgs.swi-prolog}/bin/swipl";
}
