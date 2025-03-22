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
    ]
  );

in
pkgs.mkShell
{
  packages = [
    pkgs.swi-prolog
    pythonEnv
  ];

  SWIPL_BIN = "${pkgs.swi-prolog}/bin/swipl";
}
