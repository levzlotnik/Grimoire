{
  python3Packages,
  pkgs ? import <nixpkgs> {}
}:

let
  logfire-api = import ./logfire-api.nix { inherit python3Packages pkgs; };
in

python3Packages.buildPythonPackage rec {
  pname = "pydantic_graph";
  version = "0.2.20";

  pyproject = true;

  src = pkgs.fetchFromGitHub {
    owner = "pydantic";
    repo = "pydantic-ai";
    rev = "bfcccbab4fdf1949f38df674f07bcbfc1c3d838a";
    sha256 = "sha256-EtsmOvVtv5+426oAKNUBBgKmxN0BVSGFGFgBcTgnOLc=";
    leaveDotGit = true;
  };

  # Build from the pydantic_graph subdirectory
  sourceRoot = "source/pydantic_graph";

  build-system = with python3Packages; [
    hatchling
    uv-dynamic-versioning
  ];

  nativeBuildInputs = [ pkgs.git ];

  dependencies = with python3Packages; [
    httpx
    pydantic
    typing-extensions  # for typing-inspection
    logfire-api
  ];

  # Skip tests for now
  doCheck = false;

  meta = with pkgs.lib; {
    description = "Graph and finite state machine library for Python with type safety";
    homepage = "https://ai.pydantic.dev/graph";
    license = licenses.mit;
    maintainers = [ ];
  };
}