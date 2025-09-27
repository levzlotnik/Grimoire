{
  python3Packages,
  pkgs ? import <nixpkgs> {}
}:

let
  logfire-api = import ./logfire-api.nix { inherit python3Packages pkgs; };
in

python3Packages.buildPythonPackage rec {
  pname = "pydantic_graph";
  version = "1.0.10";

  pyproject = true;

  src = pkgs.fetchPypi {
    pname = "pydantic_graph";
    inherit version;
    sha256 = "sha256-/EZeqPKZlAmMQ9RMaVRdWRfiJA0edLcdTvHgbobeoiM=";
  };

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