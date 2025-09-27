{
  python3Packages,
  pkgs ? import <nixpkgs> {}
}:

let
  logfire-api = import ./logfire-api.nix { inherit python3Packages pkgs; };
  genai-prices = import ./genai-prices.nix { inherit python3Packages pkgs; };
  pydantic-graph = import ./pydantic-graph.nix { inherit python3Packages pkgs; };
in

python3Packages.buildPythonPackage rec {
  pname = "pydantic-ai-slim";
  version = "1.0.10";

  pyproject = true;

  # Override version for hatchling to avoid dynamic versioning
  preBuild = ''
    export SETUPTOOLS_SCM_PRETEND_VERSION="${version}"
  '';

  src = pkgs.fetchPypi {
    pname = "pydantic_ai_slim";
    inherit version;
    sha256 = "sha256-WSLZREcYrQ1dgU41KESpOii5/KoY0Cegl3YLD7aaPYI=";
  };

  build-system = with python3Packages; [
    hatchling
    uv-dynamic-versioning
  ];

  nativeBuildInputs = [ pkgs.git ];

  dependencies = with python3Packages; [
    griffe
    httpx
    pydantic
    typing-extensions  # for exceptiongroup on older Python
    opentelemetry-api
    logfire-api
    genai-prices
    pydantic-graph
    eval-type-backport
  ];

  # Skip tests for now as they may require API keys
  doCheck = false;
  
  # Don't install binaries to avoid conflicts with full pydantic-ai package
  postInstall = ''
    rm -rf $out/bin
  '';

  meta = with pkgs.lib; {
    description = "Pydantic AI Slim - Core AI agent framework without optional dependencies";
    homepage = "https://github.com/pydantic/pydantic-ai";
    license = licenses.mit;
    maintainers = [ ];
  };
}