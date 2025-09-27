{
  python3Packages,
  pkgs ? import <nixpkgs> {}
}:

let
  pydantic-ai-slim = import ./pydantic-ai-slim.nix { inherit python3Packages pkgs; };
in

python3Packages.buildPythonPackage rec {
  pname = "pydantic-ai";
  version = "1.0.10";  # Set a fixed version to avoid dynamic versioning

  pyproject = true;

  # Override version for hatchling to avoid dynamic versioning
  preBuild = ''
    export SETUPTOOLS_SCM_PRETEND_VERSION="${version}"
  '';

  src = pkgs.fetchPypi {
    pname = "pydantic_ai";
    inherit version;
    sha256 = "sha256-uCGDFdFX5DuKBZynTbL1Fbl6IijgmjmFXybSEUJ+QEw=";
  };

  build-system = with python3Packages; [
    hatchling
    uv-dynamic-versioning
  ];

  nativeBuildInputs = [ pkgs.git ];

  dependencies = with python3Packages; [
    pydantic
    pydantic-core
    httpx
    typing-extensions
    pydantic-ai-slim
  ];

  # Optional dependencies for specific providers
  optional-dependencies = {
    openai = with python3Packages; [ openai ];
    anthropic = with python3Packages; [ anthropic ];
    google = with python3Packages; [ google-generativeai ];
    groq = with python3Packages; [ groq ];
    mistral = with python3Packages; [ mistralai ];
    cohere = with python3Packages; [ cohere ];
    # Add more as needed
  };

  # Skip tests for now as they may require API keys
  doCheck = false;

  meta = with pkgs.lib; {
    description = "Pydantic AI - Agent framework for building production-grade generative AI applications";
    homepage = "https://github.com/pydantic/pydantic-ai";
    license = licenses.mit;
    maintainers = [ ];
  };
}