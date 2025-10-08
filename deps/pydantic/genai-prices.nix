{
  python3Packages,
  pkgs ? import <nixpkgs> {}
}:

python3Packages.buildPythonPackage rec {
  pname = "genai_prices";
  version = "0.0.27";

  pyproject = true;

  src = pkgs.fetchFromGitHub {
    owner = "pydantic";
    repo = "genai-prices";
    rev = "3e9bbcf4be110a0df22a4f9a50fb980aa60528bc";
    sha256 = "sha256-ZYHIQmC5Qlu77WDB0TnqS7/K85KQwwFyYTGqdUPUlAc=";
    leaveDotGit = true;
  };

  # Build from the packages/python subdirectory
  sourceRoot = "source/packages/python";

  build-system = with python3Packages; [
    uv-build
  ];
  
  nativeBuildInputs = [ pkgs.git ];

  dependencies = with python3Packages; [
    httpx
    pydantic
    typing-extensions  # for eval-type-backport on older Python
  ];

  # Skip tests for now as they may require API keys
  doCheck = false;

  meta = with pkgs.lib; {
    description = "Calculate prices for calling LLM inference APIs";
    homepage = "https://github.com/pydantic/genai-prices";
    license = licenses.mit;
    maintainers = [ ];
  };
}