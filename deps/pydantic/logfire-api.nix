{
  python3Packages,
  pkgs ? import <nixpkgs> {}
}:

python3Packages.buildPythonPackage rec {
  pname = "logfire-api";
  version = "4.3.6";

  pyproject = true;

  src = pkgs.fetchPypi {
    pname = "logfire_api";
    inherit version;
    sha256 = "sha256-wH5PoWXhXwsi5f85wCU7SNyaS0KrV0+w1j60AFDAeuE=";
  };

  build-system = with python3Packages; [
    hatchling
  ];

  dependencies = with python3Packages; [
    typing-extensions
  ];

  # Skip tests for now
  doCheck = false;

  meta = with pkgs.lib; {
    description = "Shim for Logfire SDK that does nothing unless Logfire is installed";
    homepage = "https://github.com/pydantic/logfire";
    license = licenses.mit;
    maintainers = [ ];
  };
}