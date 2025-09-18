{ lib
, python3Packages
, cmake
, ninja
, eigen
, openblas
, pkg-config
}:

python3Packages.buildPythonPackage rec {
  pname = "pybind11-demo";
  version = "1.0.0";
  
  src = ./.;
  
  pyproject = true;
  
  nativeBuildInputs = with python3Packages; [
    setuptools
    wheel
    pybind11
    cmake
    ninja
    pkg-config
  ];

  buildInputs = [
    eigen
    openblas
  ];

  propagatedBuildInputs = with python3Packages; [
    numpy
    pybind11
  ];

  checkInputs = with python3Packages; [
    pytest
    pytest-cov
    scipy
    matplotlib
  ];

  # Configure CMake build
  preConfigure = ''
    export CMAKE_PREFIX_PATH="${eigen}/include/eigen3:${openblas}"
  '';

  # Enable tests
  doCheck = true;
  
  checkPhase = ''
    runHook preCheck
    python -m pytest tests/ -v
    runHook postCheck
  '';

  meta = with lib; {
    description = "Comprehensive PyBind11 template showcasing all major features";
    longDescription = ''
      A comprehensive PyBind11 template for Grimoire that demonstrates:
      - Function bindings with various argument types
      - Class bindings with inheritance and virtual methods
      - STL container conversions
      - NumPy integration with buffer protocol
      - Exception handling and custom exceptions
      - Smart pointer usage
      - Operator overloading
      - Property access patterns
    '';
    homepage = "https://github.com/example/pybind11-demo";
    license = licenses.mit;
    maintainers = [ ];
    platforms = platforms.unix;
  };
}
