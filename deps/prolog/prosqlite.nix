{ pkgs }:

pkgs.stdenv.mkDerivation {
  pname = "prosqlite";
  version = "2.0.0";
  
  src = pkgs.fetchFromGitHub {
    owner = "nicos-angelopoulos";
    repo = "prosqlite";
    rev = "831482c8f267e002147dc482c4e6509f9e27d97e";
    sha256 = "sha256-9HOHKZdDbE/oClxQRaDT4dLdAYbgA0y7zl8IpGP5Miw=";
  };
  
  # Build dependencies needed for compilation
  nativeBuildInputs = with pkgs; [
    gcc
    gnumake
    pkg-config
  ];
  
  # Runtime dependencies
  buildInputs = with pkgs; [
    sqlite
    swi-prolog
  ];
  
  # Build phase - use prosqlite's expected build environment
  buildPhase = ''
    runHook preBuild
    
    # Set up prosqlite build environment based on buildenv.sh
    export PATH="${pkgs.swi-prolog}/bin:$PATH"
    export SWIPL="${pkgs.swi-prolog}/bin/swipl"  
    export SWIHOME="${pkgs.swi-prolog}"
    export SWIARCH="x86_64-linux"
    export PACKSODIR="lib/x86_64-linux"
    export SWISOLIB=""
    export SWILIB="-lswipl"
    export CC="gcc"
    export LD="swipl-ld"
    export CFLAGS="-fno-strict-aliasing -pthread -fPIC -I${pkgs.swi-prolog}/lib/swipl/include"
    export LDSOFLAGS="-rdynamic -O2 -pthread -Wl,-rpath=${pkgs.swi-prolog}/lib/x86_64-linux -shared"
    export SOEXT="so"
    
    # Also add SQLite to the environment
    export CFLAGS="$CFLAGS -I${pkgs.sqlite.dev}/include"
    export LDFLAGS="-L${pkgs.sqlite.out}/lib"
    export LIBS="-lsqlite3"
    
    # Use the original Makefile now that environment is set up
    echo "Building with prosqlite Makefile..."
    make all
    
    runHook postBuild
  '';
  
  # Install phase - copy to $out
  installPhase = ''
    runHook preInstall
    
    # Install the complete pack structure
    mkdir -p $out
    cp -r . $out/
    
    # Ensure shared objects are installed correctly
    find $out -name "*.so" -exec chmod +x {} \;
    
    runHook postInstall
  '';
  
  meta = with pkgs.lib; {
    description = "A Prolog interface to the SQLite database system";
    homepage = "https://github.com/nicos-angelopoulos/prosqlite";
    license = licenses.mit;
    platforms = platforms.unix;
  };
}