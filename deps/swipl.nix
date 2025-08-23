{ pkgs }:

let
  inherit (pkgs) lib;
  
  # Auto-import all packs from deps/prolog/
  prologPacks = {
    # Future packs will be added here automatically
  };
  
in

rec {
  # Base SWI-Prolog
  swi-prolog-base = pkgs.swi-prolog;
  
  # Expose packs for selection
  packs = prologPacks;
  
  # withPacks function using selector pattern
  withPacks = packSelector:
    let
      selectedPacks = packSelector prologPacks;
    in
    pkgs.stdenv.mkDerivation {
      name = "swipl-with-packs";
      version = swi-prolog-base.version;
      
      buildInputs = [ swi-prolog-base pkgs.makeWrapper ] ++ selectedPacks;
      
      dontUnpack = true;
      
      installPhase = ''
        # Create pack installation directory  
        mkdir -p $out/lib/swipl/pack
        
        # Install each selected pack
        ${lib.concatMapStringsSep "\n" (pack: ''
          echo "Installing pack: ${pack.pname}"
          mkdir -p $out/lib/swipl/pack/${pack.pname}
          cp -r ${pack}/* $out/lib/swipl/pack/${pack.pname}/
        '') selectedPacks}
        
        # Create wrapper binaries
        mkdir -p $out/bin
        
        # Wrap main swipl binary with proper environment
        makeWrapper ${swi-prolog-base}/bin/swipl $out/bin/swipl \
          --set SWIPL_PACK_PATH "$out/lib/swipl/pack" \
          --prefix LD_LIBRARY_PATH : ${lib.makeLibraryPath (lib.concatMap (pack: pack.buildInputs or []) selectedPacks)}
        
        # Link other SWI-Prolog binaries
        for bin in ${swi-prolog-base}/bin/*; do
          if [ "$(basename "$bin")" != "swipl" ]; then
            ln -s "$bin" "$out/bin/$(basename "$bin")"
          fi
        done
        
        # Expose SWI-Prolog libraries and headers for dependent packages
        mkdir -p $out/lib
        mkdir -p $out/include
        
        # Link the shared libraries
        for lib in ${swi-prolog-base}/lib/libswipl.so*; do
          ln -s "$lib" $out/lib/
        done
        
        # Copy the entire swipl library directory structure (contains architecture-specific libs)
        cp -r ${swi-prolog-base}/lib/swipl/* $out/lib/swipl/
        
        # Link headers
        if [ -d ${swi-prolog-base}/include ]; then
          cp -r ${swi-prolog-base}/include/* $out/include/
        fi
        
        # Link pkg-config file if it exists
        mkdir -p $out/share/pkgconfig
        if [ -f ${swi-prolog-base}/share/pkgconfig/swipl.pc ]; then
          ln -s ${swi-prolog-base}/share/pkgconfig/swipl.pc $out/share/pkgconfig/
        fi
      '';
      
      meta = swi-prolog-base.meta // {
        description = "SWI-Prolog with packs: ${lib.concatMapStringsSep ", " (pack: pack.pname) selectedPacks}";
      };
    };
}