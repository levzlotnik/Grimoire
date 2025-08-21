{ pkgs }:

let
  inherit (pkgs) lib;
  
  # Auto-import all packs from deps/prolog/
  prologPacks = {
    prosqlite = import ./prolog/prosqlite.nix { inherit pkgs; };
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
      '';
      
      meta = swi-prolog-base.meta // {
        description = "SWI-Prolog with packs: ${lib.concatMapStringsSep ", " (pack: pack.pname) selectedPacks}";
      };
    };
}