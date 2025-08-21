{ pkgs }:

with pkgs;

{
  withPrologPacks = packList:
    let
      packString = lib.concatStringsSep " " (map (pack: "'${pack}'") packList);
    in
    stdenv.mkDerivation {
      name = "swipl-with-packs";
      version = swi-prolog.version;
      
      buildInputs = [
        swi-prolog
        sqlite        # Required for prosqlite
        openssl       # Required for SSL/TLS packs  
        cacert        # Required for HTTPS downloads
      ];
      
      nativeBuildInputs = [
        makeWrapper
      ];
      
      # No source needed - we're just installing packs
      dontUnpack = true;
      
      buildPhase = ''
        runHook preBuild
        
        # Create pack directory
        mkdir -p $out/lib/swipl/pack
        
        # Install each pack
        ${lib.concatMapStringsSep "\n" (pack: 
          "echo \"Installing pack: ${pack}\" && swipl pack install --dir=$out/lib/swipl/pack -y -q '${pack}'"
        ) packList}
        
        runHook postBuild
      '';
      
      installPhase = ''
        runHook preInstall
        
        # Create wrapper script
        mkdir -p $out/bin
        makeWrapper ${swi-prolog}/bin/swipl $out/bin/swipl \
          --set SWIPL_PACK_PATH "$out/lib/swipl/pack" \
          --prefix PATH : ${lib.makeBinPath [ sqlite openssl ]}
        
        # Copy other binaries from base SWI-Prolog  
        for bin in ${swi-prolog}/bin/*; do
          if [ "$(basename "$bin")" != "swipl" ]; then
            ln -s "$bin" "$out/bin/$(basename "$bin")"
          fi
        done
        
        runHook postInstall
      '';
      
      # Ensure we have certificates for HTTPS downloads
      SSL_CERT_FILE = "${cacert}/etc/ssl/certs/ca-bundle.crt";
      
      meta = with lib; {
        description = "SWI-Prolog with pre-installed packs: ${lib.concatStringsSep ", " packList}";
        homepage = "https://www.swi-prolog.org/";
        license = licenses.bsd2;
        maintainers = [ ];
        platforms = platforms.unix;
      };
    };
}