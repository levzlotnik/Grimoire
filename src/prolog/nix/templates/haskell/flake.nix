{
  description = "Haskell development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            ghc
            stack
            haskell-language-server
            git
          ];

          shellHook = ''
            echo "λ Haskell Development Environment"
            echo "GHC: $(ghc --version)"
            echo "Stack: $(stack --version)"
            echo ""
            echo "Run: ghc main.hs && ./main"
            echo "Or: runhaskell main.hs"
          '';
        };

        packages.default = pkgs.stdenv.mkDerivation {
          pname = "haskell-template";
          version = "0.1.0";
          src = ./.;

          buildInputs = with pkgs; [ ghc ];

          buildPhase = ''
            ghc -o main main.hs Lib.hs
          '';

          installPhase = ''
            mkdir -p $out/bin
            cp main $out/bin/
          '';
        };

        apps = {
          default = {
            type = "app";
            program = "${self.packages.${system}.default}/bin/main";
          };
        };
      });
}
