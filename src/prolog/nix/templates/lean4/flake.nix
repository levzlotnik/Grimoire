{
  description = "Lean4 development environment";

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
            lean4
            elan
            git
            curl
          ];

          shellHook = ''
            echo "🔬 Lean4 Development Environment"
            echo "Lean: $(lean --version)"
            echo "Elan: $(elan --version)"
            echo ""
            echo "Build: lake build"
            echo "Run: lake exe LeanTemplate"
          '';
        };
      });
}
