{
  description = "Minimal test flake for Grimoire test suite";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      packages = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = pkgs.writeScriptBin "test-script" ''
            #!${pkgs.bash}/bin/bash
            echo "Test package from Grimoire test flake"
          '';

          hello = pkgs.writeScriptBin "test-hello" ''
            #!${pkgs.bash}/bin/bash
            echo "Hello from test flake"
          '';
        });

      apps = forAllSystems (system: {
        default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/test-script";
        };

        hello = {
          type = "app";
          program = "${self.packages.${system}.hello}/bin/test-hello";
        };
      });

      devShells = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = pkgs.mkShell {
            buildInputs = [ pkgs.hello ];
          };
        });
    };
}
