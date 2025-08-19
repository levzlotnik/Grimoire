{
  description = "Grimoire: A Knowledge-Based Operating System";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };


  outputs = { self, nixpkgs, ... }@inputs:
  let
    systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
    forAllSystems = nixpkgs.lib.genAttrs systems;
  in
  {
    devShells = forAllSystems (system: {
      default = import ./shell.nix {
        pkgs = nixpkgs.legacyPackages.${system};
      };
    });

    # Apps for running Grimoire
    apps = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      # Launch interactive Grimoire shell
      grimoire = {
        type = "app";
        program = "${pkgs.writeShellScript "grimoire" ''
          cd ${./.}
          exec ./grimoire repl
        ''}";
      };

      # Run all system tests
      test = {
        type = "app";
        program = "${pkgs.writeShellScript "grimoire-test" ''
          cd ${./.}
          exec ./grimoire test
        ''}";
      };
    });
  };
}
