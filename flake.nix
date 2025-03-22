{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    nixos-templates.url = "github:NixOS/templates";
  };

  outputs = { self, nixpkgs, nixos-templates, ... }@inputs:
  let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in
  {
    devShells.${system}.default =
      import ./shell.nix { pkgs = pkgs; };

    inherit (nixos-templates) templates;
  };
}
