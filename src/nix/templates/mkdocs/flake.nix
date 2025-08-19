{
  description = "MkDocs with Material theme development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Python environment with MkDocs and Material theme
        mkdocsEnv = pkgs.python312.withPackages (ps: with ps; [
          mkdocs
          mkdocs-material
          mkdocs-material-extensions
          pymdown-extensions
          mkdocs-minify-plugin
          mkdocs-redirects
        ]);

        # Script to build the site
        buildScript = pkgs.writeShellScriptBin "build-mkdocs" ''
          # Create a temporary directory for building
          TEMP_DIR=$(mktemp -d)
          cp -r ${./.}/* $TEMP_DIR/
          cd $TEMP_DIR
          chmod -R u+w .
          ${mkdocsEnv}/bin/mkdocs build
        '';

        # Script to serve the site locally
        serveScript = pkgs.writeShellScriptBin "serve-mkdocs" ''
          cd ${./.}
          ${mkdocsEnv}/bin/mkdocs serve
        '';

        # Script to deploy to GitHub Pages
        deployScript = pkgs.writeShellScriptBin "deploy-mkdocs" ''
          cd ${./.}
          ${mkdocsEnv}/bin/mkdocs gh-deploy
        '';

      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            mkdocsEnv
            git
            nodejs # For additional plugins if needed
          ];

          shellHook = ''
            echo "📚 MkDocs Development Environment with Material Theme"
            echo "MkDocs: $(mkdocs --version)"
            echo ""
            echo "Available commands:"
            echo "  mkdocs build     - Build the site"
            echo "  mkdocs serve     - Serve locally on http://127.0.0.1:8000"
            echo "  mkdocs gh-deploy - Deploy to GitHub Pages"
            echo ""
            echo "Or use Nix apps:"
            echo "  nix run .#build  - Build the site"
            echo "  nix run .#serve  - Serve locally"
            echo "  nix run .#deploy - Deploy to GitHub Pages"
          '';
        };

        packages = {
          default = pkgs.stdenv.mkDerivation {
            name = "mkdocs-template-site";
            src = ./.;
            buildInputs = [ mkdocsEnv ];
            buildPhase = ''
              ${mkdocsEnv}/bin/mkdocs build
            '';
            installPhase = ''
              mkdir -p $out
              cp -r site/* $out/
            '';
          };
        };

        apps = {
          default = {
            type = "app";
            program = "${buildScript}/bin/build-mkdocs";
          };
          build = {
            type = "app";
            program = "${buildScript}/bin/build-mkdocs";
          };
          serve = {
            type = "app";
            program = "${serveScript}/bin/serve-mkdocs";
          };
          deploy = {
            type = "app";
            program = "${deployScript}/bin/deploy-mkdocs";
          };
        };
      });
}
