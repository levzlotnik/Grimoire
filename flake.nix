{
  description = "Grimoire: A Knowledge-Based Operating System";

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

    # Apps for running Grimoire
    apps.${system} = {
      # Launch interactive Grimoire shell
      grimoire = {
        type = "app";
        program = "${pkgs.writeShellScript "grimoire" ''
          cd ${./.}
          exec ${pkgs.swi-prolog}/bin/swipl -s ${./repl.pl}
        ''}";
      };

      # Run all system tests
      test = {
        type = "app";
        program = "${pkgs.writeShellScript "grimoire-test" ''
          cd ${./.}
          exec ${pkgs.swi-prolog}/bin/swipl -g "consult('${./src/prolog/tests/run_tests.pl}'), run_tests, halt." -t 'halt(1).'
        ''}";
      };

      # Run tests and then open shell on success
      test-shell = {
        type = "app";
        program = "${pkgs.writeShellScript "grimoire-test-shell" ''
          cd ${./.}
          echo "🧪 Running Grimoire tests..."
          if ${pkgs.swi-prolog}/bin/swipl -g "consult('${./src/prolog/tests/run_tests.pl}'), run_tests, halt." -t 'halt(1).' > /dev/null 2>&1; then
            echo "✅ All tests passed! Opening Grimoire shell..."
            exec ${pkgs.swi-prolog}/bin/swipl -s ${./repl.pl}
          else
            echo "❌ Tests failed! Running tests with output..."
            exec ${pkgs.swi-prolog}/bin/swipl -g "consult('${./src/prolog/tests/run_tests.pl}'), run_tests, halt." -t 'halt(1).'
          fi
        ''}";
      };
    };

    inherit (nixos-templates) templates;
  };
}
