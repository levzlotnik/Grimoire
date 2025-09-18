{
  description = "Prolog-C Binding Pattern Template";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    grimoire.url = "path:../../../..";
  };

  outputs = { self, nixpkgs, grimoire, ... }:
  let
    systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
    forAllSystems = nixpkgs.lib.genAttrs systems;
  in
  {
    lib = {
      getGrimoireEnv = system: self.packages.${system}.grimoireEnv;
    };

    packages = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      grimoireEnv = grimoire.lib.getGrimoireEnv system;

      # Build the C foreign library
      prolog-c-binding = pkgs.stdenv.mkDerivation {
        pname = "prolog-c-binding";
        version = "1.0.0";
        
        src = ./.;
        
        nativeBuildInputs = [ 
          grimoireEnv.swipl
          pkgs.gcc
          pkgs.gnumake
        ];
        
        buildInputs = [
          grimoireEnv.swipl
        ];

        # Set up SWI-Prolog environment
        configurePhase = ''
          export SWIPLHOME=${grimoireEnv.swipl}/lib/swipl
          export SWIPL=${grimoireEnv.swipl}/bin/swipl
          export PATH=${grimoireEnv.swipl}/bin:$PATH
        '';

        buildPhase = ''
          # Create build directory
          mkdir -p build
          
          # Compile the C library using swipl-ld
          ${grimoireEnv.swipl}/bin/swipl-ld -shared -o build/prolog_c_binding.so src/prolog_c_binding.c
        '';

        checkPhase = ''
          # Basic functionality test
          ${grimoireEnv.swipl}/bin/swipl -g "
            use_foreign_library('./build/prolog_c_binding.so'),
            add_numbers(5, 3, R),
            (R =:= 8 -> writeln('C binding test passed') ; (writeln('C binding test failed'), halt(1))),
            halt
          " -t "halt(1)"
        '';

        installPhase = ''
          mkdir -p $out/lib
          mkdir -p $out/share/prolog
          
          # Install the shared library
          cp build/prolog_c_binding.so $out/lib/
          
          # Install Prolog source files
          cp auxiliary.pl $out/share/prolog/
          cp semantics.pl $out/share/prolog/
          cp semantics.plt $out/share/prolog/
          
          # Create wrapper scripts
          mkdir -p $out/bin
          cat > $out/bin/test-c-binding << 'EOF'
#!/bin/bash
cd $out/share/prolog
${grimoireEnv.swipl}/bin/swipl -g "consult('semantics.pl'), halt" -t "halt(1)"
EOF
          chmod +x $out/bin/test-c-binding
          
          cat > $out/bin/run-tests << 'EOF'
#!/bin/bash
cd $out/share/prolog
${grimoireEnv.swipl}/bin/swipl -g "run_tests" -t "halt" semantics.plt
EOF
          chmod +x $out/bin/run-tests
        '';

        doCheck = true;

        meta = with pkgs.lib; {
          description = "SWI-Prolog C foreign function interface example";
          license = licenses.mit;
          maintainers = [ ];
          platforms = platforms.unix;
        };
      };

    in
    {
      grimoireEnv = grimoireEnv;
      prolog-c-binding = prolog-c-binding;
      default = prolog-c-binding;
    });

    devShells = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      grimoireEnv = self.lib.getGrimoireEnv system;
    in
    {
      default = pkgs.mkShell (grimoireEnv.env // {
        buildInputs = [ 
          grimoireEnv.swipl
          pkgs.gcc
          pkgs.gnumake
          pkgs.gdb
          pkgs.valgrind
          pkgs.cppcheck
          pkgs.clang-tools
        ];
        
        # Set up SWI-Prolog environment
        SWIPLHOME = "${grimoireEnv.swipl}/lib/swipl";
        SWIPL = "${grimoireEnv.swipl}/bin/swipl";
        PATH = "${grimoireEnv.swipl}/bin:${pkgs.gcc}/bin:${pkgs.gnumake}/bin";
        
        shellHook = ''
          echo "Prolog-C Binding Pattern Development Environment"
          echo "SWI-Prolog: ${grimoireEnv.swipl}/bin/swipl"
          echo "GCC: ${pkgs.gcc}/bin/gcc"
          echo "Make: ${pkgs.gnumake}/bin/make"
          echo ""
          echo "Available commands:"
          echo "  make            - Build the C library"
          echo "  make test       - Quick test of C predicates"
          echo "  make test-full  - Run complete test suite"
          echo "  grimoire exec   - Run with Grimoire"
          echo "  grimoire test   - Run Grimoire tests"
          echo ""
          echo "Development tools:"
          echo "  gdb             - GNU Debugger"
          echo "  valgrind        - Memory checker"
          echo "  cppcheck        - Static analysis"
          echo "  clang-format    - Code formatter"
        '';
      });
    });

    apps = forAllSystems (system: {
      # Run the basic test
      test = {
        type = "app";
        program = "${self.packages.${system}.prolog-c-binding}/bin/test-c-binding";
      };
      
      # Run the full test suite
      test-full = {
        type = "app";
        program = "${self.packages.${system}.prolog-c-binding}/bin/run-tests";
      };
      
      # Interactive SWI-Prolog with the library loaded
      repl = {
        type = "app";
        program = let
          repl-script = nixpkgs.legacyPackages.${system}.writeShellScript "prolog-c-repl" ''
            cd ${self.packages.${system}.prolog-c-binding}/share/prolog
            ${grimoire.lib.getGrimoireEnv system}.swipl -g "consult('semantics.pl')"
          '';
        in "${repl-script}";
      };
    });

    checks = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      # Run C library tests
      c-binding-tests = pkgs.runCommand "c-binding-tests" {
        buildInputs = [
          grimoire.packages.${system}.grimoire
          self.packages.${system}.prolog-c-binding
        ];
      } ''
        cd ${self.packages.${system}.prolog-c-binding}/share/prolog
        
        # Test basic C functionality
        ${grimoire.lib.getGrimoireEnv system}.swipl -g "
          use_foreign_library('${self.packages.${system}.prolog-c-binding}/lib/prolog_c_binding.so'),
          add_numbers(10, 5, R1), (R1 =:= 15 -> true ; halt(1)),
          multiply_floats(2.5, 4.0, R2), (R2 =:= 10.0 -> true ; halt(1)),
          string_length(hello, R3), (R3 =:= 5 -> true ; halt(1)),
          writeln('All basic C tests passed'),
          halt
        " -t "halt(1)"
        
        # Run full PLUnit test suite
        ${grimoire.lib.getGrimoireEnv system}.swipl -g "run_tests" -t "halt" semantics.plt
        
        touch $out
      '';
      
      # Run Grimoire integration tests
      grimoire-tests = pkgs.runCommand "grimoire-tests" {
        buildInputs = [
          grimoire.packages.${system}.grimoire
        ];
      } ''
        cd ${./.}
        ${grimoire.packages.${system}.grimoire}/bin/grimoire exec -g "run_tests" -t "halt" semantics.plt
        touch $out
      '';
    });
  };
}