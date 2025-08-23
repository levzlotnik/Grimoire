{
  description = "A collection of project templates for various programming languages";

  outputs = { self }: {

    templates = {
    cpp = {
      path = ./cpp;
      description = "A C++ template with Nix and Docker support";
      welcomeText = ''
        # Getting started
        - Run `nix develop` to enter the development environment
        - Run `cmake -B build && cmake --build build` to build the project
        - Run `./build/main` to execute the program
      '';
    };

    haskell = {
      path = ./haskell;
      description = "A Haskell template with Nix and Docker support";
      welcomeText = ''
        # Getting started
        - Run `nix develop` to enter the development environment
        - Run `stack run` to execute the program
        - Run `stack build` to build the project
      '';
    };

    lean4 = {
      path = ./lean4;
      description = "A Lean 4 template with Nix and Docker support";
      welcomeText = ''
        # Getting started
        - Run `nix develop` to enter the development environment
        - Run `lake build` to build the project
        - Run `lake exe main` to execute the program
      '';
    };

    mkdocs = {
      path = ./mkdocs;
      description = "An MkDocs template with Material theme, Nix, and Docker support";
      welcomeText = ''
        # Getting started
        - Run `nix develop` to enter the development environment
        - Run `mkdocs serve` to start the local development server
        - Run `nix run .#build` to build the static site
        - Run `nix run .#deploy` to deploy to GitHub Pages
      '';
    };

    python = {
      path = ./python;
      description = "A Python template with Nix and Docker support";
      welcomeText = ''
        # Getting started
        - Run `nix develop` to enter the development environment
        - Run `python main.py` to execute the main script
        - Run `python -m build` to build the package
      '';
    };

    python-rest-api = {
      path = ./python-rest-api;
      description = "A Python REST API template with FastAPI and Nix support";
      welcomeText = ''
        # Getting started
        - Run `nix develop` to enter the development environment
        - Run `nix run .#run` or `uvicorn main:app --reload` to start the API server
        - Visit http://localhost:8000/docs for interactive API documentation
        - Run `nix run .#test` or `pytest` to run tests
      '';
    };

    rust = {
      path = ./rust;
      description = "A Rust template with Nix and Docker support";
      welcomeText = ''
        # Getting started
        - Run `nix develop` to enter the development environment
        - Run `cargo run` to execute the program
        - Run `cargo build` to build the project
      '';
    };
  };

  # The default template to use when none is specified with `nix flake new`
  defaultTemplate = self.templates.python;

  };
}
