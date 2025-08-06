# Haskell Template

A simple Haskell project template following Haskell conventions.

## Project Philosophy

- **main.hs** is the primary entry point - most visible use case
- **Library modules** in separate `.hs` files for reusable components
- **Module hierarchy** (e.g., `Lib/Utils.hs`) as project grows
- Follow Haskell conventions: most common use case should be `runhaskell main.hs`

## Quick Start

### Using Nix (recommended)

#### Development Shell

```bash
# Enter development environment
nix develop

# Run directly (most common use case)
runhaskell main.hs

# Or compile and run
ghc main.hs && ./main

# Interactive development
ghci main.hs

# Load modules in REPL
ghci
> :load main.hs
```

#### Direct Execution

```bash
# Build the package
nix build

# Run the built package
nix run

# Or run with runhaskell directly
nix run .#run
```

### Using Docker

```bash
# Build the image
docker build -t haskell-project .

# Run the container
docker run haskell-project
```

## Project Structure

- `main.hs` - **Main executable** (primary entry point, most common use case)
- `*.hs` - **Module files** (functions, types, reusable components)
- `Lib/` - **Module hierarchy** (organized library code)
- `flake.nix` - Nix development environment
- `Dockerfile` - Docker container configuration

## Development Workflow

1. **Start simple**: Put your main logic in `main.hs`
2. **Extract reusable code**: Create separate modules for functions and types
3. **Organize with hierarchy**: Use directories for larger module structures
4. **Interactive development**: Use `ghci` for REPL-driven development
