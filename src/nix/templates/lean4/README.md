# Lean4 Template

A simple Lean4 project template following Lean conventions.

## Project Philosophy

- **Main file** (`LeanTemplate.lean`) is the primary entry point - most visible use case
- **Library code** organized in `LeanTemplate/` directory
- **Lake integration** for proper Lean4 project management
- **Nix development environment** for reproducible builds
- Follow Lean conventions: most common use case should be `lake exe LeanTemplate`

## Quick Start

### Using Nix (recommended)

#### Development Shell

```bash
# Enter development environment
nix develop

# Build the project (most common use case)
lake build

# Run the project
lake exe LeanTemplate

# Interactive mode
lean --server

# Check a specific file
lean LeanTemplate.lean
```

#### Direct Execution (development shell)

```bash
# Build and run without entering the shell
nix develop -c sh -c "lake build && lake exe LeanTemplate"
```

**Note**: `nix run` is not supported for this template because Lake manages dependencies differently from Nix. The dependencies (including mathlib) need to be downloaded into `.lake/packages/` at runtime, which conflicts with Nix's read-only build environment. Use the development shell approach instead.

### Using Docker

```bash
# Build the image
docker build -t lean4-project .

# Run the container
docker run lean4-project
```

## Project Structure

- `LeanTemplate.lean` - **Main file** (primary entry point, most common use case)
- `LeanTemplate/` - **Library modules** (definitions, theorems, reusable components)
- `lakefile.toml` - **Lake configuration** (project settings, dependencies)
- `flake.nix` - Nix development environment
- `Dockerfile` - Docker container configuration

## Development Workflow

1. **Start simple**: Put your main definitions in `LeanTemplate.lean`
2. **Extract reusable code**: Create modules in `LeanTemplate/` directory
3. **Manage dependencies**: Use Lake for external Lean libraries
4. **Interactive development**: Use Lean server mode for real-time feedback
