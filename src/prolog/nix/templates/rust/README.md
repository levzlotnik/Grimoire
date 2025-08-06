# Rust Template

A simple Rust project template following Rust conventions.

## Project Philosophy

- **Main binary** (`src/main.rs`) is the primary entry point - most visible use case
- **Library code** goes in `src/lib.rs` and `src/` modules - for reusable components
- **Additional binaries** can go in `src/bin/` if you need multiple executables
- Follow Cargo conventions: the most common use case should be `cargo run`

## Quick Start

### Using Nix (recommended)

#### Development Shell

```bash
# Enter development environment
nix develop

# Run the main binary (most common use case)
cargo run

# Build the project
cargo build

# Run tests
cargo test

# Format and lint
cargo fmt
cargo clippy
```

#### Direct Execution

```bash
# Build the package
nix build

# Run the built package executable
nix run

# Or run with cargo directly
nix run .#run
```

### Using Docker

```bash
# Build the image
docker build -t rust-project .

# Run the container
docker run rust-project
```

## Project Structure

- `src/main.rs` - **Main binary** (primary entry point, most common use case)
- `src/lib.rs` - **Library code** (reusable modules, functions, structs)
- `src/bin/` - **Additional binaries** (if you need multiple executables)
- `Cargo.toml` - Rust project configuration and dependencies
- `flake.nix` - Nix development environment
- `Dockerfile` - Docker container configuration

## Development Workflow

1. **Start simple**: Put your main logic in `src/main.rs`
2. **Extract reusable code**: Move functions/structs to `src/lib.rs` as your project grows
3. **Add more binaries**: Use `src/bin/` if you need multiple entry points
4. **Manage dependencies**: Add crates to `Cargo.toml`
