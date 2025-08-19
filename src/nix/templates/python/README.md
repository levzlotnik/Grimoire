# Python Template

A simple Python project template following clear separation principles.

## Project Philosophy

- **Main scripts** (like `main.py`) go at the **top level** - users should see them immediately
- **Library code** goes in `src/` - this is your reusable, importable code
- **Additional scripts** (if you have many) can go in `scripts/` directory
- Keep it simple: the most common use case should be the most visible

## Quick Start

### Using Nix (recommended)

#### Development Shell

```bash
# Enter development environment
nix develop

# Run the main script (most common use case)
python main.py

# Install in development mode to test packaging
pip install -e .

# Then use as installed package
template-python-project
```

#### Direct Execution

```bash
# Build the package
nix build

# Run the built package executable
nix run

# Or run the main script directly
nix run .#run
```

### Using Docker

```bash
# Build the image
docker build -t python-project .

# Run the container
docker run python-project
```

## Project Structure

- `main.py` - **Main script** (primary entry point, most visible)
- `src/` - **Library code** (reusable modules, functions, classes)
- `scripts/` - **Additional scripts** (if you need multiple executables)
- `pyproject.toml` - Python package configuration
- `flake.nix` - Nix development environment
- `Dockerfile` - Docker container configuration

## Development Workflow

1. **Start simple**: Put your main logic in `main.py`
2. **Extract reusable code**: Move functions/classes to `src/` as your project grows
3. **Add more scripts**: Use `scripts/` if you need multiple entry points
4. **Package when ready**: Use `pip install -e .` to test your package
