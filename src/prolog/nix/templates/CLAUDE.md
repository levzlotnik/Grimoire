# Multi-Language Template Testing & Development Guide

## Project Philosophy

Each language template in this directory follows a unified development philosophy:

### Dual Build System Approach
- **Nix Flakes**: Reproducible development environments with declarative dependencies
- **Docker**: Containerized builds for consistent deployment across platforms
- **Library + Executable Pattern**: Each template provides both library functionality and executable entry points

### Template Structure Standards
Each language template contains:
- **Library Code**: Core functionality that can be imported/used by other modules
- **Executable Entry Point**: Main program that demonstrates the library functionality
- **Nix Flake**: Declarative environment with development shell and app targets
- **Dockerfile**: Multi-stage build for containerized deployment
- **Configuration Files**: Language-specific build configuration (pyproject.toml, Cargo.toml, etc.)

## Systematic Testing Workflow

### Discovery Phase
```bash
cd templates/{language}
nix flake show
```
This reveals available app targets (typically `default` and `run`) and package information.

### Nix Testing Phase
```bash
# Test the primary app target
nix run .#default
```

### Docker Testing Phase
```bash
# Build the Docker image
docker build -t {language}-template .

# Run the containerized application
docker run --rm {language}-template
```

### Expected Output
All templates should produce consistent output:
```
Hello, World!
Hello from the library!
```

## Development Best Practices

### When Creating New Templates
1. **Start with Nix Flake**: Define development environment and app targets
2. **Implement Library Pattern**: Core functionality separate from executable
3. **Add Docker Support**: Multi-stage build for production deployment
4. **Test Both Paths**: Verify both Nix and Docker produce identical output
5. **Document Configuration**: Maintain language-specific build configuration

### When Debugging Issues
1. **Check Flake Structure**: Use `nix flake show` to verify available targets
2. **Test Development Shell**: Use `nix develop` to debug build issues
3. **Verify Package Configuration**: Ensure consistent naming across all files
4. **Test Import Resolution**: Verify library can be imported by executable code

### Naming Conventions
- **Package Names**: Use kebab-case for consistency (`template-rust`, `template-python`)
- **Docker Images**: Use language name followed by `-template`
- **Nix Apps**: Provide `apps.default` target that invokes the main script of the project (if it has one). If you have more scripts, add them as additional targets at `apps.someOtherScript`.

## Template Dependencies

### Finding Nix Packages
Use `nix search` to find available packages in nixpkgs:
```bash
# Search for a specific package
nix search nixpkgs python3

# Search for packages containing a keyword
nix search nixpkgs compiler

# Search for development tools
nix search nixpkgs haskell-language-server
```

### Common Nix Dependencies
- Language-specific toolchains (python3, rustc, dotnet-sdk, etc.)
- Build tools (setuptools, cargo, etc.)
- Runtime dependencies as needed

### Docker Base Images
- **Python**: `python:3.12-slim`
- **Rust**: `rust:1.70`
- **Haskell**: Typically `haskell:9` or similar
- **Lean4**: Custom or Ubuntu-based with Lean installation
- **MkDocs**: Multi-stage build with `python:3.12-slim` and `nginx:alpine`

## Troubleshooting Common Issues

### Nix Flake Problems
- **No app targets**: Check flake.nix apps section
- **Build failures**: Verify all dependencies are declared
- **Import errors**: Ensure package structure matches configuration

### Docker Build Issues
- **Build context**: Verify all necessary files are copied
- **Multi-stage builds**: Check intermediate stages complete successfully
- **Runtime errors**: Ensure runtime dependencies are available

### Package Configuration Mismatches
- **Python**: pyproject.toml packages list must match actual structure
- **Rust**: Cargo.toml name must match binary name and imports
- **MkDocs**: mkdocs.yml navigation must reference existing files in docs/

## Integration Testing

After implementing changes to any template:

1. **Run the template**:
   ```bash
  echo "Testing $lang..."
  nix run .#default
  docker build -t $lang-template . && docker run --rm $lang-template
  # Test nix shell works:
  nix develop
   ```

2. **Verify Output Consistency**: All templates should produce identical output
3. **Check Development Environments**: `nix develop` should provide working dev shell

This systematic approach ensures all language templates maintain consistency while leveraging the strengths of both Nix and Docker ecosystems.
