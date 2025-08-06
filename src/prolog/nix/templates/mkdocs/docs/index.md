# MkDocs Template

Welcome to the MkDocs template with Material theme! This template provides a complete documentation website setup following the established template patterns.

## Overview

This template demonstrates the dual build system approach:

- **Nix Flakes**: Reproducible development environment with MkDocs and Material theme
- **Docker**: Containerized build for consistent deployment across platforms

## Quick Start

### Using Nix

```bash
# Build the site
nix run .#build

# Serve locally (opens http://127.0.0.1:8000)
nix run .#serve

# Deploy to GitHub Pages
nix run .#deploy
```

### Using Docker

```bash
# Build and run
docker build -t mkdocs-template .
docker run -p 8080:80 mkdocs-template
```

Visit [http://localhost:8080](http://localhost:8080) to view the site.

## Features

This template includes:

- **Material Theme**: Modern, responsive design
- **Search**: Full-text search functionality
- **Code Highlighting**: Syntax highlighting with copy buttons
- **Navigation**: Organized with tabs and sections
- **Dark/Light Mode**: Toggle between themes
- **Responsive Design**: Works on all devices

## Template Output

Both Nix and Docker builds produce:

```
Hello, World!
Hello from the library!
```

This maintains consistency with other language templates in the project.

## Library Pattern

The documentation serves as the "library" component, providing comprehensive information about:

- Installation and setup
- API documentation
- Examples and tutorials
- Best practices

The build process serves as the "executable" component, generating a deployable website.

## Next Steps

1. Customize `mkdocs.yml` for your project
2. Add your documentation in the `docs/` directory
3. Update the navigation structure
4. Configure deployment settings

Happy documenting! 📚
