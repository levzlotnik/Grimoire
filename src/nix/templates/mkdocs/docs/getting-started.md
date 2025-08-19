# Getting Started

This guide will help you get up and running with the MkDocs template.

## Prerequisites

Choose one of the following approaches:

### Option 1: Nix (Recommended)

- Install [Nix](https://nixos.org/download.html) with flakes enabled
- All dependencies will be managed automatically

### Option 2: Docker

- Install [Docker](https://docs.docker.com/get-docker/)
- No other dependencies needed

### Option 3: Manual Installation

If you prefer to install dependencies manually:

```bash
pip install mkdocs mkdocs-material mkdocs-material-extensions pymdown-extensions mkdocs-minify-plugin mkdocs-redirects
```

## Development Workflow

### 1. Development Environment

=== "Nix"

    ```bash
    # Enter development shell
    nix develop

    # Start local server
    mkdocs serve
    ```

=== "Docker"

    ```bash
    # Build and run development container
    docker build -t mkdocs-template .
    docker run -p 8080:80 mkdocs-template
    ```

=== "Manual"

    ```bash
    # Start local server
    mkdocs serve
    ```

### 2. Writing Documentation

- Add new pages in the `docs/` directory
- Use Markdown with Material theme extensions
- Update navigation in `mkdocs.yml`

### 3. Building for Production

=== "Nix"

    ```bash
    nix run .#build
    ```

=== "Docker"

    ```bash
    docker build -t mkdocs-template .
    ```

=== "Manual"

    ```bash
    mkdocs build
    ```

## Configuration

The `mkdocs.yml` file contains all configuration options:

- **Site information**: Name, description, URL
- **Theme settings**: Colors, features, navigation
- **Plugins**: Search, minify, redirects
- **Markdown extensions**: Syntax highlighting, admonitions, etc.

## Deployment

### GitHub Pages

=== "Nix"

    ```bash
    nix run .#deploy
    ```

=== "Manual"

    ```bash
    mkdocs gh-deploy
    ```

### Custom Server

Deploy the built `site/` directory to any web server:

```bash
mkdocs build
# Upload site/ directory to your server
```

## Tips

!!! tip "Live Reload"
    The `mkdocs serve` command provides live reload - changes are automatically reflected in your browser.

!!! note "Navigation"
    Update the `nav` section in `mkdocs.yml` to organize your documentation structure.

!!! warning "GitHub Pages"
    Ensure your repository has GitHub Pages enabled in the settings before deploying.
