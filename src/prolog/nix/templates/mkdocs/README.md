# MkDocs Template

A complete MkDocs template with Material theme following the established dual build system approach.

## Features

- **Material Theme**: Modern, responsive documentation design
- **Dual Build System**: Both Nix flakes and Docker support
- **Rich Content**: Code highlighting, admonitions, tabs, and more
- **Search**: Full-text search with highlighting
- **Dark/Light Mode**: Automatic theme switching
- **Mobile Responsive**: Optimized for all devices

## Quick Start

### Using Nix (Recommended)

```bash
# Enter development environment
nix develop

# Serve locally
mkdocs serve

# Build for production
nix run .#build

# Deploy to GitHub Pages
nix run .#deploy
```

### Using Docker

```bash
# Build and run
docker build -t mkdocs-template .
docker run -p 8080:80 mkdocs-template
```

## Template Output

Both build systems produce the expected template output:

```
Hello, World!
Hello from the library!
```

## Structure

```
mkdocs/
├── flake.nix          # Nix development environment
├── Dockerfile         # Container build configuration
├── mkdocs.yml         # MkDocs configuration
├── README.md          # This file
└── docs/              # Documentation source
    ├── index.md       # Homepage
    ├── getting-started.md
    ├── library/       # API documentation
    ├── examples.md    # Usage examples
    └── about.md       # Project information
```

## Configuration

The template is pre-configured with:

- Material theme with navigation features
- Comprehensive Markdown extensions
- Search and minification plugins
- Responsive design with dark/light mode
- Code highlighting with copy buttons

## Development

1. **Write**: Create content in Markdown
2. **Preview**: Use `mkdocs serve` for live reload
3. **Build**: Generate static site with `mkdocs build`
4. **Deploy**: Publish to GitHub Pages or custom server

## Deployment Options

- **GitHub Pages**: Automated deployment with `mkdocs gh-deploy`
- **Custom Server**: Deploy the `site/` directory
- **Container**: Use Docker for containerized deployment
- **CDN**: Upload to any static hosting provider

## Customization

Easily customize:
- Theme colors and fonts
- Navigation structure
- Plugin configuration
- Markdown extensions
- Custom CSS/JavaScript

Perfect for documentation websites, project wikis, and technical guides!
