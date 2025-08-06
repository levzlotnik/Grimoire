# API Reference

This page documents the key configuration options and customization points for the MkDocs template.

## Configuration API

### Site Configuration

```yaml
site_name: string           # Site title
site_description: string    # Meta description
site_author: string        # Author name
site_url: string           # Canonical URL
repo_name: string          # Repository name
repo_url: string           # Repository URL
```

### Theme Configuration

```yaml
theme:
  name: material
  features:                 # List of Material theme features
    - navigation.tabs       # Enable top-level tabs
    - navigation.sections   # Enable section grouping
    - search.suggest       # Search suggestions
    - content.code.copy    # Copy code buttons
  palette:                 # Color scheme configuration
    - scheme: default      # Light mode
      primary: teal        # Primary color
      accent: purple       # Accent color
    - scheme: slate        # Dark mode
      primary: teal
      accent: lime
```

### Plugin Configuration

#### Search Plugin

```yaml
plugins:
  - search:
      separator: '[\s\-,:!=\[\]()"`/]+|\.(?!\d)|&[lg]t;|(?!\b)(?=[A-Z][a-z])'
```

#### Minify Plugin

```yaml
  - minify:
      minify_html: true
```

#### Redirects Plugin

```yaml
  - redirects:
      redirect_maps:
        'old-page.md': 'new-page.md'
```

### Markdown Extensions

#### Code Highlighting

```yaml
markdown_extensions:
  - pymdownx.highlight:
      anchor_linenums: true
      line_spans: __span
      pygments_lang_class: true
  - pymdownx.inlinehilite
  - pymdownx.superfences
```

#### Content Features

```yaml
  - pymdownx.tabbed:
      alternate_style: true
  - pymdownx.details
  - admonition
  - attr_list
  - md_in_html
```

#### Task Lists and Tables

```yaml
  - pymdownx.tasklist:
      custom_checkbox: true
  - tables
  - def_list
```

## Build API

### Nix Flake Apps

| App | Command | Description |
|-----|---------|-------------|
| `default` | `nix run` | Build site and show template output |
| `build` | `nix run .#build` | Build static site |
| `serve` | `nix run .#serve` | Start development server |
| `deploy` | `nix run .#deploy` | Deploy to GitHub Pages |

### Docker Commands

| Command | Description |
|---------|-------------|
| `docker build -t mkdocs-template .` | Build container image |
| `docker run -p 8080:80 mkdocs-template` | Run container with port mapping |

### Development Shell

```bash
nix develop
# Provides:
# - MkDocs with Material theme
# - All required Python packages
# - Git for version control
# - Node.js for additional plugins
```

## Customization API

### Adding New Pages

1. Create Markdown file in `docs/`
2. Add to navigation in `mkdocs.yml`:

```yaml
nav:
  - Home: index.md
  - New Page: new-page.md
```

### Custom CSS

Add custom styles:

```yaml
extra_css:
  - stylesheets/extra.css
```

### Custom JavaScript

Add custom scripts:

```yaml
extra_javascript:
  - javascripts/extra.js
```

### Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `MKDOCS_CONFIG_FILE` | Config file path | `mkdocs.yml` |
| `MKDOCS_STRICT` | Strict mode | `false` |

## Deployment API

### GitHub Pages

```bash
# Set up repository
git remote add origin https://github.com/user/repo.git

# Deploy
mkdocs gh-deploy
```

### Custom Deployment

```bash
# Build site
mkdocs build

# Deploy site/ directory to your server
rsync -av site/ user@server:/var/www/html/
```

## Error Handling

Common configuration errors and solutions:

!!! error "Plugin not found"
    Ensure all plugins are installed: `pip install mkdocs-material mkdocs-minify-plugin`

!!! error "Navigation error"
    Check that all files referenced in `nav` exist in the `docs/` directory.

!!! error "Theme error"
    Verify Material theme is installed: `pip install mkdocs-material`
