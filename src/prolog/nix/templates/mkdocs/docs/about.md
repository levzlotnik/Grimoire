# About

## Project Information

This MkDocs template with Material theme provides a complete documentation solution following the established template patterns in this repository.

### Template Philosophy

This template adheres to the dual build system approach:

- **Nix Flakes**: Reproducible development environments with declarative dependencies
- **Docker**: Containerized builds for consistent deployment across platforms
- **Library + Executable Pattern**: Documentation serves as the library, build process as the executable

### Features

- **Material Design**: Modern, responsive documentation theme
- **Search Functionality**: Full-text search with highlighting
- **Code Highlighting**: Syntax highlighting with copy buttons
- **Dark/Light Mode**: Automatic theme switching
- **Mobile Responsive**: Optimized for all device sizes
- **Rich Content**: Support for admonitions, tabs, diagrams, and more

## Technical Details

### Dependencies

The template includes carefully selected dependencies:

- **MkDocs**: Static site generator for documentation
- **Material Theme**: Modern Material Design theme
- **Extensions**: Comprehensive set of Markdown extensions
- **Plugins**: Optimization and enhancement plugins

### Build Targets

| Target | Description | Command |
|--------|-------------|---------|
| `default` | Build site with template output | `nix run` |
| `build` | Build static site | `nix run .#build` |
| `serve` | Development server | `nix run .#serve` |
| `deploy` | GitHub Pages deployment | `nix run .#deploy` |

### Container Support

Multi-stage Docker build:
1. **Build Stage**: Install dependencies and build site
2. **Runtime Stage**: Serve with nginx for production deployment

## Consistency

This template maintains consistency with other language templates by:

- Following the same Nix flake structure
- Providing Docker containerization
- Producing standardized template output
- Using consistent naming conventions

### Expected Output

```
Hello, World!
Hello from the library!
```

This output ensures compatibility with the established testing workflow.

## Contributing

When modifying this template:

1. **Test Both Systems**: Verify Nix and Docker builds work correctly
2. **Maintain Output**: Ensure template output remains consistent
3. **Update Documentation**: Keep all documentation current
4. **Follow Patterns**: Adhere to established naming and structure conventions

## Support

For issues or questions:

- Check the troubleshooting section in the main CLAUDE.md guide
- Verify your environment meets the prerequisites
- Test with both Nix and Docker to isolate issues

## License

This template follows the same license as the parent project.

---

*Last updated: July 2025*
