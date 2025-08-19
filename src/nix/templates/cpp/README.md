# C++ Template

Simple C++ project template using **CMake** with a conventional project structure.

## Philosophy
- **CMake integration** for modern C++ build management
- **Conventional structure** with `include/` headers and `src/` implementation
- **Library separation** - reusable code in library, main executable separate
- Follow C++ best practices: most common use case should be `make` in build directory

## Quick Start

### Using Nix (recommended)

#### Development Shell

```bash
# Enter development environment
nix develop

# Build and run (CMake)
mkdir -p build
cd build
cmake ..
make
./cpp-template

# Development mode with debugging
mkdir -p build
cd build
cmake -DCMAKE_BUILD_TYPE=Debug ..
make
gdb ./cpp-template
```

#### Direct Execution

```bash
# Build the package
nix build

# Run the built package executable
nix run

# Or compile and run directly
nix run .#run
```

### Using Docker

```bash
# Build the image
docker build -t cpp-project .

# Run the container
docker run cpp-project
```

## Project Structure

```
├── CMakeLists.txt      - **Build configuration** (CMake setup, dependencies)
├── src/
│   ├── main.cpp        - **Main executable** (primary entry point, most common use case)
│   └── hello.cpp       - **Library implementation** (reusable functions)
├── include/
│   └── hello.h         - **Library headers** (public interface)
├── flake.nix          - **Nix development environment**
├── Dockerfile         - **Container setup**
└── README.md          - **This file**
```

## Development Workflow

1. **Start simple**: Add your main logic to `src/main.cpp`
2. **Extract reusable code**: Create functions in `src/` with headers in `include/`
3. **Build with CMake**: Use conventional `mkdir build && cd build && cmake .. && make`
4. **Test and debug**: Use `gdb` for debugging, `valgrind` for memory checking

## Development Workflow

1. **Start simple**: Put your main logic in `main.cpp`
2. **Extract reusable code**: Create `.h/.cpp` pairs for classes and functions
3. **Organize headers**: Use `include/` directory for public interfaces
4. **Build system**: Consider CMake when project becomes complex
