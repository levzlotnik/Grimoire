# PyBind11 Comprehensive Template

A production-ready PyBind11 template for Grimoire that demonstrates all major features and best practices for creating high-performance Python bindings for C++ code.

## Features Demonstrated

This template showcases **ALL** major PyBind11 features:

### Function Bindings
- ✅ Basic functions with value, reference, and pointer parameters
- ✅ Overloaded functions with automatic signature resolution
- ✅ Default and keyword arguments
- ✅ Callback functions and lambda support
- ✅ Complex return types (tuples, pairs)

### Class Bindings
- ✅ Constructors, destructors, and method bindings
- ✅ Property access (getters/setters)
- ✅ Operator overloading (+, -, *, ==, !=, [], etc.)
- ✅ Static methods and class attributes
- ✅ Inheritance hierarchies with virtual methods
- ✅ Abstract base classes and pure virtual methods

### Type Conversions
- ✅ Automatic type conversion for primitives
- ✅ STL containers: `std::vector` ↔ `list`, `std::unordered_map` ↔ `dict`
- ✅ Smart pointers: `std::shared_ptr`, `std::unique_ptr`
- ✅ Custom type casters and converters

### NumPy Integration
- ✅ Array operations using buffer protocol
- ✅ Matrix class with linear algebra operations
- ✅ Zero-copy array access for performance
- ✅ Vectorized operations and broadcasting
- ✅ Image processing demonstrations

### Exception Handling
- ✅ C++ → Python exception translation
- ✅ Custom exception classes
- ✅ Exception safety guarantees
- ✅ Proper error propagation and recovery

### Performance Features
- ✅ Minimal overhead bindings
- ✅ Move semantics support
- ✅ Efficient memory management
- ✅ RAII patterns and resource safety

## Project Structure

```
src/nix/templates/pybind11/
├── src/                          # C++ source files
│   ├── pybind_module.cpp        # Main module definition
│   ├── functions.cpp            # Function binding examples
│   ├── classes.cpp              # Class binding examples
│   └── numpy_demo.cpp           # NumPy integration examples
├── include/                      # C++ headers
│   ├── functions.h              # Function declarations
│   ├── classes.h                # Class declarations
│   └── numpy_demo.h             # NumPy integration headers
├── python/                       # Python package structure
│   └── pybind_demo/
│       └── __init__.py          # Package initialization
├── tests/                        # Comprehensive test suite
│   ├── conftest.py              # Test configuration and fixtures
│   ├── test_functions.py        # Function binding tests
│   ├── test_classes.py          # Class binding tests
│   ├── test_numpy.py            # NumPy integration tests
│   ├── test_containers.py       # Container conversion tests
│   └── test_exceptions.py       # Exception handling tests
├── flake.nix                    # Nix build configuration
├── pybind11-demo.nix            # Nix package definition
├── pyproject.toml               # pip package configuration
├── CMakeLists.txt               # CMake build configuration
├── semantics.pl                 # Grimoire ECS model
├── semantics.plt                # Prolog unit tests
└── README.md                    # This file
```

## Quick Start

### Using Nix (Recommended)

1. **Enter development environment:**
   ```bash
   cd src/nix/templates/pybind11
   nix develop
   ```

2. **Build the package:**
   ```bash
   nix build
   ```

3. **Run tests:**
   ```bash
   nix run .#test-python
   ```

4. **Try the interactive demo:**
   ```bash
   nix run .#demo
   ```

### Using pip

1. **Install in development mode:**
   ```bash
   pip install -e .
   ```

2. **Run tests:**
   ```bash
   python -m pytest tests/ -v
   ```

## Usage Examples

### Basic Function Calls

```python
import pybind_demo

# Basic arithmetic
result = pybind_demo.functions.add(5, 3)  # 8

# Function with default arguments
greeting = pybind_demo.functions.greet("World")  # "Hello World"
custom_greeting = pybind_demo.functions.greet("Alice", "Hi")  # "Hi Alice"

# Overloaded functions
int_result = pybind_demo.functions.multiply(3, 4)  # 12
float_result = pybind_demo.functions.multiply(2.5, 3.0)  # 7.5
vector_result = pybind_demo.functions.multiply([1.0, 2.0, 3.0], 2.0)  # [2.0, 4.0, 6.0]
```

### Class Usage

```python
# Calculator with method chaining
calc = pybind_demo.Calculator(10.0, "my_calc")
calc.add(5.0).multiply(2.0).subtract(10.0)  # calc.value = 20.0

# Operator overloading
calc1 = pybind_demo.Calculator(10.0)
calc2 = pybind_demo.Calculator(5.0)
calc3 = calc1 + calc2  # calc3.value = 15.0

# Property access
calc.value = 42.0
calc.name = "updated_calc"
```

### Inheritance and Polymorphism

```python
# Create shapes using factory methods
rect = pybind_demo.Shape.create_rectangle(5.0, 3.0, "my_rect")
circle = pybind_demo.Shape.create_circle(2.0, "my_circle")

# Polymorphic method calls
print(f"Rectangle area: {rect.area()}")  # 15.0
print(f"Circle area: {circle.area()}")   # ~12.57

# Direct instantiation
rect2 = pybind_demo.Rectangle(4.0, 6.0)
rect2.width = 8.0  # Property modification
```

### Container Operations

```python
# STL vector ↔ Python list
data = [1, 2, 3, 4, 5]
processed = pybind_demo.functions.process_list(data)  # [2, 5, 10, 17, 26]

# STL map ↔ Python dict
input_dict = {"hello": 5, "world": 10}
result_dict = pybind_demo.functions.process_dict(input_dict)  # {"HELLO": 10, "WORLD": 20}

# Smart pointer usage
vec = pybind_demo.functions.create_shared_vector(5, 2.0)
pybind_demo.functions.modify_shared_vector(vec, 3.0)  # Modifies in place
```

### NumPy Integration

```python
import numpy as np
import pybind_demo.numpy_demo as numpy_demo

# Array operations
arr = np.array([1.0, 2.0, 3.0, 4.0])
squared = numpy_demo.square_array(arr)  # [1.0, 4.0, 9.0, 16.0]

# Matrix operations
matrix = numpy_demo.Matrix(3, 3, 1.0)  # 3x3 matrix filled with 1.0
matrix(0, 0) = 10.0  # Element access
transposed = numpy_demo.matrix_transpose(matrix)

# Statistical operations
data = np.array([1.0, 2.0, 3.0, 4.0, 5.0])
mean_val = numpy_demo.mean(data)
normalized = numpy_demo.normalize(data)
```

### Exception Handling

```python
try:
    result = pybind_demo.functions.divide(10, 0)
except pybind_demo.PyBindDemoException as e:
    print(f"Error: {e}")  # "Error: Division by zero"

# Exception safety - object state preserved
calc = pybind_demo.Calculator(100.0)
try:
    calc.divide(0.0)
except pybind_demo.PyBindDemoException:
    pass
# calc.value is still 100.0, object is still usable
calc.add(50.0)  # Works fine
```

## Development Workflow

### Building with CMake

```bash
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make -j$(nproc)
```

### Running Tests

```bash
# Python tests with coverage
python -m pytest tests/ -v --cov=pybind_demo --cov-report=html

# Specific test categories
python -m pytest tests/test_functions.py -v
python -m pytest tests/ -k "numpy" -v
python -m pytest tests/ -m "not slow" -v

# Grimoire/Prolog tests
./grimoire test
```

### Code Quality

```bash
# Format code
black tests/ python/
isort tests/ python/

# Type checking
mypy python/pybind_demo/

# Linting
ruff check tests/ python/
```

## Performance Considerations

This template demonstrates several performance optimization techniques:

1. **Zero-copy NumPy operations**: Direct buffer access without copying
2. **Move semantics**: Efficient transfer of large objects
3. **Smart pointer integration**: Proper memory management
4. **Minimal overhead bindings**: Thin wrapper layer
5. **Vectorized operations**: Batch processing for better performance

### Benchmarking

```python
import numpy as np
import time
import pybind_demo.numpy_demo as numpy_demo

# Benchmark array operations
large_array = np.random.rand(1000000)

start = time.time()
result = numpy_demo.square_array(large_array)
end = time.time()

print(f"Processed 1M elements in {end - start:.3f} seconds")
```

## Build System Support

### Nix Flake

- ✅ Reproducible builds across platforms
- ✅ Automatic dependency management
- ✅ Development environment setup
- ✅ Cross-platform support (Linux, macOS)

### CMake

- ✅ Modern CMake (3.20+) configuration
- ✅ Automatic PyBind11 detection
- ✅ NumPy integration
- ✅ Optimization flags for Release builds
- ✅ Debug symbols for Debug builds

### pip/PyPI

- ✅ Standard Python package structure
- ✅ Development dependencies specification
- ✅ Testing and quality tools integration
- ✅ Wheel building support

## Testing Philosophy

Every C++ export is thoroughly tested with multiple test categories:

- **Unit tests**: Individual function/class testing
- **Integration tests**: Cross-module functionality
- **Performance tests**: Benchmarking and optimization
- **Error handling tests**: Exception scenarios
- **Edge case tests**: Boundary conditions and corner cases

Test coverage aims for >95% with comprehensive validation of:
- All function overloads and argument combinations
- All class methods, properties, and operators
- All container conversions and type safety
- All exception paths and error recovery
- All NumPy integration features

## Grimoire Integration

### ECS Model

The template includes a comprehensive ECS (Entity-Component-System) model in `semantics.pl`:

```prolog
% Query available features
?- cast(perceive(list_features(function_bindings)), Features).

% Test specific components
?- cast(conjure(test_feature(all_exported_functions)), Result).

% Validate bindings
?- cast(perceive(validate_bindings), Status).
```

### Spell System

- `conjure(generate_binding(Type, Name))` - Generate new bindings
- `conjure(test_feature(Feature))` - Test specific features
- `perceive(list_features(Domain))` - List available features
- `perceive(check_coverage)` - Verify test coverage

## Contributing

When extending this template:

1. **Add C++ implementation** in `src/` and `include/`
2. **Export in module** via `src/pybind_module.cpp`
3. **Add comprehensive tests** in `tests/`
4. **Update ECS model** in `semantics.pl`
5. **Document usage** in this README

## License

MIT License - see template for full details.

## References

- [PyBind11 Documentation](https://pybind11.readthedocs.io/)
- [NumPy C API](https://numpy.org/doc/stable/reference/c-api/)
- [CMake PyBind11 Integration](https://pybind11.readthedocs.io/en/stable/compiling.html)
- [Grimoire Documentation](https://github.com/example/grimoire)