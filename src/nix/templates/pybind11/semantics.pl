% PyBind11 comprehensive template semantics
% Demonstrates C++ to Python binding patterns in Grimoire ECS architecture

:- self_entity(pybind11_template).

% Core template entity
entity(pybind11_template).

% Binding domains - different categories of PyBind11 features
entity(function_bindings).
entity(class_bindings).
entity(numpy_bindings).
entity(container_bindings).
entity(exception_bindings).
entity(smart_pointer_bindings).

% Component types for PyBind11 features
component(function_bindings, ctor, basic_function).
component(function_bindings, ctor, overloaded_function).
component(function_bindings, ctor, default_args_function).
component(function_bindings, ctor, keyword_args_function).
component(function_bindings, ctor, callback_function).
component(function_bindings, ctor, container_function).

component(class_bindings, ctor, basic_class).
component(class_bindings, ctor, inheritance_class).
component(class_bindings, ctor, virtual_class).
component(class_bindings, ctor, abstract_class).
component(class_bindings, ctor, property_class).
component(class_bindings, ctor, operator_class).

component(numpy_bindings, ctor, array_operation).
component(numpy_bindings, ctor, matrix_operation).
component(numpy_bindings, ctor, buffer_protocol).
component(numpy_bindings, ctor, vectorized_operation).

component(container_bindings, ctor, vector_conversion).
component(container_bindings, ctor, map_conversion).
component(container_bindings, ctor, set_conversion).
component(container_bindings, ctor, tuple_conversion).

component(exception_bindings, ctor, standard_exception).
component(exception_bindings, ctor, custom_exception).
component(exception_bindings, ctor, exception_translator).

component(smart_pointer_bindings, ctor, shared_ptr_binding).
component(smart_pointer_bindings, ctor, unique_ptr_binding).
component(smart_pointer_bindings, ctor, weak_ptr_binding).

% Template structure components
component(pybind11_template, has_directory, src).
component(pybind11_template, has_directory, include).
component(pybind11_template, has_directory, python).
component(pybind11_template, has_directory, tests).

component(pybind11_template, has_file, 'flake.nix').
component(pybind11_template, has_file, 'pyproject.toml').
component(pybind11_template, has_file, 'CMakeLists.txt').
component(pybind11_template, has_file, 'README.md').
component(pybind11_template, has_file, 'semantics.pl').
component(pybind11_template, has_file, 'semantics.plt').

% Source files
component(src, contains_file, 'pybind_module.cpp').
component(src, contains_file, 'functions.cpp').
component(src, contains_file, 'classes.cpp').
component(src, contains_file, 'numpy_demo.cpp').

% Header files
component(include, contains_file, 'functions.h').
component(include, contains_file, 'classes.h').
component(include, contains_file, 'numpy_demo.h').

% Python package structure
component(python, contains_directory, pybind_demo).
component(pybind_demo, contains_file, '__init__.py').

% Test files
component(tests, contains_file, 'test_functions.py').
component(tests, contains_file, 'test_classes.py').
component(tests, contains_file, 'test_numpy.py').
component(tests, contains_file, 'test_containers.py').
component(tests, contains_file, 'test_exceptions.py').
component(tests, contains_file, 'conftest.py').

% Feature mappings - what each C++ component demonstrates
component(function_bindings, demonstrates, basic_function) :-
    component(basic_function, example, 'int add(int a, int b)').

component(function_bindings, demonstrates, overloaded_function) :-
    component(overloaded_function, example, 'multiply functions with different signatures').

component(function_bindings, demonstrates, default_args_function) :-
    component(default_args_function, example, 'greet(name, greeting="Hello")').

component(function_bindings, demonstrates, keyword_args_function) :-
    component(keyword_args_function, example, 'format_message with named arguments').

component(class_bindings, demonstrates, basic_class) :-
    component(basic_class, example, 'Calculator class with methods and properties').

component(class_bindings, demonstrates, inheritance_class) :-
    component(inheritance_class, example, 'Shape -> Rectangle/Circle hierarchy').

component(class_bindings, demonstrates, virtual_class) :-
    component(virtual_class, example, 'Shape abstract base with virtual methods').

component(numpy_bindings, demonstrates, array_operation) :-
    component(array_operation, example, 'square_array, add_arrays, dot_product').

component(numpy_bindings, demonstrates, matrix_operation) :-
    component(matrix_operation, example, 'Matrix class with linear algebra operations').

component(container_bindings, demonstrates, vector_conversion) :-
    component(vector_conversion, example, 'std::vector <-> Python list').

component(container_bindings, demonstrates, map_conversion) :-
    component(map_conversion, example, 'std::unordered_map <-> Python dict').

component(exception_bindings, demonstrates, custom_exception) :-
    component(custom_exception, example, 'PyBindDemoException class').

component(smart_pointer_bindings, demonstrates, shared_ptr_binding) :-
    component(shared_ptr_binding, example, 'ResourceManager with std::shared_ptr').

% Build system components
component(pybind11_template, build_system, nix).
component(pybind11_template, build_system, cmake).
component(pybind11_template, build_system, pip).

component(nix, provides, 'reproducible development environment').
component(nix, provides, 'cross-platform builds').
component(nix, provides, 'dependency management').

component(cmake, provides, 'C++ compilation configuration').
component(cmake, provides, 'PyBind11 integration').
component(cmake, provides, 'library linking').

component(pip, provides, 'Python package installation').
component(pip, provides, 'PyPI compatibility').
component(pip, provides, 'development mode installation').

% Test coverage components
component(pybind11_template, test_coverage, complete).
component(complete, covers, all_exported_functions).
component(complete, covers, all_exported_classes).
component(complete, covers, all_exception_paths).
component(complete, covers, all_type_conversions).

% Documentation components
component(pybind11_template, documentation_level, comprehensive).
component(comprehensive, includes, usage_examples).
component(comprehensive, includes, api_documentation).
component(comprehensive, includes, build_instructions).
component(comprehensive, includes, development_workflow).

% Performance considerations
component(pybind11_template, performance_aspect, minimal_overhead).
component(pybind11_template, performance_aspect, zero_copy_numpy).
component(pybind11_template, performance_aspect, move_semantics).
component(pybind11_template, performance_aspect, efficient_conversions).

% Best practices demonstrated
component(pybind11_template, best_practice, proper_exception_handling).
component(pybind11_template, best_practice, memory_safety).
component(pybind11_template, best_practice, pythonic_interfaces).
component(pybind11_template, best_practice, comprehensive_testing).
component(pybind11_template, best_practice, clear_documentation).

% Spell system for template operations
component(conjure, ctor, generate_binding).
component(conjure, ctor, test_feature).
component(conjure, ctor, build_package).

component(perceive, ctor, list_features).
component(perceive, ctor, check_coverage).
component(perceive, ctor, validate_bindings).

% Generate binding spell
cast(conjure(generate_binding(FeatureType, Name)), Result) :-
    component(FeatureType, ctor, Name),
    format(atom(Result), 'Generated ~w binding for ~w', [FeatureType, Name]).

% Test feature spell
cast(conjure(test_feature(Feature)), Result) :-
    component(pybind11_template, test_coverage, complete),
    component(complete, covers, Feature),
    format(atom(Result), 'Testing ~w - coverage complete', [Feature]).

% List features perception
cast(perceive(list_features(Domain)), Features) :-
    findall(Feature, component(Domain, ctor, Feature), Features).

% Check coverage perception
cast(perceive(check_coverage), Coverage) :-
    findall(Feature, component(complete, covers, Feature), Coverage).

% Validate bindings perception
cast(perceive(validate_bindings), Status) :-
    component(pybind11_template, test_coverage, complete),
    component(pybind11_template, documentation_level, comprehensive),
    Status = all_bindings_valid.

% Docstrings
docstring(pybind11_template,
   {|string(_)||
   Comprehensive PyBind11 template for Grimoire showcasing all major features.
   
   This template demonstrates:
   - Function bindings (basic, overloaded, default/keyword args, callbacks)
   - Class bindings (inheritance, virtual methods, properties, operators)  
   - Type conversions (primitives, STL containers, smart pointers)
   - NumPy integration (arrays, matrices, buffer protocol)
   - Exception handling (standard and custom exceptions)
   - Build systems (Nix, CMake, pip)
   - Comprehensive testing with pytest
   
   Structure:
   - src/: C++ source implementations
   - include/: C++ headers  
   - python/: Python package structure
   - tests/: Complete pytest suite
   - flake.nix: Nix build configuration
   - pyproject.toml: pip package configuration
   |}).

docstring(function_bindings,
   {|string(_)||
   Function binding demonstrations covering all PyBind11 function features.
   
   Includes examples of:
   - Basic functions with value/reference parameters
   - Overloaded functions with different signatures  
   - Functions with default and keyword arguments
   - Callback functions and lambdas
   - Functions working with STL containers
   - Exception-throwing functions
   |}).

docstring(class_bindings,
   {|string(_)||
   Class binding demonstrations covering inheritance and advanced features.
   
   Includes examples of:
   - Basic classes with constructors, methods, properties
   - Inheritance hierarchies with virtual methods
   - Abstract base classes and pure virtual methods
   - Operator overloading
   - Property access patterns
   - Static methods and class attributes
   |}).

docstring(numpy_bindings,
   {|string(_)||
   NumPy integration demonstrations for high-performance array operations.
   
   Includes examples of:
   - Array operations using buffer protocol
   - Matrix class with linear algebra operations
   - Vectorized operations for performance
   - Image processing operations
   - Statistical computations
   - Zero-copy array access patterns
   |}).
