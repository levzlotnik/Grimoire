% PLUnit tests for PyBind11 template semantics
% Tests the ECS model and spell system for the comprehensive PyBind11 template

:- use_module(library(plunit)).
:- use_module('semantics.pl').

:- begin_tests(pybind11_template_entity).

test(template_entity_exists) :-
    entity(pybind11_template).

test(template_self_entity) :-
    % Test that self_entity declaration works
    current_predicate(self_entity/1),
    clause(self_entity(pybind11_template), true).

test(binding_domains_exist) :-
    entity(function_bindings),
    entity(class_bindings),
    entity(numpy_bindings),
    entity(container_bindings),
    entity(exception_bindings),
    entity(smart_pointer_bindings).

:- end_tests(pybind11_template_entity).

:- begin_tests(component_types).

test(function_binding_components) :-
    component(function_bindings, ctor, basic_function),
    component(function_bindings, ctor, overloaded_function),
    component(function_bindings, ctor, default_args_function),
    component(function_bindings, ctor, keyword_args_function),
    component(function_bindings, ctor, callback_function),
    component(function_bindings, ctor, container_function).

test(class_binding_components) :-
    component(class_bindings, ctor, basic_class),
    component(class_bindings, ctor, inheritance_class),
    component(class_bindings, ctor, virtual_class),
    component(class_bindings, ctor, abstract_class),
    component(class_bindings, ctor, property_class),
    component(class_bindings, ctor, operator_class).

test(numpy_binding_components) :-
    component(numpy_bindings, ctor, array_operation),
    component(numpy_bindings, ctor, matrix_operation),
    component(numpy_bindings, ctor, buffer_protocol),
    component(numpy_bindings, ctor, vectorized_operation).

test(container_binding_components) :-
    component(container_bindings, ctor, vector_conversion),
    component(container_bindings, ctor, map_conversion),
    component(container_bindings, ctor, set_conversion),
    component(container_bindings, ctor, tuple_conversion).

test(exception_binding_components) :-
    component(exception_bindings, ctor, standard_exception),
    component(exception_bindings, ctor, custom_exception),
    component(exception_bindings, ctor, exception_translator).

test(smart_pointer_binding_components) :-
    component(smart_pointer_bindings, ctor, shared_ptr_binding),
    component(smart_pointer_bindings, ctor, unique_ptr_binding),
    component(smart_pointer_bindings, ctor, weak_ptr_binding).

:- end_tests(component_types).

:- begin_tests(template_structure).

test(template_directories) :-
    component(pybind11_template, has_directory, src),
    component(pybind11_template, has_directory, include),
    component(pybind11_template, has_directory, python),
    component(pybind11_template, has_directory, tests).

test(template_files) :-
    component(pybind11_template, has_file, 'flake.nix'),
    component(pybind11_template, has_file, 'pyproject.toml'),
    component(pybind11_template, has_file, 'CMakeLists.txt'),
    component(pybind11_template, has_file, 'README.md'),
    component(pybind11_template, has_file, 'semantics.pl'),
    component(pybind11_template, has_file, 'semantics.plt').

test(source_files) :-
    component(src, contains_file, 'pybind_module.cpp'),
    component(src, contains_file, 'functions.cpp'),
    component(src, contains_file, 'classes.cpp'),
    component(src, contains_file, 'numpy_demo.cpp').

test(header_files) :-
    component(include, contains_file, 'functions.h'),
    component(include, contains_file, 'classes.h'),
    component(include, contains_file, 'numpy_demo.h').

test(python_structure) :-
    component(python, contains_directory, pybind_demo),
    component(pybind_demo, contains_file, '__init__.py').

test(test_files) :-
    component(tests, contains_file, 'test_functions.py'),
    component(tests, contains_file, 'test_classes.py'),
    component(tests, contains_file, 'test_numpy.py'),
    component(tests, contains_file, 'test_containers.py'),
    component(tests, contains_file, 'test_exceptions.py'),
    component(tests, contains_file, 'conftest.py').

:- end_tests(template_structure).

:- begin_tests(feature_demonstrations).

test(function_examples) :-
    component(function_bindings, demonstrates, basic_function),
    component(basic_function, example, 'int add(int a, int b)'),
    
    component(function_bindings, demonstrates, overloaded_function),
    component(overloaded_function, example, 'multiply functions with different signatures'),
    
    component(function_bindings, demonstrates, default_args_function),
    component(default_args_function, example, 'greet(name, greeting="Hello")'),
    
    component(function_bindings, demonstrates, keyword_args_function),
    component(keyword_args_function, example, 'format_message with named arguments').

test(class_examples) :-
    component(class_bindings, demonstrates, basic_class),
    component(basic_class, example, 'Calculator class with methods and properties'),
    
    component(class_bindings, demonstrates, inheritance_class),
    component(inheritance_class, example, 'Shape -> Rectangle/Circle hierarchy'),
    
    component(class_bindings, demonstrates, virtual_class),
    component(virtual_class, example, 'Shape abstract base with virtual methods').

test(numpy_examples) :-
    component(numpy_bindings, demonstrates, array_operation),
    component(array_operation, example, 'square_array, add_arrays, dot_product'),
    
    component(numpy_bindings, demonstrates, matrix_operation),
    component(matrix_operation, example, 'Matrix class with linear algebra operations').

test(container_examples) :-
    component(container_bindings, demonstrates, vector_conversion),
    component(vector_conversion, example, 'std::vector <-> Python list'),
    
    component(container_bindings, demonstrates, map_conversion),
    component(map_conversion, example, 'std::unordered_map <-> Python dict').

test(exception_examples) :-
    component(exception_bindings, demonstrates, custom_exception),
    component(custom_exception, example, 'PyBindDemoException class').

test(smart_pointer_examples) :-
    component(smart_pointer_bindings, demonstrates, shared_ptr_binding),
    component(shared_ptr_binding, example, 'ResourceManager with std::shared_ptr').

:- end_tests(feature_demonstrations).

:- begin_tests(build_systems).

test(build_system_components) :-
    component(pybind11_template, build_system, nix),
    component(pybind11_template, build_system, cmake),
    component(pybind11_template, build_system, pip).

test(nix_capabilities) :-
    component(nix, provides, 'reproducible development environment'),
    component(nix, provides, 'cross-platform builds'),
    component(nix, provides, 'dependency management').

test(cmake_capabilities) :-
    component(cmake, provides, 'C++ compilation configuration'),
    component(cmake, provides, 'PyBind11 integration'),
    component(cmake, provides, 'library linking').

test(pip_capabilities) :-
    component(pip, provides, 'Python package installation'),
    component(pip, provides, 'PyPI compatibility'),
    component(pip, provides, 'development mode installation').

:- end_tests(build_systems).

:- begin_tests(test_coverage).

test(coverage_completeness) :-
    component(pybind11_template, test_coverage, complete),
    component(complete, covers, all_exported_functions),
    component(complete, covers, all_exported_classes),
    component(complete, covers, all_exception_paths),
    component(complete, covers, all_type_conversions).

:- end_tests(test_coverage).

:- begin_tests(documentation).

test(documentation_level) :-
    component(pybind11_template, documentation_level, comprehensive),
    component(comprehensive, includes, usage_examples),
    component(comprehensive, includes, api_documentation),
    component(comprehensive, includes, build_instructions),
    component(comprehensive, includes, development_workflow).

:- end_tests(documentation).

:- begin_tests(performance_aspects).

test(performance_considerations) :-
    component(pybind11_template, performance_aspect, minimal_overhead),
    component(pybind11_template, performance_aspect, zero_copy_numpy),
    component(pybind11_template, performance_aspect, move_semantics),
    component(pybind11_template, performance_aspect, efficient_conversions).

:- end_tests(performance_aspects).

:- begin_tests(best_practices).

test(best_practices_demonstrated) :-
    component(pybind11_template, best_practice, proper_exception_handling),
    component(pybind11_template, best_practice, memory_safety),
    component(pybind11_template, best_practice, pythonic_interfaces),
    component(pybind11_template, best_practice, comprehensive_testing),
    component(pybind11_template, best_practice, clear_documentation).

:- end_tests(best_practices).

:- begin_tests(spell_system).

test(conjure_spells) :-
    component(conjure, ctor, generate_binding),
    component(conjure, ctor, test_feature),
    component(conjure, ctor, build_package).

test(perceive_spells) :-
    component(perceive, ctor, list_features),
    component(perceive, ctor, check_coverage),
    component(perceive, ctor, validate_bindings).

test(generate_binding_spell) :-
    cast(conjure(generate_binding(function_bindings, basic_function)), Result),
    atom_string(Result, ResultStr),
    sub_string(ResultStr, _, _, _, 'Generated function_bindings binding for basic_function').

test(test_feature_spell) :-
    cast(conjure(test_feature(all_exported_functions)), Result),
    atom_string(Result, ResultStr),
    sub_string(ResultStr, _, _, _, 'Testing all_exported_functions - coverage complete').

test(list_features_perception) :-
    cast(perceive(list_features(function_bindings)), Features),
    member(basic_function, Features),
    member(overloaded_function, Features),
    member(default_args_function, Features),
    member(keyword_args_function, Features),
    member(callback_function, Features),
    member(container_function, Features).

test(check_coverage_perception) :-
    cast(perceive(check_coverage), Coverage),
    member(all_exported_functions, Coverage),
    member(all_exported_classes, Coverage),
    member(all_exception_paths, Coverage),
    member(all_type_conversions, Coverage).

test(validate_bindings_perception) :-
    cast(perceive(validate_bindings), Status),
    Status = all_bindings_valid.

:- end_tests(spell_system).

:- begin_tests(docstrings).

test(main_docstring_exists) :-
    docstring(pybind11_template, DocString),
    atom_string(DocString, DocStr),
    sub_string(DocStr, _, _, _, 'Comprehensive PyBind11 template').

test(function_bindings_docstring) :-
    docstring(function_bindings, DocString),
    atom_string(DocString, DocStr),
    sub_string(DocStr, _, _, _, 'Function binding demonstrations').

test(class_bindings_docstring) :-
    docstring(class_bindings, DocString),
    atom_string(DocString, DocStr),
    sub_string(DocStr, _, _, _, 'Class binding demonstrations').

test(numpy_bindings_docstring) :-
    docstring(numpy_bindings, DocString),
    atom_string(DocString, DocStr),
    sub_string(DocStr, _, _, _, 'NumPy integration demonstrations').

:- end_tests(docstrings).
