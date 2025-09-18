cmake_minimum_required(VERSION 3.20)
project(pybind11_demo)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Enable position independent code
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# Find required packages
find_package(pybind11 REQUIRED)

# Find Python
find_package(Python3 COMPONENTS Interpreter Development REQUIRED)

# Find NumPy
execute_process(
    COMMAND "${Python3_EXECUTABLE}" -c
            "import numpy as np; print(np.get_include())"
    OUTPUT_VARIABLE NUMPY_INCLUDE_DIR
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

# Find Eigen (optional, for advanced linear algebra)
find_package(Eigen3 QUIET)
if(Eigen3_FOUND)
    message(STATUS "Found Eigen3: ${EIGEN3_INCLUDE_DIR}")
    add_compile_definitions(HAVE_EIGEN)
endif()

# Find OpenBLAS or BLAS (optional, for optimized linear algebra)
find_package(BLAS QUIET)
if(BLAS_FOUND)
    message(STATUS "Found BLAS libraries: ${BLAS_LIBRARIES}")
    add_compile_definitions(HAVE_BLAS)
endif()

# Include directories
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)
include_directories(${NUMPY_INCLUDE_DIR})
if(Eigen3_FOUND)
    include_directories(${EIGEN3_INCLUDE_DIR})
endif()

# Source files
set(SOURCES
    src/functions.cpp
    src/classes.cpp
    src/numpy_demo.cpp
    src/pybind_module.cpp
)

# Create the Python module
pybind11_add_module(pybind_demo ${SOURCES})

# Compiler-specific options
target_compile_definitions(pybind_demo PRIVATE VERSION_INFO=${EXAMPLE_VERSION_INFO})

# Link libraries
if(BLAS_FOUND)
    target_link_libraries(pybind_demo PRIVATE ${BLAS_LIBRARIES})
endif()

# Optimization flags
if(CMAKE_BUILD_TYPE STREQUAL "Release")
    target_compile_options(pybind_demo PRIVATE
        $<$<CXX_COMPILER_ID:GNU>:-O3 -march=native -ffast-math>
        $<$<CXX_COMPILER_ID:Clang>:-O3 -march=native -ffast-math>
        $<$<CXX_COMPILER_ID:MSVC>:/O2>
    )
endif()

# Debug flags
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    target_compile_options(pybind_demo PRIVATE
        $<$<CXX_COMPILER_ID:GNU>:-g -O0 -Wall -Wextra -Wpedantic>
        $<$<CXX_COMPILER_ID:Clang>:-g -O0 -Wall -Wextra -Wpedantic>
        $<$<CXX_COMPILER_ID:MSVC>:/W4>
    )
endif()

# Additional warnings for better code quality
target_compile_options(pybind_demo PRIVATE
    $<$<CXX_COMPILER_ID:GNU>:-Wshadow -Wconversion -Wsign-conversion>
    $<$<CXX_COMPILER_ID:Clang>:-Wshadow -Wconversion -Wsign-conversion>
)

# Set properties for the module
set_target_properties(pybind_demo PROPERTIES
    CXX_VISIBILITY_PRESET "hidden"
    VISIBILITY_INLINES_HIDDEN YES
)

# Platform-specific settings
if(APPLE)
    set_target_properties(pybind_demo PROPERTIES
        INSTALL_RPATH "@loader_path"
    )
elseif(UNIX)
    set_target_properties(pybind_demo PROPERTIES
        INSTALL_RPATH "$ORIGIN"
    )
endif()

# Testing support (optional)
option(BUILD_TESTING "Build tests" OFF)
if(BUILD_TESTING)
    enable_testing()
    
    # Find pytest
    find_program(PYTEST_EXECUTABLE pytest REQUIRED)
    
    # Add Python tests
    add_test(
        NAME python_tests
        COMMAND ${PYTEST_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/tests -v
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    )
    
    # Set environment for tests
    set_tests_properties(python_tests PROPERTIES
        ENVIRONMENT "PYTHONPATH=${CMAKE_CURRENT_BINARY_DIR}:$ENV{PYTHONPATH}"
    )
endif()

# Installation rules
install(TARGETS pybind_demo
        DESTINATION python/pybind_demo
        COMPONENT python)

# Install Python package files
install(FILES python/pybind_demo/__init__.py
        DESTINATION python/pybind_demo
        COMPONENT python)

# Print configuration summary
message(STATUS "")
message(STATUS "Configuration Summary:")
message(STATUS "  C++ Standard: ${CMAKE_CXX_STANDARD}")
message(STATUS "  Build Type: ${CMAKE_BUILD_TYPE}")
message(STATUS "  Python: ${Python3_EXECUTABLE}")
message(STATUS "  NumPy Include: ${NUMPY_INCLUDE_DIR}")
if(Eigen3_FOUND)
    message(STATUS "  Eigen3: Found")
else()
    message(STATUS "  Eigen3: Not found (optional)")
endif()
if(BLAS_FOUND)
    message(STATUS "  BLAS: Found")
else()
    message(STATUS "  BLAS: Not found (optional)")
endif()
message(STATUS "  Testing: ${BUILD_TESTING}")
message(STATUS "")