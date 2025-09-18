cmake_minimum_required(VERSION 3.16)
project(prolog_cpp_binding VERSION 1.0.0 LANGUAGES CXX)

# Set C++ standard
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Find required packages
find_package(PkgConfig REQUIRED)
pkg_check_modules(SWIPL REQUIRED swipl)

# Add compiler flags for SWI-Prolog
add_compile_definitions(${SWIPL_CFLAGS_OTHER})
include_directories(${SWIPL_INCLUDE_DIRS})
link_directories(${SWIPL_LIBRARY_DIRS})

# Enable debug information in debug builds
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g -O0 -DDEBUG")
set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3 -DNDEBUG")

# Add include directory
include_directories(include)

# Library sources
set(LIBRARY_SOURCES
    src/cpp_bridge.cpp
    src/data_structures.cpp
    src/object_manager.cpp
    src/type_conversions.cpp
)

# Create shared library for Prolog foreign predicates
add_library(prolog_cpp_binding SHARED ${LIBRARY_SOURCES})

# Link with SWI-Prolog libraries
target_link_libraries(prolog_cpp_binding ${SWIPL_LIBRARIES})

# Set library properties
set_target_properties(prolog_cpp_binding PROPERTIES
    VERSION ${PROJECT_VERSION}
    SOVERSION 1
    CXX_VISIBILITY_PRESET hidden
    VISIBILITY_INLINES_HIDDEN YES
)

# Compiler-specific options
target_compile_options(prolog_cpp_binding PRIVATE
    -Wall -Wextra -Wpedantic
    -Wno-unused-parameter  # SWI-Prolog callbacks often have unused params
    $<$<CONFIG:Debug>:-fsanitize=address>
    $<$<CONFIG:Debug>:-fsanitize=undefined>
)

# Link-time options
target_link_options(prolog_cpp_binding PRIVATE
    $<$<CONFIG:Debug>:-fsanitize=address>
    $<$<CONFIG:Debug>:-fsanitize=undefined>
)

# Install targets
install(TARGETS prolog_cpp_binding
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    RUNTIME DESTINATION bin
)

install(DIRECTORY include/
    DESTINATION include
    FILES_MATCHING PATTERN "*.hpp"
)

# Copy Prolog files to build directory for testing
configure_file(auxiliary.pl ${CMAKE_BINARY_DIR}/auxiliary.pl COPYONLY)
configure_file(semantics.pl ${CMAKE_BINARY_DIR}/semantics.pl COPYONLY)
configure_file(semantics.plt ${CMAKE_BINARY_DIR}/semantics.plt COPYONLY)

# Testing support
enable_testing()

# Custom target to run Prolog tests
add_custom_target(prolog_tests
    COMMAND swipl -g "run_tests" -t "halt" semantics.plt
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    DEPENDS prolog_cpp_binding
    COMMENT "Running Prolog unit tests"
)