cmake_minimum_required(VERSION 3.16)
project(cpp-template VERSION 1.0.0 LANGUAGES CXX)

# Set C++ standard
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Include directories
include_directories(include)

# Create library from source files
add_library(cpp-template-lib src/hello.cpp)
target_include_directories(cpp-template-lib PUBLIC include)

# Create executable
add_executable(cpp-template src/main.cpp)
target_link_libraries(cpp-template cpp-template-lib)

# Install targets
install(TARGETS cpp-template DESTINATION bin)
install(TARGETS cpp-template-lib DESTINATION lib)
install(DIRECTORY include/ DESTINATION include)
