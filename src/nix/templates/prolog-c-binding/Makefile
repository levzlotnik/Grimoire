# Makefile for Prolog-C binding template
# Uses swipl-ld for proper compilation and linking

# Compiler and flags
CC = gcc
CFLAGS = -fPIC -O2 -Wall -Wextra
SWIPL_LD = swipl-ld

# Directories
SRC_DIR = src
BUILD_DIR = build

# Source files
C_SOURCES = $(SRC_DIR)/prolog_c_binding.c
SHARED_LIB = $(BUILD_DIR)/prolog_c_binding.so

# Default target
all: $(SHARED_LIB)

# Create build directory
$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

# Build shared library using swipl-ld
$(SHARED_LIB): $(C_SOURCES) | $(BUILD_DIR)
	$(SWIPL_LD) -shared -o $@ $(C_SOURCES)

# Test the library by loading it in SWI-Prolog
test: $(SHARED_LIB)
	@echo "Testing C foreign library..."
	swipl -g "use_foreign_library('$(SHARED_LIB)'), add_numbers(5, 3, R), writeln(R), halt" -t "halt(1)"

# Test with semantics
test-semantics: $(SHARED_LIB)
	@echo "Testing with semantics..."
	swipl -g "consult('semantics.pl'), halt" -t "halt(1)"

# Run all PLUnit tests
test-full: $(SHARED_LIB)
	@echo "Running full test suite..."
	swipl -g "run_tests" -t "halt" semantics.plt

# Install the library to a system location (optional)
install: $(SHARED_LIB)
	@echo "Installing shared library..."
	install -d /usr/local/lib/swipl/lib/$(shell swipl --dump-runtime-variables | grep PLARCH | cut -d= -f2)
	install $(SHARED_LIB) /usr/local/lib/swipl/lib/$(shell swipl --dump-runtime-variables | grep PLARCH | cut -d= -f2)/

# Clean build artifacts
clean:
	rm -rf $(BUILD_DIR)

# Debug build with symbols
debug: CFLAGS += -g -DDEBUG
debug: $(SHARED_LIB)

# Check C code with static analysis
check:
	@echo "Running static analysis..."
	cppcheck --enable=all --suppress=missingIncludeSystem $(C_SOURCES)

# Format C code
format:
	@echo "Formatting C source..."
	clang-format -i $(C_SOURCES)

# Show help
help:
	@echo "Available targets:"
	@echo "  all         - Build the shared library (default)"
	@echo "  test        - Quick test of the C library"
	@echo "  test-semantics - Test loading with semantics.pl"
	@echo "  test-full   - Run complete PLUnit test suite"
	@echo "  install     - Install library to system location"
	@echo "  clean       - Remove build artifacts"
	@echo "  debug       - Build with debug symbols"
	@echo "  check       - Run static analysis"
	@echo "  format      - Format C source code"
	@echo "  help        - Show this help message"

# Phony targets
.PHONY: all test test-semantics test-full install clean debug check format help