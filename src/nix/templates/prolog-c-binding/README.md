# Prolog-C Binding Template

This template demonstrates comprehensive integration between SWI-Prolog and C using the foreign function interface. It shows how to create C predicates that integrate seamlessly with Prolog's logical programming paradigm and Grimoire's ECS architecture.

## Overview

The template provides examples of:

- **Arithmetic Operations**: Integer addition and floating-point multiplication
- **String/Atom Manipulation**: Length calculation and concatenation  
- **List Processing**: Summation and reversal operations
- **Compound Term Operations**: Construction and decomposition
- **Deterministic Predicates**: Single-solution predicates like evenness testing
- **Non-deterministic Predicates**: Multiple-solution predicates with backtracking
- **Unification Examples**: Demonstrating Prolog's unification in C
- **Memory Management**: Proper handling of Prolog terms and C memory
- **Error Handling**: Robust error checking and reporting

## Architecture

```
src/
├── prolog_c_binding.c    # C foreign predicates implementation
auxiliary.pl              # Prolog module importing C predicates  
semantics.pl              # ECS integration using imported predicates
semantics.plt             # Comprehensive PLUnit tests
flake.nix                 # Nix build configuration
Makefile                  # Native build system
README.md                 # This documentation
```

## File Structure

### C Implementation (`src/prolog_c_binding.c`)

The C file demonstrates the full range of SWI-Prolog's foreign interface:

```c
// Arithmetic operations
static foreign_t pl_add_numbers(term_t a, term_t b, term_t result);
static foreign_t pl_multiply_floats(term_t a, term_t b, term_t result);

// String/atom manipulation  
static foreign_t pl_string_length(term_t string, term_t length);
static foreign_t pl_atom_concat(term_t atom1, term_t atom2, term_t result);

// List operations
static foreign_t pl_list_sum(term_t list, term_t sum);
static foreign_t pl_list_reverse(term_t input, term_t output);

// Compound term operations
static foreign_t pl_make_compound(term_t functor_name, term_t args, term_t result);
static foreign_t pl_decompose_compound(term_t compound, term_t functor_name, term_t arity, term_t args);

// Deterministic predicates
static foreign_t pl_is_even(term_t number);
static foreign_t pl_factorial(term_t n, term_t result);

// Non-deterministic predicates (with choice points)
static foreign_t pl_generate_range(term_t start, term_t end, term_t current, int control);
static foreign_t pl_find_divisors(term_t number, term_t divisor, int control);

// Unification examples
static foreign_t pl_unify_structure(term_t input, term_t pattern);
```

### Auxiliary Module (`auxiliary.pl`)

Clean Prolog interface that imports all C predicates:

```prolog
:- module(auxiliary, [
    add_numbers/3,
    multiply_floats/3,
    string_length/2,
    atom_concat/3,
    list_sum/2,
    list_reverse/2,
    make_compound/3,
    decompose_compound/4,
    is_even/1,
    factorial/2,
    generate_range/3,
    find_divisors/2,
    unify_structure/2
]).

:- use_foreign_library(foreign(prolog_c_binding)).
```

### ECS Integration (`semantics.pl`)

Shows how to integrate C predicates into Grimoire's Entity-Component-System:

```prolog
:- self_entity(prolog_c_domain).

% Import C predicates
:- use_module('auxiliary.pl', [...]).

% Example entities using C predicates
entity(calculator).
entity(text_processor).
entity(list_processor).
entity(term_manipulator).
entity(number_analyzer).

% Components using C predicates
component(calculator, compute, Result) :-
    component(calculator, operands, [A, B]),
    add_numbers(A, B, Result).

% Conjure spells for C operations
cast(conjure(arithmetic_operation(add, A, B)), Result) :-
    add_numbers(A, B, Result).
```

## Building and Testing

### Using Nix (Recommended)

```bash
# Enter development environment
nix develop

# Build the library
nix build

# Run basic tests
nix run .#test

# Run full test suite
nix run .#test-full

# Interactive REPL with library loaded
nix run .#repl
```

### Using Make

```bash
# Build the shared library
make

# Quick test of C predicates
make test

# Test with semantics loading
make test-semantics

# Run complete PLUnit test suite
make test-full

# Build with debug symbols
make debug

# Static analysis
make check

# Format code
make format
```

### Using Grimoire

```bash
# Load semantics in Grimoire
grimoire exec semantics.pl

# Run tests through Grimoire
grimoire test -- semantics.plt
```

## Usage Examples

### Basic Arithmetic

```prolog
?- add_numbers(5, 3, Result).
Result = 8.

?- multiply_floats(2.5, 4.0, Result).
Result = 10.0.
```

### String Operations

```prolog
?- string_length(hello, Length).
Length = 5.

?- atom_concat(hello, world, Result).
Result = helloworld.
```

### List Processing

```prolog
?- list_sum([1, 2, 3, 4, 5], Sum).
Sum = 15.

?- list_reverse([a, b, c], Reversed).
Reversed = [c, b, a].
```

### Compound Terms

```prolog
?- make_compound(person, [john, 25], Term).
Term = person(john, 25).

?- decompose_compound(person(john, 25), Name, Arity, Args).
Name = person,
Arity = 2,
Args = [john, 25].
```

### Non-deterministic Predicates

```prolog
?- generate_range(1, 5, X).
X = 1 ;
X = 2 ;
X = 3 ;
X = 4 ;
X = 5.

?- find_divisors(12, D).
D = 1 ;
D = 2 ;
D = 3 ;
D = 4 ;
D = 6 ;
D = 12.
```

### ECS Integration

```prolog
% Using through conjure spells
?- cast(conjure(arithmetic_operation(add, 10, 5)), Result).
Result = 15.

?- cast(conjure(list_operation(reverse, [x, y, z])), Reversed).
Reversed = [z, y, x].

% Querying capabilities
?- query(perceive(available_operations(calculator)), Operations).
Operations = [addition, multiplication].
```

## Key Concepts Demonstrated

### Type Conversions

The template shows proper conversion between Prolog and C types:

- `PL_get_long()` / `PL_unify_integer()` for integers
- `PL_get_float()` / `PL_unify_float()` for floating-point numbers  
- `PL_get_chars()` / `PL_new_atom()` for atoms and strings
- `PL_get_list()` / `PL_cons_list()` for list manipulation
- `PL_get_functor()` / `PL_put_functor()` for compound terms

### Memory Management

Proper handling of Prolog terms and C memory:

- Using `term_t` references for Prolog terms
- Proper atom registration/unregistration with `PL_register_atom()`/`PL_unregister_atom()`
- Safe memory allocation and deallocation in C

### Non-deterministic Predicates

Implementation of choice points for backtracking:

- `PL_FIRST_CALL` for initial solution
- `PL_REDO` for subsequent solutions via backtracking
- `PL_retry_address()` to continue with choice points
- `PL_PRUNED` for cleanup when backtracking is cut

### Error Handling

Robust error checking throughout:

- Type validation with appropriate warnings
- Graceful handling of invalid inputs
- Proper failure modes for different error conditions

## Testing

The template includes comprehensive tests covering:

- All C predicates individually
- Type conversion edge cases
- Error conditions and boundary cases
- ECS component integration
- Conjure spell functionality
- Perception query operations
- Non-deterministic predicate behavior

Run tests with:

```bash
# Nix
nix run .#test-full

# Make  
make test-full

# Grimoire
grimoire test -- semantics.plt
```

## Integration with Grimoire

The template demonstrates clean integration with Grimoire's patterns:

1. **Self-entity declaration**: `:- self_entity(prolog_c_domain).`
2. **Module separation**: C predicates isolated in `auxiliary.pl`
3. **ECS architecture**: Entities, components, and relations using C predicates
4. **Spell system**: Conjure and perceive spells wrapping C functionality
5. **Documentation**: Comprehensive docstrings for all entities
6. **Testing**: PLUnit tests validating all functionality

## Development Notes

### Performance Considerations

- C predicates are significantly faster than equivalent Prolog code
- Non-deterministic predicates use choice points efficiently
- Memory allocation is minimized and properly managed

### Best Practices

- Always validate input types in C predicates
- Use appropriate error handling and warnings
- Properly manage Prolog term references
- Document all foreign predicates thoroughly
- Test both success and failure cases

### Debugging

- Use `gdb` for debugging C code
- Use `valgrind` for memory leak detection
- Enable debug builds with `make debug`
- Use SWI-Prolog's debugging facilities for Prolog code

This template provides a comprehensive foundation for integrating C code with SWI-Prolog in the Grimoire environment, demonstrating best practices for foreign function interfaces and ECS architecture integration.