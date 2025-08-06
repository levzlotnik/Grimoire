# Grimoire

A Knowledge-Based Operating System built on Entity-Component-System (ECS) architecture using Prolog for semantic knowledge management and Nix for system configuration.

## Core Philosophy

Grimoire treats the operating system as a knowledge management system where:
- **Semantic knowledge** lives in Prolog files (`semantics.pl`)
- **System state** is managed through Nix
- **Each domain** uses its natural tools while integrating through common patterns
- **Knowledge is local** to where it belongs (following git-like distributed patterns)

## Architecture

### Entity-Component-System (ECS)
- Inspired by game engine architectures
- Extensible sum types through `component(Type, ctor, ...)`
- Clean composition of behaviors across domains

### Domain Integration
- **Nix**: System configuration and package management
- **Git**: Version control operations
- **Prolog**: Semantic knowledge and query interface
- Each subsystem maintains its natural paradigm

### Knowledge Organization
- Distributed `semantics.pl` files define domain knowledge
- Transaction-based operations for system changes
- Mountable semantic modules for runtime extension

## Current Status

### ✅ Completed Phases

**Phase 1: Foundation** - Core ECS system with semantic mounting
**Phase 2: Nix Mastery** - Comprehensive flake-based operations with dynamic discovery
**Phase 3: Testing Infrastructure** - 44 comprehensive tests with 100% core coverage

### Implemented Features
- **Core ECS System**: Entity-component-system foundation with semantic mounting ✅
- **Nix Integration**: Comprehensive flake-based operations with dynamic target discovery ✅
  - Flake templates with JSON introspection and memoized performance
  - Build, develop, run, search commands with sum type architecture
  - Store operations (gc, repair, optimise) and debugging tools (log, why-depends)
  - Seven language template domains with automatic discovery
- **Git Integration**: Version control operations with DCG parsing and docstrings ✅
- **Project Management**: Context/configuration/dependency handling with database integration ✅
- **Testing Infrastructure**: PLUnit-based comprehensive testing with co-located test files ✅
- **Python Bridge**: Prolog-Python interface for external tool integration ✅

### Technical Achievements
- **Dynamic Target Discovery**: Self-discovering flake outputs using clean sum types
- **Memoized Operations**: Tabled predicates for efficient repeated flake queries
- **Cross-Domain Integration**: Proven semantic mounting across Git/Nix/Project domains
- **Robust Testing**: 44 tests covering ECS fundamentals, domain operations, and integration
- **Clean Architecture**: Well-tested foundation ready for advanced feature development

### 🚀 Ready for Next Phases
- **Phase 4**: Enhanced Git operations (merge, rebase, stash, advanced status/diff)
- **Phase 5**: Polyglot discovery for language-native introspection (Rust, Python, Haskell, etc.)
- **Context Engineering**: Structured semantic knowledge for massive context compression
- **Performance Optimization**: Memoized introspection and efficient entity querying

### Next Priorities
1. **Testing Implementation**: Deploy PLUnit test suite across all semantic domains
2. **Polyglot Expansion**: Rust, Dart, Lean4 semantic discovery
3. **Context Compression**: Leverage structured knowledge for 80-90% token savings
4. **Integration Testing**: End-to-end workflow validation

*"In the pursuit of knowledge, even the smallest spell can unlock great understanding."*
