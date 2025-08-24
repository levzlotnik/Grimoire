# GRIMOIRE: Knowledge-Based Operating System

Grimoire is an Entity-Component-System (ECS) architecture that serves as both an evolving knowledge base and an operating system. Every subsystem recursively implements the same ECS patterns, creating a fractal architecture of knowledge organization.

## Core Philosophy

### Everything is Knowledge
All system state, configuration, and operations are represented as semantic knowledge encoded in Prolog. There is no distinction between "code" and "data" - everything is declarative knowledge that can be reasoned about.

### File-Based Truth
All knowledge lives in files (`semantics.pl`), following the principle of immutability. There are no runtime assertions that modify the knowledge base - all knowledge is loaded from files and remains constant during execution. This ensures reproducibility and enables git-like versioning.

### Distributed Patterns
Knowledge is organized locally - each domain (git, nix, filesystem, etc.) maintains its own `semantics.pl` file that declares entities and components relevant to that domain. These can be composed together through the `load_entity(semantic(...))` mechanism.

### Entity-Component-System Architecture
- **Entities** (`entity/1`): Things that exist - from concrete objects like files to abstract concepts like transactions
- **Components** (`component/3`): Relationships and properties that connect entities
- **Systems**: Operations that act on entities with specific components

### Recursive Subsystems
Each domain implements the same ECS patterns recursively. For example, `nix` is an entity with its own entities like `nix(flake)`, which has its own entities like `nix(flake(template))`. This fractal structure allows unlimited depth while maintaining consistent patterns.

## Knowledge Organization

### Semantic Sources
- `semantic(file("path/to/file.pl"))`: Individual knowledge files
- `semantic(folder("path/to/dir"))`: Folders containing `semantics.pl`

Each semantic source is self-contained and can be loaded independently.

### Test Files (*.plt)
Discriminative context that validates the generative knowledge. While `.pl` files define what exists (generative), `.plt` files verify correctness (discriminative). Tests ensure the knowledge base remains consistent and operations behave as expected.

### Operation Model
- **Perceive**: Query operations that observe without modification
- **Conjure**: Mutable operations that change system state
- **Cast**: The mechanism to execute spells with proper error handling

All operations are structured as Prolog terms, maintaining semantic clarity and enabling reasoning about operations themselves.