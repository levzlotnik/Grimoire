{{base_context}}

Your task is to use the exploration results to plan the necessary commands for solving the task.
Create a transaction plan that will accomplish this task.
Explain your reasoning for each command.

Note:
    - Based on exploration, identify needed commands
    - Commands are constructors of the command entity
    - Each command variant has specific parameters
    - Build a transaction list of commands

Example command construction:

    User: TASK: Create a new project directory and initialize a flake.nix file

    AI: return: {
        "summary": "To create a new project directory and initialize a flake.nix file, we will use the mkproject and nix(flake(init)) commands.",
        "commands": [
            {
                "command": "mkproject('/path/to/project', [git(true), template(basic)])",
                "rationale": "The mkproject command is used to create a new project directory with git initialization and a basic template."
            },
            {
                "command": "nix(flake(init('/path/to/project')))",
                "rationale": "Initialize a flake.nix file in the project directory."
            }
        ],
    }

### Example planning:

Assuming the following knowledge tree:
{
  "system": {
    "docstring": "...",
    "concept": ["$command", "$git", "$nix", ...]
  },
  "command": {
    "docstring": "...",
    "constructors": ["mkdir", "mkfile", "mkproject", "nix(flake(init))", ...],
  },
  "mkproject": {
    "docstring": "Creates a new project directory with full initialization. Format: mkproject(Path, Options) Available Options: - git: Initialize a git repository - template: Use a specific template - nix: Use Nix package manager",
    "option": ["$git", "$template"]
  },
  "git": {
    "docstring": "Initialize a git repository",
    "parameters": ["true", "false"]
  },
  "template": {
    "docstring": "Use a specific template",
    "parameters": ["basic", "full"]
  },
  "nix(flake(init))": {
    "docstring": "Initialize a flake.nix file in the project directory Format: nix(flake(init(Path)))",
    "parameters": ["path"]
  }
}
TASK: Create a new project directory and initialize a flake.nix file
AI: return: {
"summary": "To create a new project directory and initialize a flake.nix file, we will use the mkproject and nix(flake(init)) commands.",
"commands": [
    {
    "command": "mkproject('/path/to/project', [git(true), template(basic)])",
    "rationale": "The mkproject command is used to create a new project directory with git initialization and a basic template."
    },
    {
    "command": "nix(flake(init('/path/to/project')))",
    "rationale": "Initialize a flake.nix file in the project directory."
    }
],
}

Relevant Knowledge:
{{knowledge_tree}}

Response format:
{{retval_format}}