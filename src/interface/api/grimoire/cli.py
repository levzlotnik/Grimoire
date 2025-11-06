#!/usr/bin/env python3
"""
Grimoire Python CLI - Alternative CLI using GrimoireClient

Provides the same interface as the bash+Prolog CLI but implemented in Python.
Uses GrimoireClient for direct access to Grimoire's Prolog interface layer.
"""

import sys
import os
import argparse
from typing import List, Optional
import subprocess

from grimoire.client import (
    Grimoire, GrimoireError,
    ComponentTypesResponse, ComponentsResponse, DocstringResponse,
    EntitiesResponse, TestResponse,
    GenericResponse, ConjureResponse, PerceiveResponse
)


class GrimoireCLI:
    """Grimoire command-line interface using Grimoire Python client"""

    def __init__(self):
        self.grimoire = Grimoire()
        self.session_id = os.getenv('GRIMOIRE_SESSION_ID')

    @staticmethod
    def parse_variable_args(args: list[str]) -> dict[str, any]:
        """Parse Variable=value arguments into a dict.

        Supports:
        - Simple bindings: Variable=value → {"Variable": "value"}
        - Repeated variables for lists: Paths=f1 Paths=f2 → {"Paths": ["f1", "f2"]}

        Args:
            args: List of "Variable=value" strings

        Returns:
            Dict mapping variable names to values (or lists of values)
        """
        result = {}
        for arg in args:
            if '=' not in arg:
                raise ValueError(f"Invalid variable binding: {arg} (expected Variable=value)")

            var, value = arg.split('=', 1)

            # Validate variable name is capitalized (Prolog convention)
            if not var or not var[0].isupper():
                raise ValueError(f"Variable name must start with uppercase letter: {var}")

            # Support repeated variables for lists
            if var in result:
                # Convert to list on first repeat
                if not isinstance(result[var], list):
                    result[var] = [result[var]]
                result[var].append(value)
            else:
                result[var] = value

        return result

    def create_parser(self) -> argparse.ArgumentParser:
        """Create argument parser with all subcommands"""
        parser = argparse.ArgumentParser(
            prog='grimoire-cli',
            description='🔮 Grimoire - Knowledge-Based Operating System',
            formatter_class=argparse.RawDescriptionHelpFormatter
        )

        subparsers = parser.add_subparsers(dest='command', help='Available commands')

        # compt command
        compt_parser = subparsers.add_parser('compt', help='List component types')
        compt_parser.add_argument('-e', '--entity', default=None,
                                 help='Entity to query (default: focused entity or system)')

        # comp command
        comp_parser = subparsers.add_parser('comp', help='List components of specific type')
        comp_parser.add_argument('comp_type', help='Component type to list')
        comp_parser.add_argument('-e', '--entity', default=None,
                                help='Entity to query (default: focused entity or system)')

        # doc command
        doc_parser = subparsers.add_parser('doc', help='Show entity documentation')
        doc_parser.add_argument('-e', '--entity', default=None,
                               help='Entity to document (default: focused entity or system)')

        # entities command
        subparsers.add_parser('entities', help='List all entities in the system')

        # status command
        subparsers.add_parser('status', help='Show session/transaction status')

        # test command
        test_parser = subparsers.add_parser('test', help='Run test suite')
        test_parser.add_argument('test_names', nargs='*',
                               help='Specific tests to run (default: all)')

        # session command with subcommands
        session_parser = subparsers.add_parser('session', help='Session management')
        session_subparsers = session_parser.add_subparsers(dest='session_subcommand', help='Session subcommands')

        # session create
        session_create_parser = session_subparsers.add_parser('create', help='Create a new session')
        session_create_parser.add_argument('session_id', help='Session ID to create')

        # session switch
        session_switch_parser = session_subparsers.add_parser('switch', help='Switch to a different session')
        session_switch_parser.add_argument('session_id', help='Session ID to switch to')

        # session delete
        session_delete_parser = session_subparsers.add_parser('delete', help='Delete a session')
        session_delete_parser.add_argument('session_id', help='Session ID to delete')

        # session export
        session_export_parser = session_subparsers.add_parser('export', help='Export session to archive')
        session_export_parser.add_argument('session_id', help='Session ID to export')
        session_export_parser.add_argument('destination', help='Destination directory')

        # session import
        session_import_parser = session_subparsers.add_parser('import', help='Import session from archive')
        session_import_parser.add_argument('archive', help='Archive path to import')

        # session context
        session_context_parser = session_subparsers.add_parser('context', help='Get comprehensive session context')

        # conjure command
        conjure_parser = subparsers.add_parser(
            'conjure',
            help='Cast conjuration spells',
            description='''Cast conjuration spells with template variable binding.

                Examples:
                  grimoire conjure "git(commit(message(Message)))" Message="Fix bug"
                  grimoire conjure "git(add(paths(Paths)))" Paths=file1.py Paths=file2.py
                  grimoire conjure --list

                Variables:
                  Use Variable=value syntax to bind template variables in spell signatures.
                  Variable names must be capitalized (Prolog convention).
                  Repeated variables create lists: Paths=f1 Paths=f2 → {"Paths": ["f1", "f2"]}
            ''',
            formatter_class=argparse.RawDescriptionHelpFormatter
        )
        conjure_parser.add_argument('spell', nargs='?', help='Spell to cast')
        conjure_parser.add_argument('args', nargs='*', help='Variable bindings (Variable=value)')
        conjure_parser.add_argument('--list', action='store_true', help='List available conjure spells')

        # perceive command
        perceive_parser = subparsers.add_parser(
            'perceive',
            help='Execute perception queries',
            description='''Execute perception queries with template variable binding.

                Examples:
                  grimoire perceive "git(status(git_root(Root)))" Root="/path/to/repo"
                  grimoire perceive "interface(components(entity(Entity), type(Type)))" Entity=git Type=ctor
                  grimoire perceive --list

                Variables:
                  Use Variable=value syntax to bind template variables in query signatures.
                  Variable names must be capitalized (Prolog convention).
                  Repeated variables create lists.
            ''',
            formatter_class=argparse.RawDescriptionHelpFormatter
        )
        perceive_parser.add_argument('query', nargs='?', help='Query to execute')
        perceive_parser.add_argument('args', nargs='*', help='Variable bindings (Variable=value)')
        perceive_parser.add_argument('--list', action='store_true', help='List available perceive queries')

        # load command
        load_parser = subparsers.add_parser('load', help='Load entity into current session')
        load_parser.add_argument('entity_spec', help='Entity specification to load')

        # serve command
        serve_parser = subparsers.add_parser(
            'serve',
            help='Start HTTP REST API server',
            description='''Start Grimoire HTTP REST API server using uvicorn.

                The API provides RESTful endpoints for all Grimoire operations,
                useful for web applications, integrations, and remote access.

                Examples:
                  grimoire serve                      # Start on localhost:8000
                  grimoire serve --port 3000          # Custom port
                  grimoire serve --host 0.0.0.0       # Expose to network
                  grimoire serve --reload             # Development mode with auto-reload
                  grimoire serve --workers 4          # Production with 4 workers

                Uvicorn Options:
                  --host HOST         Bind to host (default: 127.0.0.1)
                  --port PORT         Bind to port (default: 8000)
                  --reload            Enable auto-reload on code changes (development)
                  --workers N         Number of worker processes (production)
                  --log-level LEVEL   Logging level (critical, error, warning, info, debug, trace)
                  --access-log        Enable access log
                  --no-access-log     Disable access log

                API Documentation:
                  Once running, visit:
                  - http://localhost:8000/docs       (Swagger UI)
                  - http://localhost:8000/redoc      (ReDoc)
                  - http://localhost:8000/openapi.json

                Security:
                  By default, the server binds to localhost (127.0.0.1) and is only
                  accessible from the local machine. Use --host 0.0.0.0 to expose to
                  network, but ensure proper authentication is configured.
            ''',
            formatter_class=argparse.RawDescriptionHelpFormatter
        )
        serve_parser.add_argument('args', nargs='*', help='Additional uvicorn arguments')

        # mcp command
        mcp_parser = subparsers.add_parser(
            'mcp',
            help='Start MCP server for IDE integration',
            description='''Start Grimoire MCP (Model Context Protocol) server.

                The MCP server runs over stdio by default, allowing integration with
                IDEs like Claude Code, Cursor, and other MCP-compatible tools.

                Examples:
                  grimoire mcp                    # Start with stdio (default)
                  grimoire mcp --log-level debug  # Enable debug logging

                MCP Server Options:
                  --log-level LEVEL    Set logging level (debug, info, warning, error)
                                       Default: info

                IDE Integration:
                  Add to your IDE's MCP configuration (.mcp.json):
                  {
                    "mcpServers": {
                      "grimoire": {
                        "command": "grimoire",
                        "args": ["mcp"]
                      }
                    }
                  }

                Environment Variables:
                  GRIMOIRE_ROOT       Path to Grimoire installation (auto-detected)
                  GRIMOIRE_DATA       Path to session data (default: ./.grimoire)
            ''',
            formatter_class=argparse.RawDescriptionHelpFormatter
        )
        mcp_parser.add_argument('args', nargs='*', help='Additional MCP server arguments')

        # repl command
        subparsers.add_parser('repl', help='Start interactive REPL')

        # focus command
        focus_parser = subparsers.add_parser('focus', help='Focus on entity or show current focus')
        focus_parser.add_argument('target', nargs='?', help='Entity name or path to focus on (omit to show current focus)')

        # unfocus command
        subparsers.add_parser('unfocus', help='Clear focused entity')

        # init command
        init_parser = subparsers.add_parser('init', help='Initialize Grimoire for existing project')
        init_parser.add_argument('path', nargs='?', default='.',
                                help='Project directory (default: current directory)')
        init_parser.add_argument('--force', action='store_true',
                                help='Overwrite existing semantics.pl')

        # do command
        do_parser = subparsers.add_parser('do', help='Invoke a skill on an entity')
        do_parser.add_argument('skill_term', help='Skill term to invoke (e.g., "nix(build(hello))")')
        do_parser.add_argument('-e', '--entity', default=None,
                              help='Entity to invoke skill on (default: focused entity or system)')

        # skills command
        skills_parser = subparsers.add_parser('skills', help='List available skills for an entity')
        skills_parser.add_argument('-e', '--entity', default=None,
                                  help='Entity to query skills for (default: focused entity or system)')

        # exec command
        exec_parser = subparsers.add_parser('exec', help='Execute arbitrary Prolog query')
        exec_parser.add_argument('query', help='Prolog query to execute')

        # read_file command
        read_parser = subparsers.add_parser('read_file', help='Read lines from a file')
        read_parser.add_argument('file_path', help='Path to file')
        read_parser.add_argument('start', type=int, help='Start line number')
        read_parser.add_argument('end', type=int, help='End line number')

        # edit_file command
        edit_parser = subparsers.add_parser('edit_file', help='Edit file with operations')
        edit_parser.add_argument('args', nargs='+', help='Edit operation arguments')

        return parser

    def handle_compt(self, args: argparse.Namespace) -> int:
        """Handle compt command - list component types"""
        try:
            # Pass entity (could be None) - client will resolve to focused or 'system'
            result = self.grimoire.component_types(args.entity)

            # Get the resolved entity for display (it's in the client already)
            entity_display = args.entity if args.entity else "(focused or system)"
            print(f"{entity_display} component types:")
            for comp_type in result.types:
                print(f"  {comp_type}")
            return 0
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1

    def handle_comp(self, args: argparse.Namespace) -> int:
        """Handle comp command - list components"""
        try:
            # Pass entity (could be None) - client will resolve to focused or 'system'
            result = self.grimoire.components(args.entity, args.comp_type)

            entity_display = args.entity if args.entity else "(focused or system)"
            print(f"{entity_display} components of type {args.comp_type}:")

            if result.is_unique:
                print(f"  {result.value}")
            else:
                for comp in result.values:
                    print(f"  {comp}")

            return 0
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1

    def handle_doc(self, args: argparse.Namespace) -> int:
        """Handle doc command - show documentation"""
        try:
            # Pass entity (could be None) - client will resolve to focused or 'system'
            result = self.grimoire.docstring(args.entity)

            entity_display = args.entity if args.entity else "(focused or system)"
            print(f"Documentation for '{entity_display}':")
            print(result.docstring)
            return 0
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1

    def handle_entities(self, args: argparse.Namespace) -> int:
        """Handle entities command - list all entities"""
        try:
            result = self.grimoire.entities()

            print(f"System entities ({len(result.entities)} total):")
            for entity in result.entities:
                print(f"  {entity}")
            return 0
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1

    def handle_status(self, args: argparse.Namespace) -> int:
        """Handle status command - show session status"""
        try:
            result = self.grimoire.session_status()
            if result.is_success:
                status = result.result
                # Pretty-print the structured session status
                if hasattr(status, 'functor') and status.functor == 'session_status':
                    session_id = status.args[0].args[0] if hasattr(status.args[0], 'args') else status.args[0]
                    session_path = status.args[1].args[0] if hasattr(status.args[1], 'args') else status.args[1]
                    focused_term = status.args[2]
                    active_term = status.args[3]

                    print(f"Session: {session_id}")
                    print(f"Path: {session_path}")

                    # Handle focused entity
                    if hasattr(focused_term, 'functor') and focused_term.functor == 'entity':
                        focused_entity = focused_term.args[0]
                        print(f"Focused: {focused_entity}")
                    else:
                        print("Focused: none")

                    print(f"Active: {active_term}")
                else:
                    print(result.result)
            else:
                print(f"Error: {result.error}", file=sys.stderr)
                return 1
            return 0
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1

    def handle_test(self, args: argparse.Namespace) -> int:
        """Handle test command - run tests"""
        try:
            result = self.grimoire.test(args.test_names if args.test_names else None)
            if result.status == "passed":
                print("Tests passed!")
                return 0
            elif result.status == "listed":
                print("Tests listed")
                return 0
            else:
                print(f"Tests failed: {result.details}", file=sys.stderr)
                return 1
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1

    def handle_session(self, args: argparse.Namespace) -> int:
        """Handle session command - session management"""
        try:
            if not args.session_subcommand:
                print("Error: session command requires subcommand (create/switch/delete/export/import/context)", file=sys.stderr)
                return 1

            if args.session_subcommand == "create":
                result = self.grimoire.session_create(args.session_id)
                print(result.result)
            elif args.session_subcommand == "switch":
                result = self.grimoire.session_switch(args.session_id)
                print(result.result)
            elif args.session_subcommand == "delete":
                result = self.grimoire.session_delete(args.session_id)
                print(result.result)
            elif args.session_subcommand == "export":
                result = self.grimoire.session_export(args.session_id, args.destination)
                print(result.result)
            elif args.session_subcommand == "import":
                result = self.grimoire.session_import(args.archive)
                print(result.result)
            elif args.session_subcommand == "context":
                result = self.grimoire.session_context()
                # Pretty-print the context result
                import yaml
                print(yaml.dump(result.model_dump(), default_flow_style=False, sort_keys=False))
            else:
                print(f"Error: Unknown session subcommand: {args.session_subcommand}", file=sys.stderr)
                return 1

            return 0
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1

    def handle_conjure(self, args: argparse.Namespace) -> int:
        """Handle conjure command - cast spells"""
        # Handle --list flag
        if hasattr(args, 'list') and args.list:
            try:
                # Query conjure constructors
                comp_result = self.grimoire.components("conjure", "ctor")
                print("Available conjure spells:")
                values = comp_result.values if not comp_result.is_unique else [comp_result.value]
                for comp in values:
                    print(f"  {comp}")
                return 0
            except GrimoireError as e:
                print(f"Error: {e}", file=sys.stderr)
                return 1

        if not args.spell:
            print("Error: spell argument required (or use --list to show available spells)", file=sys.stderr)
            return 1

        try:
            # Parse Variable=value arguments
            spell_args = self.parse_variable_args(args.args) if args.args else {}

            # Cast spell with parsed arguments
            result = self.grimoire.conjure(args.spell, spell_args)
            print(result.result)
            return 0
        except ValueError as e:
            print(f"Error parsing arguments: {e}", file=sys.stderr)
            return 1
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1

    def handle_perceive(self, args: argparse.Namespace) -> int:
        """Handle perceive command - execute queries"""
        # Handle --list flag
        if hasattr(args, 'list') and args.list:
            try:
                # Query perceive constructors
                comp_result = self.grimoire.components("perceive", "ctor")
                print("Available perceive queries:")
                values = comp_result.values if not comp_result.is_unique else [comp_result.value]
                for comp in values:
                    print(f"  {comp}")
                return 0
            except GrimoireError as e:
                print(f"Error: {e}", file=sys.stderr)
                return 1

        if not args.query:
            print("Error: query argument required (or use --list to show available queries)", file=sys.stderr)
            return 1

        try:
            # Parse Variable=value arguments
            query_args = self.parse_variable_args(args.args) if args.args else {}

            # Execute query with parsed arguments
            result = self.grimoire.perceive(args.query, query_args)
            print(result.result)
            return 0
        except ValueError as e:
            print(f"Error parsing arguments: {e}", file=sys.stderr)
            return 1
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1

    def handle_load(self, args: argparse.Namespace) -> int:
        """Handle load command - load entity"""
        print("Error: load command not yet implemented in interface domain", file=sys.stderr)
        print("Use the Prolog CLI: ./grimoire load", file=sys.stderr)
        return 1

    def handle_serve(self, args: argparse.Namespace) -> int:
        """Handle serve command - start REST API server"""
        try:
            # Find rest_api.py
            api_dir = os.path.join(os.getenv('GRIMOIRE_ROOT', '.'), 'src', 'interface', 'api')
            rest_api_path = os.path.join(api_dir, 'rest_api.py')

            print(f"Starting REST API server from {rest_api_path}...")

            # Run uvicorn with rest_api
            cmd = ['uvicorn', 'rest_api:app'] + args.args
            result = subprocess.run(cmd, cwd=api_dir)
            return result.returncode
        except Exception as e:
            print(f"Error starting server: {e}", file=sys.stderr)
            return 1

    def handle_mcp(self, args: argparse.Namespace) -> int:
        """Handle mcp command - start MCP server"""
        try:
            # Import and run the MCP server main function
            from grimoire.mcp import main as mcp_main
            return mcp_main(args.args) or 0
        except Exception as e:
            print(f"Error starting MCP server: {e}", file=sys.stderr)
            return 1

    def handle_repl(self, args: argparse.Namespace) -> int:
        """Handle repl command - start interactive REPL"""
        print("Error: REPL command not yet implemented in Python CLI", file=sys.stderr)
        print("Please use the Prolog CLI: ./grimoire repl", file=sys.stderr)
        return 1

    def handle_focus(self, args: argparse.Namespace) -> int:
        """Handle focus command - focus on entity or show current focus"""
        try:
            # No target - show current focus
            if not args.target:
                result = self.grimoire.session_get_focused()
                if result.is_success:
                    focused = result.result
                    # Pretty-print the structured output
                    if hasattr(focused, 'functor') and focused.functor == 'focused_entity':
                        entity_term = focused.args[0]
                        loc_term = focused.args[1]
                        key_comps_term = focused.args[2]

                        entity_name = str(entity_term.args[0]) if hasattr(entity_term, 'args') else str(entity_term)
                        print(f"Focused on: {entity_name}")

                        if hasattr(loc_term, 'args') and loc_term.args:
                            print(f"Location: {loc_term.args[0]}")

                        if hasattr(key_comps_term, 'args') and key_comps_term.args:
                            comps = key_comps_term.args[0] if key_comps_term.args else []
                            if comps and hasattr(comps, '__iter__'):
                                print(f"Key components: {', '.join(str(c) for c in comps)}")
                    else:
                        print(result.result)
                else:
                    print("No entity focused")
                return 0

            # Has target - set focus
            target = args.target

            # Detect if target is a path (contains / or is .)
            if '/' in target or target == '.':
                # Resolve to absolute path
                import os
                abs_path = os.path.abspath(target)
                result = self.grimoire.session_focus_path(abs_path)
            else:
                # Treat as entity name
                result = self.grimoire.session_focus_entity(target)

            if result.is_success:
                focused = result.result
                if hasattr(focused, 'functor') and focused.functor == 'focused':
                    entity_term = focused.args[0]
                    entity_name = str(entity_term.args[0]) if hasattr(entity_term, 'args') else str(entity_term)
                    print(f"Focused on: {entity_name}")
                else:
                    print(result.result)
            else:
                print(f"Error: {result.error}", file=sys.stderr)
                return 1

            return 0
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1

    def handle_unfocus(self, args: argparse.Namespace) -> int:
        """Handle unfocus command - clear focused entity"""
        try:
            result = self.grimoire.session_unfocus()
            if result.is_success:
                print("Focus cleared")
            else:
                print(f"Error: {result.error}", file=sys.stderr)
                return 1
            return 0
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1

    def handle_init(self, args: argparse.Namespace) -> int:
        """Handle init command - initialize Grimoire for existing project"""
        try:
            import os
            abs_path = os.path.abspath(args.path)
            options = ['force'] if args.force else []

            result = self.grimoire.conjure(
                "project(init(folder(Folder), options(Options)))",
                {"Folder": abs_path, "Options": options}
            )

            if result.is_success:
                print(result.result)
            else:
                print(f"Error: {result.error}", file=sys.stderr)
                return 1
            return 0
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1

    def handle_do(self, args: argparse.Namespace) -> int:
        """Handle do command - invoke a skill on an entity"""
        try:
            result = self.grimoire.do(args.skill_term, args.entity)
            if result.is_success:
                print(result.result)
            else:
                print(f"Error: {result.error}", file=sys.stderr)
                return 1
            return 0
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1

    def handle_skills(self, args: argparse.Namespace) -> int:
        """Handle skills command - list available skills for an entity"""
        try:
            result = self.grimoire.skills(args.entity)
            if result.is_success:
                entity_display = args.entity if args.entity else "(focused or system)"
                print(f"Available skills for {entity_display}:")
                # Result structure is ok(skills_list([skill(...), ...]))
                if hasattr(result.result, 'functor') and result.result.functor == 'skill_result':
                    # Unwrap skill_result from invoke_skill delegation
                    inner = result.result.args[0]
                    if hasattr(inner, 'functor') and inner.functor == 'skills_list':
                        skills = inner.args[0] if inner.args else []
                        if skills and hasattr(skills, '__iter__'):
                            for skill in skills:
                                print(f"  {skill}")
                        else:
                            print("  (no skills available)")
                elif hasattr(result.result, 'functor') and result.result.functor == 'skills_list':
                    skills = result.result.args[0] if result.result.args else []
                    if skills and hasattr(skills, '__iter__'):
                        for skill in skills:
                            print(f"  {skill}")
                    else:
                        print("  (no skills available)")
                else:
                    print(result.result)
            else:
                print(f"Error: {result.error}", file=sys.stderr)
                return 1
            return 0
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1

    def handle_exec(self, args: argparse.Namespace) -> int:
        """Handle exec command - execute arbitrary Prolog query"""
        try:
            solutions = self.grimoire.exec(args.query)
            # Format output similar to Prolog CLI - emit variable bindings
            if solutions:
                for solution in solutions:
                    if solution:
                        # Solution has variable bindings
                        for key, value in solution.items():
                            print(f"{key} = {value}")
                    else:
                        # Solution with no variables (e.g., "true")
                        print("true")
            else:
                # No solutions found - query failed
                print("false")
            return 0
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1

    def handle_read_file(self, args: argparse.Namespace) -> int:
        """Handle read_file command - read lines from a file"""
        print("Error: read_file command not yet implemented in interface domain", file=sys.stderr)
        print("Use the Prolog CLI or fs domain directly", file=sys.stderr)
        return 1

    def handle_edit_file(self, args: argparse.Namespace) -> int:
        """Handle edit_file command - edit file with operations"""
        print("Error: edit_file command not yet fully implemented in Python CLI", file=sys.stderr)
        print("Please use the Prolog CLI or the REST API", file=sys.stderr)
        return 1

    def run(self, argv: Optional[List[str]] = None) -> int:
        """Main entry point - dispatch to command handlers"""
        parser = self.create_parser()

        # Parse arguments
        args = parser.parse_args(argv)

        # If no command provided, show help
        if not args.command:
            parser.print_help()
            return 1

        # Dispatch to command handlers
        handlers = {
            'compt': self.handle_compt,
            'comp': self.handle_comp,
            'doc': self.handle_doc,
            'entities': self.handle_entities,
            'status': self.handle_status,
            'test': self.handle_test,
            'session': self.handle_session,
            'conjure': self.handle_conjure,
            'perceive': self.handle_perceive,
            'load': self.handle_load,
            'serve': self.handle_serve,
            'mcp': self.handle_mcp,
            'repl': self.handle_repl,
            'focus': self.handle_focus,
            'unfocus': self.handle_unfocus,
            'init': self.handle_init,
            'do': self.handle_do,
            'skills': self.handle_skills,
            'exec': self.handle_exec,
            'read_file': self.handle_read_file,
            'edit_file': self.handle_edit_file,
        }

        handler = handlers.get(args.command)
        if handler:
            return handler(args)
        else:
            print(f"Unknown command: {args.command}", file=sys.stderr)
            parser.print_help()
            return 1


def main(argv: Optional[List[str]] = None) -> int:
    """Main entry point for grimoire-py CLI

    Args:
        argv: Command line arguments. If None, uses sys.argv[1:]

    Returns:
        Exit code (0 for success, non-zero for error)
    """
    if argv is None:
        argv = sys.argv[1:]

    cli = GrimoireCLI()
    return cli.run(argv)


if __name__ == "__main__":
    main()
