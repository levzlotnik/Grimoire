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

from grimoire_client import (
    GrimoireClient, GrimoireError,
    ComponentTypesResponse, ComponentsResponse, DocumentationResponse,
    EntitiesResponse, StatusResponse, TestResponse,
    SessionCommandResponse, LoadResponse,
    ReadFileResponse, EditFileResponse
)


class GrimoireCLI:
    """Grimoire command-line interface using GrimoireClient"""

    def __init__(self):
        self.grimoire = GrimoireClient()
        self.session_id = os.getenv('GRIMOIRE_SESSION_ID')

    def show_usage(self):
        """Show usage information - mirrors cli.pl format"""
        print()
        print('🔮 Grimoire - Knowledge-Based Operating System')
        print()

        # Get current context
        try:
            # Try to get current entity, default to system
            context = "system"
            print(f'Context: {context}\n')
        except:
            print('Context: system\n')

        print('Available commands:')

        # Get docstrings from Prolog
        try:
            docs = self.grimoire.query_interface_docstrings()
            commands = ['compt', 'comp', 'doc', 'entities', 'repl', 'status',
                       'test', 'session', 'conjure', 'perceive', 'load',
                       'read_file', 'edit_file', 'exec']

            for cmd in commands:
                doc = docs.get(cmd, "No documentation available")
                # Truncate long docs
                if len(doc) > 80:
                    doc = doc[:77] + "..."
                print(f'  {cmd:12} - {doc}')
        except:
            # Fallback if docstring query fails
            print('  compt        - List all component types of current entity')
            print('  comp         - List components of specific type')
            print('  doc          - Show docstring of current entity')
            print('  entities     - List all entities in the system')
            print('  repl         - Start interactive REPL (not implemented in Python CLI)')
            print('  status       - Show session/transaction status')
            print('  test         - Run the test suite')
            print('  session      - Session management commands')
            print('  conjure      - Execute conjuration spells')
            print('  perceive     - Execute perception spells')
            print('  load         - Load entity into current session')
            print('  read_file    - Read lines from a file')
            print('  edit_file    - Edit file with operations')
            print('  exec         - Execute arbitrary Prolog query (not implemented in Python CLI)')

        print()
        print('Usage patterns:')
        print('  grimoire-py compt [entity]         # List component types')
        print('  grimoire-py comp <entity> <type>   # List components of entity')
        print('  grimoire-py doc [entity]           # Show documentation')
        print()
        print('Entity paths:')
        print('  /                               # System entity')
        print('  .                               # Current directory entity')
        print('  path/to/semantic/folder         # Custom semantic entity')
        print()
        print('Spell-based commands:')
        print('  grimoire-py conjure "operation"    # Cast conjuration spells')
        print('  grimoire-py perceive "query(...)"  # Execute perception queries')
        print()
        print('Server commands:')
        print('  grimoire-py serve [args]           # Start REST API server')
        print('  grimoire-py mcp [args]             # Start MCP server')
        print()

    def handle_compt(self, args: List[str]) -> int:
        """Handle compt command - list component types"""
        try:
            entity = args[0] if args else None
            result = self.grimoire.compt(entity)

            print(f"{result.entity} component types:")
            for comp_type in result.component_types:
                print(f"  {comp_type}")
            return 0
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1

    def handle_comp(self, args: List[str]) -> int:
        """Handle comp command - list components"""
        if len(args) < 2:
            print("Error: comp requires entity and type arguments", file=sys.stderr)
            print("Usage: grimoire-py comp <entity> <type>", file=sys.stderr)
            return 1

        try:
            entity, comp_type = args[0], args[1]
            result = self.grimoire.comp(entity, comp_type)

            print(f"{result.entity} components of type {result.component_type}:")
            print(f"{'Component':<40} | {'v/e'}")
            print("-" * 50)

            for comp in result.components:
                flag = 'e' if comp.flag == 'entity' else 'v'
                print(f"{str(comp.component):<40} | {flag}")

            return 0
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1

    def handle_doc(self, args: List[str]) -> int:
        """Handle doc command - show documentation"""
        try:
            entity = args[0] if args else None
            result = self.grimoire.doc(entity)

            print(f"{result.entity}:")
            print(result.docstring)
            return 0
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1

    def handle_entities(self, args: List[str]) -> int:
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

    def handle_status(self, args: List[str]) -> int:
        """Handle status command - show session status"""
        try:
            result = self.grimoire.status()

            # Format status with emoji
            status_emoji = "✓" if result.status.working_status == "clean" else "⚠"
            print(f"📍 Branch: {result.status.current_branch}")
            print(f"{status_emoji} Working directory: {result.status.working_status}")

            if result.status.sessions:
                print(f"🔄 Sessions: {', '.join(str(s) for s in result.status.sessions)}")

            return 0
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1

    def handle_test(self, args: List[str]) -> int:
        """Handle test command - run tests"""
        try:
            result = self.grimoire.test(args if args else None)
            print(result.result)
            # Return 0 for success - GrimoireClient will raise exception on failure
            return 0
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1

    def handle_session(self, args: List[str]) -> int:
        """Handle session command - session management"""
        if not args:
            print("Error: session command requires arguments", file=sys.stderr)
            return 1

        try:
            result = self.grimoire.session(args)
            print(result.result)
            return 0
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1

    def handle_conjure(self, args: List[str]) -> int:
        """Handle conjure command - cast spells"""
        if not args:
            print("Error: conjure requires a spell argument", file=sys.stderr)
            return 1

        spell = args[0]

        # Handle --list flag
        if spell == "--list":
            try:
                # Query conjure constructors
                comp_result = self.grimoire.comp("conjure", "ctor")
                print("Available conjure spells:")
                for comp in comp_result.components:
                    print(f"  {comp.component}")
                return 0
            except GrimoireError as e:
                print(f"Error: {e}", file=sys.stderr)
                return 1

        try:
            result = self.grimoire.call_conjure_spell(spell)
            print(result.result)
            return 0
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1

    def handle_perceive(self, args: List[str]) -> int:
        """Handle perceive command - execute queries"""
        if not args:
            print("Error: perceive requires a query argument", file=sys.stderr)
            return 1

        query = args[0]

        # Handle --list flag
        if query == "--list":
            try:
                # Query perceive constructors
                comp_result = self.grimoire.comp("perceive", "ctor")
                print("Available perceive queries:")
                for comp in comp_result.components:
                    print(f"  {comp.component}")
                return 0
            except GrimoireError as e:
                print(f"Error: {e}", file=sys.stderr)
                return 1

        try:
            result = self.grimoire.call_perceive_query(query)
            # Format output similar to Prolog CLI
            if result.solutions:
                for solution in result.solutions:
                    for key, value in solution.items():
                        print(f"{key} = {value}")
            else:
                print("No solutions found")
            return 0
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1

    def handle_load(self, args: List[str]) -> int:
        """Handle load command - load entity"""
        if not args:
            print("Error: load requires an entity spec argument", file=sys.stderr)
            return 1

        try:
            result = self.grimoire.load(args[0])
            print(f"Loaded entity: {result.entity}")
            return 0
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1

    def handle_serve(self, args: List[str]) -> int:
        """Handle serve command - start REST API server"""
        try:
            # Find rest_api.py
            api_dir = os.path.join(os.getenv('GRIMOIRE_ROOT', '.'), 'src', 'interface', 'api')
            rest_api_path = os.path.join(api_dir, 'rest_api.py')

            print(f"Starting REST API server from {rest_api_path}...")

            # Run uvicorn with rest_api
            cmd = ['uvicorn', 'rest_api:app'] + args
            result = subprocess.run(cmd, cwd=api_dir)
            return result.returncode
        except Exception as e:
            print(f"Error starting server: {e}", file=sys.stderr)
            return 1

    def handle_mcp(self, args: List[str]) -> int:
        """Handle mcp command - start MCP server"""
        try:
            # Find mcp_server.py
            api_dir = os.path.join(os.getenv('GRIMOIRE_ROOT', '.'), 'src', 'interface', 'api')
            mcp_server_path = os.path.join(api_dir, 'mcp_server.py')

            print(f"Starting MCP server from {mcp_server_path}...")

            # Run mcp server
            cmd = ['python', mcp_server_path] + args
            result = subprocess.run(cmd, cwd=api_dir)
            return result.returncode
        except Exception as e:
            print(f"Error starting MCP server: {e}", file=sys.stderr)
            return 1

    def handle_repl(self, args: List[str]) -> int:
        """Handle repl command - start interactive REPL"""
        print("Error: REPL command not yet implemented in Python CLI", file=sys.stderr)
        print("Please use the Prolog CLI: ./grimoire repl", file=sys.stderr)
        return 1

    def handle_exec(self, args: List[str]) -> int:
        """Handle exec command - execute arbitrary Prolog query"""
        if not args:
            print("Error: exec requires a query argument", file=sys.stderr)
            return 1

        query_str = args[0]
        print(f"Error: exec command not yet implemented in Python CLI", file=sys.stderr)
        print(f"Please use the Prolog CLI: ./grimoire exec \"{query_str}\"", file=sys.stderr)
        return 1

    def handle_read_file(self, args: List[str]) -> int:
        """Handle read_file command - read lines from a file"""
        if len(args) < 3:
            print("Error: read_file requires file_path, start, and end arguments", file=sys.stderr)
            print("Usage: grimoire-py read_file <file_path> <start> <end>", file=sys.stderr)
            return 1

        try:
            file_path = args[0]
            start = int(args[1])
            end = int(args[2])
            result = self.grimoire.read_file(file_path, start, end)

            for line in result.lines:
                print(f"{line.line_number}\t{line.content}")
            return 0
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1
        except ValueError as e:
            print(f"Error: start and end must be integers: {e}", file=sys.stderr)
            return 1

    def handle_edit_file(self, args: List[str]) -> int:
        """Handle edit_file command - edit file with operations"""
        print("Error: edit_file command not yet fully implemented in Python CLI", file=sys.stderr)
        print("Please use the Prolog CLI or the REST API", file=sys.stderr)
        return 1

    def run(self, args: List[str]) -> int:
        """Main entry point - dispatch to command handlers"""
        if not args:
            self.show_usage()
            return 1

        command = args[0]
        cmd_args = args[1:]

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
            'exec': self.handle_exec,
            'read_file': self.handle_read_file,
            'edit_file': self.handle_edit_file,
        }

        handler = handlers.get(command)
        if handler:
            return handler(cmd_args)
        else:
            print(f"Unknown command: {command}", file=sys.stderr)
            self.show_usage()
            return 1


def main():
    """Main entry point for grimoire-py CLI"""
    cli = GrimoireCLI()
    sys.exit(cli.run(sys.argv[1:]))


if __name__ == "__main__":
    main()
