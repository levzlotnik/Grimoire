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

    def create_parser(self) -> argparse.ArgumentParser:
        """Create argument parser with all subcommands"""
        parser = argparse.ArgumentParser(
            prog='grimoire-py',
            description='🔮 Grimoire - Knowledge-Based Operating System',
            formatter_class=argparse.RawDescriptionHelpFormatter
        )

        subparsers = parser.add_subparsers(dest='command', help='Available commands')

        # compt command
        compt_parser = subparsers.add_parser('compt', help='List component types')
        compt_parser.add_argument('entity', nargs='?', default=None,
                                 help='Entity to query (default: current context)')

        # comp command
        comp_parser = subparsers.add_parser('comp', help='List components of specific type')
        comp_parser.add_argument('entity', help='Entity to query')
        comp_parser.add_argument('comp_type', help='Component type to list')

        # doc command
        doc_parser = subparsers.add_parser('doc', help='Show entity documentation')
        doc_parser.add_argument('entity', nargs='?', default=None,
                               help='Entity to document (default: current context)')

        # entities command
        subparsers.add_parser('entities', help='List all entities in the system')

        # status command
        subparsers.add_parser('status', help='Show session/transaction status')

        # test command
        test_parser = subparsers.add_parser('test', help='Run test suite')
        test_parser.add_argument('test_names', nargs='*',
                               help='Specific tests to run (default: all)')

        # session command
        session_parser = subparsers.add_parser('session', help='Session management')
        session_parser.add_argument('args', nargs='+', help='Session command arguments')

        # conjure command
        conjure_parser = subparsers.add_parser('conjure', help='Cast conjuration spells')
        conjure_parser.add_argument('spell', help='Spell to cast (or --list to show available)')

        # perceive command
        perceive_parser = subparsers.add_parser('perceive', help='Execute perception queries')
        perceive_parser.add_argument('query', help='Query to execute (or --list to show available)')

        # load command
        load_parser = subparsers.add_parser('load', help='Load entity into current session')
        load_parser.add_argument('entity_spec', help='Entity specification to load')

        # serve command
        serve_parser = subparsers.add_parser('serve', help='Start REST API server')
        serve_parser.add_argument('args', nargs='*', help='Additional uvicorn arguments')

        # mcp command
        mcp_parser = subparsers.add_parser('mcp', help='Start MCP server')
        mcp_parser.add_argument('args', nargs='*', help='Additional MCP server arguments')

        # repl command
        subparsers.add_parser('repl', help='Start interactive REPL')

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
            result = self.grimoire.compt(args.entity)

            print(f"{result.entity} component types:")
            for comp_type in result.component_types:
                print(f"  {comp_type}")
            return 0
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1

    def handle_comp(self, args: argparse.Namespace) -> int:
        """Handle comp command - list components"""
        try:
            result = self.grimoire.comp(args.entity, args.comp_type)

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

    def handle_doc(self, args: argparse.Namespace) -> int:
        """Handle doc command - show documentation"""
        try:
            result = self.grimoire.doc(args.entity)

            print(f"{result.entity}:")
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

    def handle_test(self, args: argparse.Namespace) -> int:
        """Handle test command - run tests"""
        try:
            result = self.grimoire.test(args.test_names if args.test_names else None)
            print(result.result)
            # Return 0 for success - GrimoireClient will raise exception on failure
            return 0
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1

    def handle_session(self, args: argparse.Namespace) -> int:
        """Handle session command - session management"""
        try:
            result = self.grimoire.session(args.args)
            print(result.result)
            return 0
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1

    def handle_conjure(self, args: argparse.Namespace) -> int:
        """Handle conjure command - cast spells"""
        # Handle --list flag
        if args.spell == "--list":
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
            result = self.grimoire.call_conjure_spell(args.spell)
            print(result.result)
            return 0
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1

    def handle_perceive(self, args: argparse.Namespace) -> int:
        """Handle perceive command - execute queries"""
        # Handle --list flag
        if args.query == "--list":
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
            result = self.grimoire.call_perceive_query(args.query)
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

    def handle_load(self, args: argparse.Namespace) -> int:
        """Handle load command - load entity"""
        try:
            result = self.grimoire.load(args.entity_spec)
            print(f"Loaded entity: {result.entity}")
            return 0
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
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
            # Find mcp_server.py
            api_dir = os.path.join(os.getenv('GRIMOIRE_ROOT', '.'), 'src', 'interface', 'api')
            mcp_server_path = os.path.join(api_dir, 'mcp_server.py')

            print(f"Starting MCP server from {mcp_server_path}...")

            # Run mcp server using the same Python interpreter as this script
            cmd = [sys.executable, mcp_server_path] + args.args
            result = subprocess.run(cmd, cwd=api_dir)
            return result.returncode
        except Exception as e:
            print(f"Error starting MCP server: {e}", file=sys.stderr)
            return 1

    def handle_repl(self, args: argparse.Namespace) -> int:
        """Handle repl command - start interactive REPL"""
        print("Error: REPL command not yet implemented in Python CLI", file=sys.stderr)
        print("Please use the Prolog CLI: ./grimoire repl", file=sys.stderr)
        return 1

    def handle_exec(self, args: argparse.Namespace) -> int:
        """Handle exec command - execute arbitrary Prolog query"""
        try:
            result = self.grimoire.exec(args.query)
            # Format output similar to Prolog CLI - emit variable bindings
            if result.solutions:
                for solution in result.solutions:
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
        try:
            result = self.grimoire.read_file(args.file_path, args.start, args.end)

            for line in result.lines:
                print(f"{line.line_number}\t{line.content}")
            return 0
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
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


def main():
    """Main entry point for grimoire-py CLI"""
    cli = GrimoireCLI()
    sys.exit(cli.run())


if __name__ == "__main__":
    main()
