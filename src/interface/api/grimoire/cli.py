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

        # conjure command
        conjure_parser = subparsers.add_parser('conjure', help='Cast conjuration spells')
        conjure_parser.add_argument('spell', nargs='?', help='Spell to cast')
        conjure_parser.add_argument('--list', action='store_true', help='List available conjure spells')

        # perceive command
        perceive_parser = subparsers.add_parser('perceive', help='Execute perception queries')
        perceive_parser.add_argument('query', nargs='?', help='Query to execute')
        perceive_parser.add_argument('--list', action='store_true', help='List available perceive queries')

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
            entity = args.entity or 'system'
            result = self.grimoire.component_types(entity)

            print(f"{entity} component types:")
            for comp_type in result.types:
                print(f"  {comp_type}")
            return 0
        except GrimoireError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1

    def handle_comp(self, args: argparse.Namespace) -> int:
        """Handle comp command - list components"""
        try:
            result = self.grimoire.components(args.entity, args.comp_type)

            print(f"{args.entity} components of type {args.comp_type}:")

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
            entity = args.entity or 'system'
            result = self.grimoire.docstring(entity)

            print(f"Documentation for '{entity}':")
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
        print("Error: status command not yet implemented in interface domain", file=sys.stderr)
        print("Use the Prolog CLI: ./grimoire status", file=sys.stderr)
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
                print("Error: session command requires subcommand (create/switch/delete/export/import)", file=sys.stderr)
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
            # Parse spell signature - assume no args for now (can be extended later)
            result = self.grimoire.conjure(args.spell, {})
            print(result.result)
            return 0
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
            # Parse query signature - assume no args for now (can be extended later)
            result = self.grimoire.perceive(args.query, {})
            print(result.result)
            return 0
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
