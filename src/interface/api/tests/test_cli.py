"""
CLI tests for grimoire-cli command

Tests CLI by calling main() with arguments and capturing stdout.
"""
import pytest
import sys
import io
from grimoire.cli import main


class TestCLI:
    """Test suite for Grimoire CLI"""

    def test_entities_command(self):
        """Test entities command"""
        old_stdout = sys.stdout
        sys.stdout = captured = io.StringIO()

        try:
            result = main(["entities"])
            output = captured.getvalue()
        finally:
            sys.stdout = old_stdout

        assert result == 0
        assert "system" in output

    def test_compt_command(self):
        """Test compt command"""
        old_stdout = sys.stdout
        sys.stdout = captured = io.StringIO()

        try:
            result = main(["compt", "system"])
            output = captured.getvalue()
        finally:
            sys.stdout = old_stdout

        assert result == 0
        assert "component types" in output

    def test_comp_command(self):
        """Test comp command"""
        old_stdout = sys.stdout
        sys.stdout = captured = io.StringIO()

        try:
            result = main(["comp", "system", "self"])
            output = captured.getvalue()
        finally:
            sys.stdout = old_stdout

        assert result == 0

    def test_doc_command(self):
        """Test doc command"""
        old_stdout = sys.stdout
        sys.stdout = captured = io.StringIO()

        try:
            result = main(["doc", "system"])
            output = captured.getvalue()
        finally:
            sys.stdout = old_stdout

        assert result == 0
        assert len(output) > 0

    def test_conjure_list(self):
        """Test conjure --list"""
        old_stdout = sys.stdout
        sys.stdout = captured = io.StringIO()

        try:
            result = main(["conjure", "--list"])
            output = captured.getvalue()
        finally:
            sys.stdout = old_stdout

        assert result == 0
        assert "Available conjure spells" in output

    def test_perceive_list(self):
        """Test perceive --list"""
        old_stdout = sys.stdout
        sys.stdout = captured = io.StringIO()

        try:
            result = main(["perceive", "--list"])
            output = captured.getvalue()
        finally:
            sys.stdout = old_stdout

        assert result == 0
        assert "Available perceive queries" in output
