"""
Comprehensive tests for MCP Server implementation

Tests all MCP tools with mocked GrimoireClient to ensure proper integration
and error handling.
"""

import pytest
from unittest.mock import Mock, patch, MagicMock
from pathlib import Path
import sys

# Add the parent directory to the path so we can import our modules
sys.path.insert(0, str(Path(__file__).parent.parent))

from grimoire_client import (
    GrimoireClient,
    GrimoireError,
    ComponentTypesResponse,
    ComponentsResponse,
    ComponentEntry,
    DocumentationResponse,
    StatusResponse,
    StatusInfo,
    EntitiesResponse,
    TestResponse,
    SessionCommandResponse,
    LoadResponse,
    ReadFileResponse,
    LineContent,
    EditFileResponse,
    PerceiveResponse,
    ConjureResponse,
    EditInsert,
    EditDelete,
    EditReplace,
    EditAppend,
)


class TestMCPServerInitialization:
    """Test MCP server initialization and setup"""

    @patch('mcp_server.GrimoireClient')
    def test_grimoire_client_initialization(self, mock_client_class):
        """Test that GrimoireClient is initialized on import"""
        # Reset the module to test initialization
        if 'mcp_server' in sys.modules:
            del sys.modules['mcp_server']

        mock_instance = Mock()
        mock_client_class.return_value = mock_instance

        # Import should initialize grimoire
        import mcp_server

        # Should have created client instance
        mock_client_class.assert_called_once()

    @patch('mcp_server.GrimoireClient')
    def test_interface_docs_query(self, mock_client_class):
        """Test that interface docstrings are queried on initialization"""
        if 'mcp_server' in sys.modules:
            del sys.modules['mcp_server']

        mock_instance = Mock()
        mock_instance.query_interface_docstrings.return_value = {
            "compt": "Test compt doc",
            "comp": "Test comp doc",
        }
        mock_client_class.return_value = mock_instance

        import mcp_server

        # Should have queried docstrings
        mock_instance.query_interface_docstrings.assert_called_once()

    @patch('mcp_server.GrimoireClient')
    def test_system_instructions_query(self, mock_client_class):
        """Test that system instructions are queried for MCP server"""
        if 'mcp_server' in sys.modules:
            del sys.modules['mcp_server']

        mock_instance = Mock()
        mock_instance.system_instructions.return_value = "Test instructions"
        mock_instance.query_interface_docstrings.return_value = {}
        mock_client_class.return_value = mock_instance

        import mcp_server

        # Should have queried system instructions for MCP init
        mock_instance.system_instructions.assert_called_once()


class TestYAMLFormatting:
    """Test YAML formatting helper"""

    @patch('mcp_server.grimoire')
    def test_model_to_string_with_basemodel(self, mock_grimoire):
        """Test _model_to_string with Pydantic BaseModel"""
        from mcp_server import _model_to_string

        # Create a mock response
        response = ComponentTypesResponse(entity="test", types=["type1", "type2"])

        result = _model_to_string(response)

        # Should be YAML formatted
        assert isinstance(result, str)
        assert "entity:" in result
        assert "test" in result
        assert "types:" in result

    @patch('mcp_server.grimoire')
    def test_model_to_string_with_non_basemodel(self, mock_grimoire):
        """Test _model_to_string with non-BaseModel input"""
        from mcp_server import _model_to_string

        result = _model_to_string("plain string")

        # Should just convert to string
        assert result == "plain string"

    @patch('mcp_server.grimoire')
    def test_model_to_string_preserves_order(self, mock_grimoire):
        """Test that _model_to_string preserves key order"""
        from mcp_server import _model_to_string

        response = StatusResponse(
            status=StatusInfo(
                current_branch="main",
                working_status="clean",
                sessions=["session1"]
            )
        )

        result = _model_to_string(response)

        # Should preserve order (current_branch before working_status)
        assert result.index("current_branch") < result.index("working_status")


class TestComptTool:
    """Test compt MCP tool"""

    @patch('mcp_server.grimoire')
    def test_compt_default_entity(self, mock_grimoire):
        """Test compt with default entity (system)"""
        from mcp_server import compt

        mock_grimoire.compt.return_value = ComponentTypesResponse(
            entity="system",
            types=["ctor", "subcommand", "docstring"]
        )

        result = compt()

        mock_grimoire.compt.assert_called_once_with("system")
        assert "entity: system" in result
        assert "types:" in result

    @patch('mcp_server.grimoire')
    def test_compt_custom_entity(self, mock_grimoire):
        """Test compt with custom entity"""
        from mcp_server import compt

        mock_grimoire.compt.return_value = ComponentTypesResponse(
            entity="git",
            types=["ctor", "docstring"]
        )

        result = compt("git")

        mock_grimoire.compt.assert_called_once_with("git")
        assert "git" in result


class TestCompTool:
    """Test comp MCP tool"""

    @patch('mcp_server.grimoire')
    def test_comp_basic(self, mock_grimoire):
        """Test comp with entity and component type"""
        from mcp_server import comp

        mock_grimoire.comp.return_value = ComponentsResponse(
            entity="interface",
            component_type="subcommand",
            components=[
                ComponentEntry(component="compt", flag="value"),
                ComponentEntry(component="comp", flag="value"),
            ]
        )

        result = comp("interface", "subcommand")

        mock_grimoire.comp.assert_called_once_with("interface", "subcommand")
        assert "interface" in result
        assert "subcommand" in result
        assert "compt" in result


class TestDocTool:
    """Test doc MCP tool"""

    @patch('mcp_server.grimoire')
    def test_doc_default_entity(self, mock_grimoire):
        """Test doc with default entity"""
        from mcp_server import doc

        mock_grimoire.doc.return_value = DocumentationResponse(
            entity="system",
            documentation="System documentation here"
        )

        result = doc()

        mock_grimoire.doc.assert_called_once_with("system")
        assert "system" in result
        assert "documentation" in result

    @patch('mcp_server.grimoire')
    def test_doc_custom_entity(self, mock_grimoire):
        """Test doc with custom entity"""
        from mcp_server import doc

        mock_grimoire.doc.return_value = DocumentationResponse(
            entity="git",
            documentation="Git domain documentation"
        )

        result = doc("git")

        mock_grimoire.doc.assert_called_once_with("git")


class TestStatusTool:
    """Test status MCP tool"""

    @patch('mcp_server.grimoire')
    def test_status(self, mock_grimoire):
        """Test status tool"""
        from mcp_server import status

        mock_grimoire.status.return_value = StatusResponse(
            status=StatusInfo(
                current_branch="main",
                working_status="clean",
                sessions=["main", "session1"]
            )
        )

        result = status()

        mock_grimoire.status.assert_called_once()
        assert "main" in result
        assert "clean" in result


class TestPerceivetTool:
    """Test perceive MCP tool"""

    @patch('mcp_server.grimoire')
    def test_perceive_query(self, mock_grimoire):
        """Test perceive with query string"""
        from mcp_server import perceive

        mock_grimoire.call_perceive_query.return_value = PerceiveResponse(
            solutions=[{"X": "value1"}, {"X": "value2"}],
            count=2,
            query="test_query"
        )

        result = perceive("test_query")

        mock_grimoire.call_perceive_query.assert_called_once_with("test_query")
        assert "solutions" in result


class TestConjureTool:
    """Test conjure MCP tool"""

    @patch('mcp_server.grimoire')
    def test_conjure_spell(self, mock_grimoire):
        """Test conjure with spell string"""
        from mcp_server import conjure

        mock_grimoire.call_conjure_spell.return_value = ConjureResponse(
            result="ok(success)",
            spell="test_spell"
        )

        result = conjure("test_spell")

        mock_grimoire.call_conjure_spell.assert_called_once_with("test_spell")
        assert "result" in result


class TestSystemInfoTool:
    """Test system_info MCP tool"""

    @patch('mcp_server.grimoire')
    def test_system_info(self, mock_grimoire):
        """Test system_info tool"""
        from mcp_server import system_info
        from grimoire_client import SystemInfo

        mock_grimoire.get_system_info.return_value = SystemInfo(
            grimoire_root="/path/to/grimoire",
            janus_available=True,
            prolog_initialized=True,
            interface_version="0.1.0"
        )

        result = system_info()

        mock_grimoire.get_system_info.assert_called_once()
        assert "grimoire_root" in result


class TestEntitiesTool:
    """Test entities MCP tool"""

    @patch('mcp_server.grimoire')
    def test_entities(self, mock_grimoire):
        """Test entities tool"""
        from mcp_server import entities

        mock_grimoire.entities.return_value = EntitiesResponse(
            entities=["system", "git", "nix", "interface"]
        )

        result = entities()

        mock_grimoire.entities.assert_called_once()
        assert "system" in result
        assert "git" in result


class TestTestTool:
    """Test test MCP tool"""

    @patch('mcp_server.grimoire')
    def test_test_no_args(self, mock_grimoire):
        """Test test tool without arguments"""
        from mcp_server import test

        mock_grimoire.test.return_value = TestResponse(result="ok(tests_passed)")

        result = test()

        mock_grimoire.test.assert_called_once_with(None)
        assert "tests_passed" in result

    @patch('mcp_server.grimoire')
    def test_test_with_args(self, mock_grimoire):
        """Test test tool with arguments"""
        from mcp_server import test

        mock_grimoire.test.return_value = TestResponse(result="ok(tests_passed)")

        result = test(["test_name"])

        mock_grimoire.test.assert_called_once_with(["test_name"])


class TestSessionTool:
    """Test session MCP tool"""

    @patch('mcp_server.grimoire')
    def test_session(self, mock_grimoire):
        """Test session tool with args"""
        from mcp_server import session

        mock_grimoire.session.return_value = SessionCommandResponse(
            result="ok(session_created)"
        )

        result = session(["new", "test-session"])

        mock_grimoire.session.assert_called_once_with(["new", "test-session"])
        assert "session_created" in result


class TestLoadTool:
    """Test load MCP tool"""

    @patch('mcp_server.grimoire')
    def test_load(self, mock_grimoire):
        """Test load tool"""
        from mcp_server import load

        mock_grimoire.load.return_value = LoadResponse(entity="git")

        result = load("git")

        mock_grimoire.load.assert_called_once_with("git")
        assert "git" in result


class TestReadFileTool:
    """Test read_file MCP tool"""

    @patch('mcp_server.grimoire')
    def test_read_file_defaults(self, mock_grimoire):
        """Test read_file with default start and end"""
        from mcp_server import read_file

        mock_grimoire.read_file.return_value = ReadFileResponse(
            file_path="/test/file.txt",
            lines=[
                LineContent(line_number=1, content="line 1"),
                LineContent(line_number=2, content="line 2"),
            ]
        )

        result = read_file("/test/file.txt")

        mock_grimoire.read_file.assert_called_once_with("/test/file.txt", 1, 10)
        assert "1  line 1" in result
        assert "2  line 2" in result

    @patch('mcp_server.grimoire')
    def test_read_file_custom_range(self, mock_grimoire):
        """Test read_file with custom start and end"""
        from mcp_server import read_file

        mock_grimoire.read_file.return_value = ReadFileResponse(
            file_path="/test/file.txt",
            lines=[
                LineContent(line_number=5, content="line 5"),
                LineContent(line_number=6, content="line 6"),
            ]
        )

        result = read_file("/test/file.txt", start=5, end=6)

        mock_grimoire.read_file.assert_called_once_with("/test/file.txt", 5, 6)
        assert "5  line 5" in result

    @patch('mcp_server.grimoire')
    def test_read_file_format(self, mock_grimoire):
        """Test read_file output format is plain text not YAML"""
        from mcp_server import read_file

        mock_grimoire.read_file.return_value = ReadFileResponse(
            file_path="/test/file.txt",
            lines=[LineContent(line_number=1, content="test content")]
        )

        result = read_file("/test/file.txt")

        # Should NOT be YAML formatted
        assert not result.startswith("file_path:")
        # Should be plain text with line numbers
        assert result == "1  test content"


class TestEditFileTool:
    """Test edit_file MCP tool"""

    @patch('mcp_server.grimoire')
    def test_edit_file_insert(self, mock_grimoire):
        """Test edit_file with insert operation"""
        from mcp_server import edit_file

        mock_grimoire.edit_file.return_value = EditFileResponse(
            file_path="/test/file.txt",
            result="ok(file_edited)"
        )

        edits = [{"operation": "insert", "line": 5, "content": "new line"}]
        result = edit_file("/test/file.txt", edits)

        # Check that grimoire.edit_file was called
        mock_grimoire.edit_file.assert_called_once()
        call_args = mock_grimoire.edit_file.call_args[0]

        # Check file path
        assert call_args[0] == "/test/file.txt"

        # Check edits list has EditInsert
        assert len(call_args[1]) == 1
        assert isinstance(call_args[1][0], EditInsert)
        assert call_args[1][0].line == 5
        assert call_args[1][0].content == "new line"

    @patch('mcp_server.grimoire')
    def test_edit_file_delete(self, mock_grimoire):
        """Test edit_file with delete operation"""
        from mcp_server import edit_file

        mock_grimoire.edit_file.return_value = EditFileResponse(
            file_path="/test/file.txt",
            result="ok(file_edited)"
        )

        edits = [{"operation": "delete", "start_line": 5, "end_line": 10}]
        result = edit_file("/test/file.txt", edits)

        call_args = mock_grimoire.edit_file.call_args[0]
        assert isinstance(call_args[1][0], EditDelete)
        assert call_args[1][0].start_line == 5
        assert call_args[1][0].end_line == 10

    @patch('mcp_server.grimoire')
    def test_edit_file_replace(self, mock_grimoire):
        """Test edit_file with replace operation"""
        from mcp_server import edit_file

        mock_grimoire.edit_file.return_value = EditFileResponse(
            file_path="/test/file.txt",
            result="ok(file_edited)"
        )

        edits = [
            {
                "operation": "replace",
                "start_line": 5,
                "end_line": 7,
                "content": "replacement text"
            }
        ]
        result = edit_file("/test/file.txt", edits)

        call_args = mock_grimoire.edit_file.call_args[0]
        assert isinstance(call_args[1][0], EditReplace)
        assert call_args[1][0].start_line == 5
        assert call_args[1][0].end_line == 7
        assert call_args[1][0].content == "replacement text"

    @patch('mcp_server.grimoire')
    def test_edit_file_append(self, mock_grimoire):
        """Test edit_file with append operation"""
        from mcp_server import edit_file

        mock_grimoire.edit_file.return_value = EditFileResponse(
            file_path="/test/file.txt",
            result="ok(file_edited)"
        )

        edits = [{"operation": "append", "content": "appended text"}]
        result = edit_file("/test/file.txt", edits)

        call_args = mock_grimoire.edit_file.call_args[0]
        assert isinstance(call_args[1][0], EditAppend)
        assert call_args[1][0].content == "appended text"

    @patch('mcp_server.grimoire')
    def test_edit_file_multiple_operations(self, mock_grimoire):
        """Test edit_file with multiple operations"""
        from mcp_server import edit_file

        mock_grimoire.edit_file.return_value = EditFileResponse(
            file_path="/test/file.txt",
            result="ok(file_edited)"
        )

        edits = [
            {"operation": "insert", "line": 1, "content": "header"},
            {"operation": "delete", "start_line": 5, "end_line": 5},
            {"operation": "append", "content": "footer"}
        ]
        result = edit_file("/test/file.txt", edits)

        call_args = mock_grimoire.edit_file.call_args[0]
        assert len(call_args[1]) == 3
        assert isinstance(call_args[1][0], EditInsert)
        assert isinstance(call_args[1][1], EditDelete)
        assert isinstance(call_args[1][2], EditAppend)

    @patch('mcp_server.grimoire')
    def test_edit_file_invalid_operation_raises_error(self, mock_grimoire):
        """Test that invalid operations raise ValueError"""
        from mcp_server import edit_file

        edits = [{"operation": "invalid_op", "line": 1}]

        with pytest.raises(ValueError) as exc_info:
            edit_file("/test/file.txt", edits)

        assert "invalid operation" in str(exc_info.value).lower()

    @patch('mcp_server.grimoire')
    def test_edit_file_missing_operation_raises_error(self, mock_grimoire):
        """Test that missing operation field raises ValueError"""
        from mcp_server import edit_file

        edits = [{"line": 1, "content": "test"}]  # Missing 'operation'

        with pytest.raises(ValueError) as exc_info:
            edit_file("/test/file.txt", edits)

        assert "missing 'operation' field" in str(exc_info.value).lower()

    @patch('mcp_server.grimoire')
    def test_edit_file_empty_edits_raises_error(self, mock_grimoire):
        """Test that empty edits list raises ValueError"""
        from mcp_server import edit_file

        with pytest.raises(ValueError) as exc_info:
            edit_file("/test/file.txt", [])

        assert "no valid edits" in str(exc_info.value).lower()


class TestErrorHandling:
    """Test error handling in MCP tools"""

    @patch('mcp_server.grimoire')
    def test_grimoire_error_propagates(self, mock_grimoire):
        """Test that GrimoireError exceptions propagate to MCP framework"""
        from mcp_server import compt

        mock_grimoire.compt.side_effect = GrimoireError("Test error")

        with pytest.raises(GrimoireError):
            compt()

    @patch('mcp_server.grimoire')
    def test_edit_file_pydantic_validation_error(self, mock_grimoire):
        """Test that Pydantic validation errors propagate"""
        from mcp_server import edit_file
        from pydantic import ValidationError

        # Missing required field should cause validation error
        edits = [{"operation": "insert"}]  # Missing 'line' and 'content'

        # EditInsert requires line and content, so this should raise ValidationError
        with pytest.raises(ValidationError):
            edit_file("/test/file.txt", edits)

    @patch('mcp_server.grimoire')
    def test_read_file_error(self, mock_grimoire):
        """Test read_file error handling"""
        from mcp_server import read_file

        mock_grimoire.read_file.side_effect = GrimoireError("File not found")

        with pytest.raises(GrimoireError):
            read_file("/nonexistent/file.txt")


class TestMainFunction:
    """Test main entry point"""

    @patch('mcp_server.mcp')
    @patch('mcp_server.argparse.ArgumentParser')
    def test_main_default_transport(self, mock_parser_class, mock_mcp):
        """Test main function with default transport"""
        from mcp_server import main

        mock_parser = Mock()
        mock_args = Mock()
        mock_args.transport = "stdio"
        mock_parser.parse_args.return_value = mock_args
        mock_parser_class.return_value = mock_parser

        main()

        mock_mcp.run.assert_called_once_with(transport="stdio")

    @patch('mcp_server.mcp')
    @patch('mcp_server.argparse.ArgumentParser')
    def test_main_sse_transport(self, mock_parser_class, mock_mcp):
        """Test main function with SSE transport"""
        from mcp_server import main

        mock_parser = Mock()
        mock_args = Mock()
        mock_args.transport = "sse"
        mock_parser.parse_args.return_value = mock_args
        mock_parser_class.return_value = mock_parser

        main()

        mock_mcp.run.assert_called_once_with(transport="sse")


# Run with: pytest test_mcp_server.py -v
