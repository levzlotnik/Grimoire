"""
Tests for GrimoireInterface module
"""

import pytest
from unittest.mock import Mock, patch, MagicMock
from pathlib import Path
import sys
import os

# Add the parent directory to the path so we can import our modules
sys.path.insert(0, str(Path(__file__).parent.parent))

from grimoire_interface import GrimoireInterface


class TestGrimoireInterface:
    """Test cases for GrimoireInterface class"""
    
    def setup_method(self):
        """Set up test fixtures"""
        self.interface = None
    
    def teardown_method(self):
        """Clean up after tests"""
        if self.interface:
            del self.interface
    
    @patch('grimoire_interface.janus_swi')
    def test_initialization_with_janus_available(self, mock_janus):
        """Test initialization when janus-swi is available"""
        mock_janus.query_once.return_value = True
        
        interface = GrimoireInterface()
        
        self.assertTrue(interface.janus_available)
        self.assertEqual(interface.janus, mock_janus)
        # Should have tried to initialize prolog
        self.assertTrue(mock_janus.query_once.called)
    
    @patch('grimoire_interface.janus_swi', side_effect=ImportError("No module named 'janus_swi'"))
    def test_initialization_without_janus(self, mock_import):
        """Test initialization when janus-swi is not available"""
        interface = GrimoireInterface()
        
        self.assertFalse(interface.janus_available)
        self.assertIsNone(interface.janus)
        self.assertFalse(interface.prolog_initialized)
    
    def test_is_available_when_not_initialized(self):
        """Test is_available returns False when not initialized"""
        with patch('grimoire_interface.janus_swi', side_effect=ImportError()):
            interface = GrimoireInterface()
            self.assertFalse(interface.is_available())
    
    @patch('grimoire_interface.janus_swi')
    def test_is_available_when_initialized(self, mock_janus):
        """Test is_available returns True when properly initialized"""
        mock_janus.query_once.return_value = True
        
        interface = GrimoireInterface()
        interface.prolog_initialized = True
        
        self.assertTrue(interface.is_available())
    
    @patch('grimoire_interface.janus_swi')
    @patch('os.chdir')
    @patch('os.getcwd')
    def test_execute_in_grimoire_context(self, mock_getcwd, mock_chdir, mock_janus):
        """Test _execute_in_grimoire_context method"""
        mock_getcwd.return_value = '/original/dir'
        mock_janus.query_once.return_value = {'X': 'test_result'}
        
        interface = GrimoireInterface()
        interface.prolog_initialized = True
        
        result = interface._execute_in_grimoire_context("X = test")
        
        self.assertEqual(result, {'X': 'test_result'})
        # Should have changed directory and restored it
        self.assertTrue(mock_chdir.called)
    
    @patch('grimoire_interface.janus_swi')
    def test_call_interface_predicate_not_available(self, mock_janus):
        """Test call_interface_predicate when system not available"""
        interface = GrimoireInterface()
        interface.janus_available = False
        
        result = interface.call_interface_predicate("test")
        
        self.assertFalse(result["success"])
        self.assertIn("not initialized", result["error"])
    
    @patch('grimoire_interface.janus_swi')
    def test_call_interface_predicate_with_args(self, mock_janus):
        """Test call_interface_predicate with arguments"""
        mock_janus.query_once.return_value = {'Result': 'success'}
        
        interface = GrimoireInterface()
        interface.prolog_initialized = True
        
        with patch.object(interface, '_execute_in_grimoire_context') as mock_execute:
            mock_execute.return_value = {'Result': 'success'}
            
            result = interface.call_interface_predicate("test", ["arg1", 42])
            
            self.assertTrue(result["success"])
            self.assertEqual(result["result"], "success")
            # Check that the query was formatted correctly
            mock_execute.assert_called_once()
            call_args = mock_execute.call_args[0][0]
            self.assertIn("interface(test('arg1', 42))", call_args)
    
    @patch('grimoire_interface.janus_swi')
    def test_call_perceive_query(self, mock_janus):
        """Test call_perceive_query method"""
        mock_solutions = [{'X': '1'}, {'X': '2'}]
        
        interface = GrimoireInterface()
        interface.prolog_initialized = True
        
        with patch.object(interface, '_execute_query_all_solutions') as mock_execute:
            mock_execute.return_value = mock_solutions
            
            result = interface.call_perceive_query("X = Y")
            
            self.assertTrue(result["success"])
            self.assertEqual(result["solutions"], mock_solutions)
            self.assertEqual(result["count"], 2)
    
    @patch('grimoire_interface.janus_swi')
    def test_call_conjure_spell(self, mock_janus):
        """Test call_conjure_spell method"""
        mock_janus.query_once.return_value = {'Result': 'spell_result'}
        
        interface = GrimoireInterface()
        interface.prolog_initialized = True
        
        with patch.object(interface, '_execute_in_grimoire_context') as mock_execute:
            mock_execute.return_value = {'Result': 'spell_result'}
            
            result = interface.call_conjure_spell("test_spell")
            
            self.assertTrue(result["success"])
            self.assertEqual(result["result"], "spell_result")
            mock_execute.assert_called_once_with("cast(conjure(test_spell), Result)")
    
    @patch('grimoire_interface.janus_swi')
    def test_test_prolog_connection(self, mock_janus):
        """Test test_prolog_connection method"""
        interface = GrimoireInterface()
        interface.prolog_initialized = True
        
        with patch.object(interface, '_execute_in_grimoire_context') as mock_execute:
            mock_execute.return_value = {'X': 'test'}
            
            result = interface.test_prolog_connection()
            
            self.assertTrue(result["janus_available"])
            self.assertTrue(result["prolog_initialized"])
            self.assertTrue(result["prolog_test"])
    
    @patch('grimoire_interface.janus_swi')
    def test_get_system_info(self, mock_janus):
        """Test get_system_info method"""
        interface = GrimoireInterface()
        
        result = interface.get_system_info()
        
        self.assertIn("grimoire_root", result)
        self.assertIn("janus_available", result)
        self.assertIn("prolog_initialized", result)
        self.assertIn("interface_version", result)
        self.assertEqual(result["interface_version"], "0.1.0")
    
    @patch('grimoire_interface.janus_swi')
    def test_error_handling_in_predicate_call(self, mock_janus):
        """Test error handling in call_interface_predicate"""
        interface = GrimoireInterface()
        interface.prolog_initialized = True
        
        with patch.object(interface, '_execute_in_grimoire_context') as mock_execute:
            mock_execute.side_effect = Exception("Prolog error")
            
            result = interface.call_interface_predicate("test")
            
            self.assertFalse(result["success"])
            self.assertIn("Prolog error", result["error"])


# Run with: pytest test_grimoire_interface.py
