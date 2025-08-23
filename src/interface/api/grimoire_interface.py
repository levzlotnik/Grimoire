"""
Grimoire Interface Module

Provides Python interface to Grimoire Prolog system using janus-swi.
Handles all Prolog integration, query execution, and result formatting.
"""

import os
import sys
from pathlib import Path
from typing import Dict, List, Any, Optional


class GrimoireInterface:
    """Interface to Grimoire Prolog system via janus-swi"""
    
    def __init__(self):
        self.prolog_initialized = False
        
        # Get Grimoire root directory
        api_dir = Path(__file__).parent
        self.grimoire_root = api_dir.parent.parent.parent
        
        # Try to import janus
        try:
            import janus_swi as janus
            self.janus = janus
            self.janus_available = True
        except ImportError:
            self.janus = None
            self.janus_available = False
        
        if self.janus_available:
            self.initialize_prolog()
    
    def initialize_prolog(self) -> bool:
        """Initialize Prolog and load Grimoire system"""
        if not self.janus_available:
            print("janus-swi not available")
            return False
            
        try:
            # Set working directory to Grimoire root
            original_cwd = os.getcwd()
            os.chdir(str(self.grimoire_root))
            
            # Load the Grimoire system
            self.janus.query_once("ensure_loaded('src/grimoire.pl')")
            self.janus.query_once("ensure_loaded('src/interface/semantics.pl')")
            
            # Restore working directory
            os.chdir(original_cwd)
            
            self.prolog_initialized = True
            return True
            
        except Exception as e:
            print(f"Failed to initialize Prolog: {e}")
            if 'original_cwd' in locals():
                os.chdir(original_cwd)
            return False
    
    def is_available(self) -> bool:
        """Check if Prolog system is available and initialized"""
        return self.janus_available and self.prolog_initialized
    
    def _execute_in_grimoire_context(self, query: str) -> Optional[Dict[str, Any]]:
        """Execute a Prolog query in Grimoire context"""
        if not self.is_available():
            raise RuntimeError("Prolog system not available")
        
        original_cwd = os.getcwd()
        try:
            os.chdir(str(self.grimoire_root))
            result = self.janus.query_once(query)
            return result
        finally:
            os.chdir(original_cwd)
    
    def _execute_query_all_solutions(self, query: str) -> List[Dict[str, Any]]:
        """Execute a Prolog query and return all solutions"""
        if not self.is_available():
            raise RuntimeError("Prolog system not available")
        
        original_cwd = os.getcwd()
        try:
            os.chdir(str(self.grimoire_root))
            solutions = list(self.janus.query(query))
            return solutions
        finally:
            os.chdir(original_cwd)
    
    def call_interface_predicate(self, predicate: str, args: Optional[List[Any]] = None) -> Dict[str, Any]:
        """Call an interface predicate and return result"""
        if not self.is_available():
            return {
                "success": False,
                "error": "Prolog system not initialized or janus-swi not available"
            }
        
        try:
            # Build the conjure query
            if args:
                # Format arguments as Prolog terms
                formatted_args = []
                for arg in args:
                    if isinstance(arg, (int, float)):
                        formatted_args.append(str(arg))
                    elif isinstance(arg, str):
                        # Escape quotes and wrap in quotes
                        escaped = arg.replace("'", "\\'")
                        formatted_args.append(f"'{escaped}'")
                    else:
                        formatted_args.append(str(arg))
                
                args_str = ', '.join(formatted_args)
                query = f"cast(conjure(interface({predicate}({args_str}))), Result)"
            else:
                query = f"cast(conjure(interface({predicate})), Result)"
            
            # Execute the query
            result = self._execute_in_grimoire_context(query)
            
            if result:
                return {
                    "success": True,
                    "result": result.get("Result", result),
                    "prolog_result": result
                }
            else:
                return {
                    "success": False,
                    "error": "Query failed or returned no results"
                }
                
        except Exception as e:
            return {
                "success": False,
                "error": str(e)
            }
    
    def call_perceive_query(self, query_str: str) -> Dict[str, Any]:
        """Call a perceive query and return all solutions"""
        if not self.is_available():
            return {
                "success": False,
                "error": "Prolog system not initialized or janus-swi not available"
            }
        
        try:
            # Execute perceive query and collect all solutions
            query = f"perceive({query_str})"
            solutions = self._execute_query_all_solutions(query)
            
            return {
                "success": True,
                "solutions": solutions,
                "count": len(solutions)
            }
                
        except Exception as e:
            return {
                "success": False,
                "error": str(e)
            }
    
    def call_conjure_spell(self, spell_str: str) -> Dict[str, Any]:
        """Call a conjure spell and return result"""
        if not self.is_available():
            return {
                "success": False,
                "error": "Prolog system not initialized or janus-swi not available"
            }
        
        try:
            # Execute conjure spell
            query = f"cast(conjure({spell_str}), Result)"
            result = self._execute_in_grimoire_context(query)
            
            if result:
                return {
                    "success": True,
                    "result": result.get("Result", result),
                    "prolog_result": result
                }
            else:
                return {
                    "success": False,
                    "error": "Conjure spell failed or returned no results"
                }
                
        except Exception as e:
            return {
                "success": False,
                "error": str(e)
            }
    
    def test_prolog_connection(self) -> Dict[str, Any]:
        """Test basic Prolog functionality"""
        try:
            if not self.janus_available:
                return {
                    "janus_available": False,
                    "error": "janus-swi not installed"
                }
            
            # Test basic Prolog functionality
            test_result = self._execute_in_grimoire_context("X = test")
            
            return {
                "janus_available": True,
                "prolog_initialized": self.prolog_initialized,
                "prolog_test": test_result is not None,
                "test_result": test_result
            }
        except Exception as e:
            return {
                "janus_available": self.janus_available,
                "prolog_initialized": self.prolog_initialized,
                "error": str(e)
            }
    
    def get_system_info(self) -> Dict[str, Any]:
        """Get information about the Grimoire interface system"""
        return {
            "grimoire_root": str(self.grimoire_root),
            "janus_available": self.janus_available,
            "prolog_initialized": self.prolog_initialized,
            "interface_version": "0.1.0"
        }