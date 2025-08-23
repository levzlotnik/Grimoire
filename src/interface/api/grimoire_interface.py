"""
Grimoire Interface Module

Provides Python interface to Grimoire Prolog system using janus-swi.
Handles all Prolog integration, query execution, and result formatting.
"""

import os
import sys
from pathlib import Path
from typing import Dict, List, Any, Optional
from pydantic import BaseModel
import janus_swi


# Response models for type-safe interface responses
class ComponentTypesResponse(BaseModel):
    """Response model for component types queries"""

    success: bool
    entity: str
    types: List[str]
    error: Optional[str] = None


class ComponentEntry(BaseModel):
    """Individual component entry with metadata"""

    component: Any
    flag: str  # 'entity' or 'value'


class ComponentsResponse(BaseModel):
    """Response model for component queries"""

    success: bool
    entity: str
    component_type: str
    components: List[ComponentEntry]
    error: Optional[str] = None


class DocumentationResponse(BaseModel):
    """Response model for documentation queries"""

    success: bool
    entity: str
    documentation: str
    error: Optional[str] = None


class StatusInfo(BaseModel):
    """Session status information"""

    current_branch: str
    working_status: str  # 'clean' or 'dirty'
    sessions: List[str]


class StatusResponse(BaseModel):
    """Response model for status queries"""

    success: bool
    status: StatusInfo
    error: Optional[str] = None


class PerceiveResponse(BaseModel):
    """Response model for perceive queries with variable bindings"""

    success: bool
    solutions: List[Dict[str, Any]]  # Variable bindings per solution
    count: int
    query: str
    error: Optional[str] = None


class ConjureResponse(BaseModel):
    """Response model for conjure spells"""

    success: bool
    result: Any
    spell: str
    error: Optional[str] = None


class InterfaceEndpoint(BaseModel):
    """Metadata for an interface endpoint"""

    method: str
    path: str
    description: str


class SystemInfo(BaseModel):
    """System information"""

    grimoire_root: str
    janus_available: bool
    prolog_initialized: bool
    interface_version: str


class EntitiesResponse(BaseModel):
    """Response model for entities queries"""

    success: bool
    entities: List[str]
    error: Optional[str] = None


class TestResponse(BaseModel):
    """Response model for test operations"""

    success: bool
    result: str
    error: Optional[str] = None


class SessionCommandResponse(BaseModel):
    """Response model for session command operations"""

    success: bool
    result: Any
    error: Optional[str] = None


class LoadResponse(BaseModel):
    """Response model for entity load operations"""

    success: bool
    entity: str
    error: Optional[str] = None


class RootResponse(BaseModel):
    """Response model for root endpoint"""

    success: bool
    api: str
    version: str
    description: str
    system_info: SystemInfo
    endpoints: List[InterfaceEndpoint]


class GrimoireInterface:
    """Interface to Grimoire Prolog system via janus-swi"""

    def __init__(self):
        self.janus = janus_swi

        # Get Grimoire root directory - handle both dev and nix store paths
        api_dir = Path(__file__).parent
        potential_root = api_dir.parent.parent.parent

        # Check if we're in nix store and need to find actual grimoire root
        if "/nix/store" in str(potential_root):
            # In nix environment, the grimoire root should be the current working directory
            # when the server is properly launched
            self.grimoire_root = Path.cwd()
        else:
            # In development, use the calculated path
            self.grimoire_root = potential_root

        self.initialize_prolog()

    def query_interface_docstrings(self) -> Dict[str, str]:
        """Query all interface command docstrings from Prolog"""
        try:
            # Query all interface subcommands and their docstrings
            query = "findall([SubCmd, Doc], (component(interface, subcommand, SubCmd), docstring(interface(SubCmd), Doc)), Results)"
            result = self._execute_in_grimoire_context(query)

            docstrings = {}
            if result and "Results" in result:
                for pair in result["Results"]:
                    if len(pair) == 2:
                        subcmd, doc = pair
                        docstrings[subcmd] = doc

            return docstrings
        except Exception as e:
            # Return empty dict on error - don't crash the whole system
            return {}

    def initialize_prolog(self) -> None:
        """Initialize Prolog and load Grimoire system"""
        # Set working directory to Grimoire root
        original_cwd = os.getcwd()
        try:
            os.chdir(str(self.grimoire_root))

            # Load the Grimoire system
            self.janus.query_once("ensure_loaded('src/grimoire.pl')")
            self.janus.query_once("ensure_loaded('src/interface/semantics.pl')")

        except Exception as e:
            raise RuntimeError(f"Failed to initialize Prolog: {e}") from e
        finally:
            # Always restore working directory
            os.chdir(original_cwd)

    def _execute_in_grimoire_context(self, query: str) -> Optional[Dict[str, Any]]:
        """Execute a Prolog query in Grimoire context"""

        original_cwd = os.getcwd()
        try:
            os.chdir(str(self.grimoire_root))
            result = self.janus.query_once(query)
            return result
        finally:
            os.chdir(original_cwd)

    def _execute_query_all_solutions(self, query: str) -> List[Dict[str, Any]]:
        """Execute a Prolog query and return all solutions"""

        original_cwd = os.getcwd()
        try:
            os.chdir(str(self.grimoire_root))
            solutions = list(self.janus.query(query))
            return solutions
        finally:
            os.chdir(original_cwd)

    def _call_interface_predicate(
        self, predicate: str, args: Optional[List[Any]] = None
    ) -> Dict[str, Any]:
        """Call an interface predicate and return result"""

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

                args_str = ", ".join(formatted_args)
                query = f"cast(conjure(interface({predicate}({args_str}))), Result)"
            else:
                query = f"cast(conjure(interface({predicate})), Result)"

            # Execute the query
            result = self._execute_in_grimoire_context(query)

            if result:
                return {
                    "success": True,
                    "result": result.get("Result", result),
                    "prolog_result": result,
                }
            else:
                return {
                    "success": False,
                    "error": "Query failed or returned no results",
                }

        except Exception as e:
            return {"success": False, "error": str(e)}

    def call_perceive_query(self, query_str: str) -> PerceiveResponse:
        """Call a perceive query and return all solutions with variable bindings"""
        try:
            # Execute perceive query and collect all solutions
            query = f"perceive({query_str})"
            solutions = self._execute_query_all_solutions(query)

            return PerceiveResponse(
                success=True, solutions=solutions, count=len(solutions), query=query_str
            )

        except Exception as e:
            return PerceiveResponse(
                success=False, solutions=[], count=0, query=query_str, error=str(e)
            )

    def call_conjure_spell(self, spell_str: str) -> ConjureResponse:
        """Call a conjure spell and return result"""
        try:
            # Execute conjure spell
            query = f"cast(conjure({spell_str}), Result)"
            result = self._execute_in_grimoire_context(query)

            if result:
                return ConjureResponse(
                    success=True, result=result.get("Result", result), spell=spell_str
                )
            else:
                return ConjureResponse(
                    success=False,
                    result=None,
                    spell=spell_str,
                    error="Conjure spell failed or returned no results",
                )

        except Exception as e:
            return ConjureResponse(
                success=False, result=None, spell=spell_str, error=str(e)
            )

    def test_prolog_connection(self) -> Dict[str, Any]:
        """Test basic Prolog functionality"""
        # Test basic Prolog functionality
        test_result = self._execute_in_grimoire_context("X = test")

        return {
            "janus_available": True,
            "prolog_initialized": True,
            "prolog_test": test_result is not None,
            "test_result": test_result,
        }

    def get_system_info(self) -> SystemInfo:
        """Get information about the Grimoire interface system"""
        return SystemInfo(
            grimoire_root=str(self.grimoire_root),
            janus_available=True,
            prolog_initialized=True,
            interface_version="0.1.0",
        )

    # Type-safe interface methods that mirror semantics.pl
    
    def compt(self, entity: str = "system") -> ComponentTypesResponse:
        """List component types for entity"""
        query = f"interface_compt('{entity}', Types)"
        result = self._execute_in_grimoire_context(query)
        
        if result and 'Types' in result:
            return ComponentTypesResponse(
                success=True,
                entity=entity,
                types=result['Types']
            )
        else:
            return ComponentTypesResponse(
                success=False,
                entity=entity,
                types=[],
                error="Failed to query component types"
            )
    
    def comp(self, entity: str, component_type: str) -> ComponentsResponse:
        """List components of specific type for entity"""
        # Query for each component individually to avoid complex terms
        query = f"component('{entity}', '{component_type}', Comp)"
        solutions = self._execute_query_all_solutions(query)
        
        components = []
        for solution in solutions:
            if 'Comp' in solution:
                components.append(ComponentEntry(
                    component=str(solution['Comp']),
                    flag="value"  # Default flag since we're querying directly
                ))
        
        return ComponentsResponse(
            success=True,
            entity=entity,
            component_type=component_type,
            components=components
        )
    
    def doc(self, entity: str = "system") -> DocumentationResponse:
        """Get documentation for entity"""
        query = f"interface_doc('{entity}', Doc)"
        result = self._execute_in_grimoire_context(query)
        
        if result and 'Doc' in result:
            return DocumentationResponse(
                success=True,
                entity=entity,
                documentation=result['Doc']
            )
        else:
            return DocumentationResponse(
                success=False,
                entity=entity,
                documentation="",
                error="Failed to query documentation"
            )
    
    def status(self) -> StatusResponse:
        """Get session status"""
        query = "get_session_status(status_info(Branch, Working, Sessions))"
        result = self._execute_in_grimoire_context(query)
        
        if result and 'Branch' in result:
            branch = str(result['Branch'])
            working = str(result['Working'])
            sessions = result['Sessions']
            
            # Convert sessions to list if it's a Prolog list
            if hasattr(sessions, '__iter__') and not isinstance(sessions, str):
                sessions_list = [str(s) for s in sessions]
            else:
                sessions_list = [str(sessions)]
            
            return StatusResponse(
                success=True,
                status=StatusInfo(
                    current_branch=branch,
                    working_status=working,
                    sessions=sessions_list
                )
            )
        else:
            return StatusResponse(
                success=False,
                status=StatusInfo(
                    current_branch="main",
                    working_status="unknown", 
                    sessions=["main"]
                ),
                error="Failed to query session status"
            )
    
    def entities(self) -> EntitiesResponse:
        """List all entities in the system"""
        query = "interface_entities(Entities)"
        result = self._execute_in_grimoire_context(query)
        
        if result and 'Entities' in result:
            return EntitiesResponse(
                success=True,
                entities=result['Entities']
            )
        else:
            return EntitiesResponse(
                success=False,
                entities=[],
                error="Failed to query entities"
            )
    
    def test(self, args: Optional[List[str]] = None) -> TestResponse:
        """Run test suite"""
        if args:
            query = f"cast(conjure(interface(test({args}))), Result)"
        else:
            query = "cast(conjure(interface(test)), Result)"
        
        result = self._execute_in_grimoire_context(query)
        
        if result and 'Result' in result:
            return TestResponse(
                success=True,
                result=str(result['Result'])
            )
        else:
            return TestResponse(
                success=False,
                result="",
                error="Failed to run tests"
            )
    
    def session(self, args: List[str]) -> SessionCommandResponse:
        """Execute session command"""
        # Format args as Prolog list
        formatted_args = [f"'{arg}'" for arg in args]
        args_str = f"[{', '.join(formatted_args)}]"
        query = f"cast(conjure(interface(session({args_str}))), Result)"
        
        result = self._execute_in_grimoire_context(query)
        
        if result and 'Result' in result:
            return SessionCommandResponse(
                success=True,
                result=result['Result']
            )
        else:
            return SessionCommandResponse(
                success=False,
                result=None,
                error="Failed to execute session command"
            )
    
    def load(self, entity_spec: str) -> LoadResponse:
        """Load entity into current session"""
        query = f"cast(conjure(interface(load('{entity_spec}'))), Result)"
        
        result = self._execute_in_grimoire_context(query)
        
        if result and 'Result' in result:
            # Extract entity name from result
            result_val = result['Result']
            if isinstance(result_val, dict) and 'entity' in result_val:
                entity = result_val['entity']
            else:
                entity = entity_spec
            
            return LoadResponse(
                success=True,
                entity=entity
            )
        else:
            return LoadResponse(
                success=False,
                entity="",
                error=f"Failed to load entity {entity_spec}"
            )
