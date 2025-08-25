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


class GrimoireError(RuntimeError):
    """Exception raised when Grimoire operations fail"""

    pass


# Response models for type-safe interface responses
class ComponentTypesResponse(BaseModel):
    """Response model for component types queries"""

    entity: str
    types: List[str]


class ComponentEntry(BaseModel):
    """Individual component entry with metadata"""

    component: Any
    flag: str  # 'entity' or 'value'


class ComponentsResponse(BaseModel):
    """Response model for component queries"""

    entity: str
    component_type: str
    components: List[ComponentEntry]


class DocumentationResponse(BaseModel):
    """Response model for documentation queries"""

    entity: str
    documentation: str


class StatusInfo(BaseModel):
    """Session status information"""

    current_branch: str
    working_status: str  # 'clean' or 'dirty'
    sessions: List[str]


class StatusResponse(BaseModel):
    """Response model for status queries"""

    status: StatusInfo


class PerceiveResponse(BaseModel):
    """Response model for perceive queries with variable bindings"""

    solutions: List[Dict[str, Any]]  # Variable bindings per solution
    count: int
    query: str


class ConjureResponse(BaseModel):
    """Response model for conjure spells"""

    result: Any
    spell: str


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

    entities: List[str]


class TestResponse(BaseModel):
    """Response model for test operations"""

    result: str


class SessionCommandResponse(BaseModel):
    """Response model for session command operations"""

    result: Any


class LoadResponse(BaseModel):
    """Response model for entity load operations"""

    entity: str


class RootResponse(BaseModel):
    """Response model for root endpoint"""

    api: str
    version: str
    description: str
    system_info: SystemInfo
    endpoints: List[InterfaceEndpoint]


class GrimoireInterface:
    """Interface to Grimoire Prolog system via janus-swi"""

    def __init__(self):
        self.janus = janus_swi

        # Get Grimoire root directory from environment variable
        grimoire_root_env = os.getenv("GRIMOIRE_ROOT")
        if not grimoire_root_env:
            raise GrimoireError(
                "GRIMOIRE_ROOT environment variable is not set. "
                "This should be set to the root directory of the Grimoire project."
            )

        self.grimoire_root = Path(grimoire_root_env)

        self.initialize_prolog()

    def query_interface_docstrings(self) -> Dict[str, str]:
        """Query all interface command docstrings from Prolog"""
        # Get interface subcommands using the working comp method
        comp_response = self.comp("interface", "subcommand")

        docstrings = {}
        for comp_entry in comp_response.components:
            subcmd = comp_entry.component
            doc_response = self.doc(f"interface({subcmd})")
            docstrings[subcmd] = doc_response.documentation

        return docstrings

    def _parse_python_dict_result(self, result_dict: Dict[str, Any]) -> Any:
        """Parse a Python dictionary result from Prolog into native types"""
        if not isinstance(result_dict, dict) or "type" not in result_dict:
            return result_dict

        result_type = result_dict["type"]

        if result_type == "atom":
            return result_dict["value"]
        elif result_type == "string":
            return result_dict["value"]
        elif result_type in ["int", "float"]:
            return result_dict["value"]
        elif result_type == "list":
            return [
                self._parse_python_dict_result(elem)
                for elem in result_dict.get("elements", [])
            ]
        elif result_type == "term_struct":
            functor = result_dict["functor"]
            args = [
                self._parse_python_dict_result(arg)
                for arg in result_dict.get("args", [])
            ]
            return {
                "functor": functor,
                "args": args,
                "arity": result_dict.get("arity", len(args)),
            }
        else:
            return result_dict.get("value", result_dict)

    def _term_to_string(self, term: Any) -> str:
        """Convert a parsed term structure to string representation"""
        if isinstance(term, dict) and term.get("functor"):
            functor = term["functor"]
            args = term.get("args", [])
            if not args:
                return functor
            else:
                arg_strs = [self._term_to_string(arg) for arg in args]
                return f"{functor}({', '.join(arg_strs)})"
        elif isinstance(term, str):
            return term
        else:
            return str(term)

    def _parse_interface_result(self, result_dict: Dict[str, Any]) -> Any:
        """Parse interface result and handle ok/error patterns"""
        parsed = self._parse_python_dict_result(result_dict)

        if isinstance(parsed, dict) and parsed.get("functor") == "ok":
            # Extract the success payload - ok/1 always has exactly one argument
            return parsed["args"][0]
        elif isinstance(parsed, dict) and parsed.get("functor") == "error":
            # Raise exception with error details - error/1 always has exactly one argument
            error_details = parsed["args"][0]
            raise GrimoireError(
                f"Grimoire operation failed: {error_details}", error_details
            )
        else:
            # Return as-is if not ok/error pattern
            return parsed

    def initialize_prolog(self) -> None:
        """Initialize Prolog and load Grimoire system"""
        # Set working directory to Grimoire root
        original_cwd = os.getcwd()
        try:
            os.chdir(self.grimoire_root)

            # Load the Grimoire system
            self.janus.query_once("ensure_loaded('src/grimoire.pl')")
            self.janus.query_once("ensure_loaded('src/interface/semantics.pl')")

        except Exception as e:
            raise GrimoireError(f"Failed to initialize Prolog: {e}") from e
        finally:
            # Always restore working directory
            os.chdir(original_cwd)

    def call_perceive_query(self, query_str: str) -> PerceiveResponse:
        """Call a perceive query and return all solutions with variable bindings"""
        # Use the interface layer with proper perceive validation
        query = "python_cast(conjure(interface(perceive(QueryTerm))), Result)"
        result = janus_swi.query_once(query, {"QueryTerm": query_str})

        if result and "Result" in result:
            parsed = self._parse_interface_result(result["Result"])
            # For perceive queries, we expect query results to be returned directly
            return PerceiveResponse(
                solutions=[parsed] if parsed is not None else [],
                count=1 if parsed is not None else 0,
                query=query_str,
            )

        raise GrimoireError("No results returned")

    def call_conjure_spell(self, spell_str: str) -> ConjureResponse:
        """Call a conjure spell and return result"""
        query = "python_cast(conjure(SpellStr), Result)"
        result = janus_swi.query_once(query, {"SpellStr": spell_str})

        if result and "Result" in result:
            parsed = self._parse_interface_result(result["Result"])
            return ConjureResponse(result=parsed, spell=spell_str)
        else:
            raise GrimoireError("Failed to execute spell")

    def test_prolog_connection(self) -> Dict[str, Any]:
        """Test basic Prolog functionality"""
        # Test basic Prolog functionality
        test_result = janus_swi.query_once("entity(system)")

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

    def system_instructions(self) -> str:
        """Get system instructions for MCP server initialization"""
        system_doc = self.doc("system").documentation
        return f"""{system_doc}

NOTE: This system operates on Prolog terms. Tools accept Prolog syntax as strings which are automatically converted to Prolog terms internally. Entity names can be atoms like 'system' or compound terms like 'nix(flake(template(python)))'."""

    # Type-safe interface methods that mirror semantics.pl

    def compt(self, entity: str = "system") -> ComponentTypesResponse:
        """List component types for entity"""
        if entity == "system":
            query = "python_cast(conjure(interface(compt)), Result)"
            result = janus_swi.query_once(query)
        else:
            query = "python_cast(conjure(interface(compt(Entity))), Result)"
            result = janus_swi.query_once(query, {"Entity": entity})

        if result and "Result" in result:
            parsed = self._parse_interface_result(result["Result"])
            if isinstance(parsed, dict) and parsed.get("functor") == "component_types":
                args = parsed.get("args", [])
                entity_name = args[0] if len(args) > 0 else entity
                types = args[1] if len(args) > 1 else []
                return ComponentTypesResponse(entity=entity_name, types=types)

        raise GrimoireError("Failed to retrieve component types")

    def comp(self, entity: str, component_type: str) -> ComponentsResponse:
        """List components of specific type for entity"""
        query = "python_cast(conjure(interface(comp(Entity, ComponentType))), Result)"
        result = janus_swi.query_once(
            query, {"Entity": entity, "ComponentType": component_type}
        )

        if result and "Result" in result:
            parsed = self._parse_interface_result(result["Result"])
            if isinstance(parsed, dict) and parsed.get("functor") == "components":
                args = parsed.get("args", [])
                entity_name = args[0] if len(args) > 0 else entity
                comp_type = args[1] if len(args) > 1 else component_type
                components_list = args[2] if len(args) > 2 else []

                components = []
                if isinstance(components_list, list):
                    for comp_entry in components_list:
                        if (
                            isinstance(comp_entry, dict)
                            and comp_entry.get("functor") == "comp_entry"
                        ):
                            args = comp_entry.get("args", [])
                            component = (
                                self._term_to_string(args[0])
                                if len(args) > 0
                                else str(comp_entry)
                            )
                            flag = str(args[1]) if len(args) > 1 else "value"
                            components.append(
                                ComponentEntry(component=component, flag=flag)
                            )
                        else:
                            # Fallback for non-comp_entry structure
                            components.append(
                                ComponentEntry(component=str(comp_entry), flag="value")
                            )

                return ComponentsResponse(
                    entity=entity_name,
                    component_type=comp_type,
                    components=components,
                )

        raise GrimoireError("Failed to retrieve components")

    def doc(self, entity: Any = "system") -> DocumentationResponse:
        """Get documentation for entity"""
        conjure_str = f"interface(doc({entity}))"
        result = self.call_conjure_spell(conjure_str)
        doc = result.result
        # doc = documentation(Entity, Doc)
        if isinstance(doc, dict) and doc.get("functor") == "documentation":
            args = doc["args"]
            if len(args) != 2:
                raise GrimoireError(f"Malformed documentation result: {doc}")
            doc_string = args[1]
            return DocumentationResponse(entity=entity, documentation=doc_string)
        else:
            raise GrimoireError(
                f"Failed to retrieve documentation for entity: {entity}"
            )

    def status(self) -> StatusResponse:
        """Get session status"""
        query = "python_cast(conjure(interface(status)), Result)"

        result = janus_swi.query_once(query)
        if result and "Result" in result:
            parsed = self._parse_interface_result(result["Result"])
            if isinstance(parsed, dict) and parsed.get("functor") == "session_status":
                args = parsed.get("args", [])
                status_info = args[0] if args else {}

                # Extract status information from the nested structure
                if isinstance(status_info, dict):
                    if status_info.get("functor") == "status_info":
                        status_args = status_info.get("args", [])
                        branch = str(status_args[0]) if len(status_args) > 0 else "main"
                        working = (
                            str(status_args[1]) if len(status_args) > 1 else "unknown"
                        )
                        sessions = status_args[2] if len(status_args) > 2 else ["main"]

                        # Convert sessions to list if needed
                        if isinstance(sessions, list):
                            sessions_list = [str(s) for s in sessions]
                        else:
                            sessions_list = [str(sessions)]

                        return StatusResponse(
                            status=StatusInfo(
                                current_branch=branch,
                                working_status=working,
                                sessions=sessions_list,
                            ),
                        )

        raise GrimoireError("Failed to retrieve session status")

    def entities(self) -> EntitiesResponse:
        """List all entities in the system"""
        query = "python_cast(conjure(interface(entities)), Result)"

        result = janus_swi.query_once(query)
        if result and "Result" in result:
            parsed = self._parse_interface_result(result["Result"])
            if isinstance(parsed, dict) and parsed.get("functor") == "entities":
                args = parsed.get("args", [])
                entities_list = args[0] if args else []
                # Convert each entity to string representation
                if isinstance(entities_list, list):
                    entity_strings = [
                        self._term_to_string(entity) for entity in entities_list
                    ]
                else:
                    entity_strings = []
                return EntitiesResponse(entities=entity_strings)

        raise GrimoireError("Failed to retrieve entities")

    def test(self, args: Optional[List[str]] = None) -> TestResponse:
        """Run test suite"""
        if args:
            query = "python_cast(conjure(interface(test(Args))), Result)"
            result = janus_swi.query_once(query, {"Args": args})
        else:
            query = "python_cast(conjure(interface(test)), Result)"
            result = janus_swi.query_once(query)

        if result and "Result" in result:
            parsed = self._parse_interface_result(result["Result"])
            return TestResponse(result=str(parsed))
        else:
            raise GrimoireError("Failed to run tests")

    def session(self, args: List[str]) -> SessionCommandResponse:
        """Execute session command"""
        query = "python_cast(conjure(interface(session(Args))), Result)"
        result = janus_swi.query_once(query, {"Args": args})

        if result and "Result" in result:
            parsed = self._parse_interface_result(result["Result"])
            return SessionCommandResponse(result=parsed)
        else:
            raise GrimoireError("Failed to execute session command")

    def load(self, entity_spec: str) -> LoadResponse:
        """Load entity into current session"""
        query = "python_cast(conjure(interface(load(EntitySpec))), Result)"
        result = janus_swi.query_once(query, {"EntitySpec": entity_spec})

        if result and "Result" in result:
            parsed = self._parse_interface_result(result["Result"])
            # Extract entity name from parsed result
            if isinstance(parsed, dict) and parsed.get("functor") == "entity_loaded":
                args = parsed.get("args", [])
                entity = str(args[0]) if args else entity_spec
            else:
                entity = entity_spec

            return LoadResponse(entity=entity)
        else:
            raise GrimoireError(f"Failed to load entity {entity_spec}")
