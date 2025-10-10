"""
Grimoire Client Module

Provides Python client interface to Grimoire Prolog system using janus-swi.
Handles all Prolog integration, query execution, and result formatting.
"""

import os
import sys
from pathlib import Path
from typing import Dict, List, Any, Optional, Union, Callable
from pydantic import BaseModel
from dataclasses import dataclass
import janus_swi


class GrimoireError(RuntimeError):
    """Exception raised when Grimoire operations fail"""


@dataclass
class GrimoireToolset:
    """
    Toolset for AI agent frameworks containing Grimoire interface methods.
    
    This provides everything an AI agent needs to interact with Grimoire:
    - Callable tools (methods)
    - System prompt with instructions
    - Tool descriptions/documentation
    """
    tools: List[Callable]
    system_prompt: str
    tool_descriptions: Dict[str, str]


class PrologTerm:
    """Type-safe representation of a Prolog compound term"""
    
    def __init__(self, functor: str, args: Optional[List[Any]] = None):
        self.functor = functor
        self.args = args or []
        self.arity = len(self.args)
    
    def __str__(self) -> str:
        """Convert to Prolog string representation"""
        if not self.args:
            return self.functor
        else:
            arg_strs = []
            for arg in self.args:
                if isinstance(arg, PrologTerm):
                    arg_strs.append(str(arg))
                elif isinstance(arg, str):
                    # Check if it needs quoting (contains spaces, special chars, or starts with uppercase)
                    if ' ' in arg or '(' in arg or ')' in arg or (arg and arg[0].isupper()):
                        arg_strs.append(f"'{arg}'")
                    else:
                        arg_strs.append(arg)
                else:
                    arg_strs.append(str(arg))
            return f"{self.functor}({', '.join(arg_strs)})"
    
    def __repr__(self) -> str:
        return f"PrologTerm(functor={self.functor!r}, args={self.args!r})"
    
    @classmethod
    def from_dict(cls, d: Dict[str, Any]) -> 'PrologTerm':
        """Create PrologTerm from parsed dictionary structure"""
        if not isinstance(d, dict) or not d.get("functor"):
            raise ValueError(f"Invalid term dictionary: {d}")
        
        args = []
        for arg in d.get("args", []):
            if isinstance(arg, dict) and arg.get("functor"):
                args.append(cls.from_dict(arg))
            else:
                args.append(arg)
        
        return cls(d["functor"], args)


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

    result: str  # String representation of result
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

    result: str  # String representation of result


class LoadResponse(BaseModel):
    """Response model for entity load operations"""

    entity: str


class LineContent(BaseModel):
    """Individual line with line number"""
    
    line_number: int
    content: str


class ReadFileResponse(BaseModel):
    """Response model for read_file operations"""

    file_path: str
    lines: List[LineContent]


class EditInsert(BaseModel):
    """Insert text at a specific line"""
    operation: str = "insert"
    line: int
    content: str


class EditDelete(BaseModel):
    """Delete lines from a file"""
    operation: str = "delete"
    start_line: int
    end_line: int


class EditReplace(BaseModel):
    """Replace a range of lines with new text"""
    operation: str = "replace"
    start_line: int
    end_line: int
    content: str


class EditAppend(BaseModel):
    """Append text to the end of file"""
    operation: str = "append"
    content: str


# Sum type for edit operations
EditOperation = Union[EditInsert, EditDelete, EditReplace, EditAppend]


class EditFileResponse(BaseModel):
    """Response model for edit_file operations"""

    file_path: str
    result: str  # Success message or result


class RootResponse(BaseModel):
    """Response model for root endpoint"""

    api: str
    version: str
    description: str
    system_info: SystemInfo
    endpoints: List[InterfaceEndpoint]


class GrimoireClient:
    """Client interface to Grimoire Prolog system via janus-swi"""

    def __init__(self, skip_prolog_init: bool = False):
        self.janus = janus_swi

        # Get Grimoire root directory from environment variable
        grimoire_root_env = os.getenv("GRIMOIRE_ROOT")
        if not grimoire_root_env:
            raise GrimoireError(
                "GRIMOIRE_ROOT environment variable is not set. "
                "This should be set to the root directory of the Grimoire project."
            )

        self.grimoire_root = Path(grimoire_root_env)

        if not skip_prolog_init:
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
        """Parse a Python dictionary result from Prolog into native types or PrologTerm"""
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
            return PrologTerm(functor, args)
        else:
            return result_dict.get("value", result_dict)

    def _term_to_string(self, term: Any) -> str:
        """Convert a parsed term or PrologTerm to string representation"""
        if isinstance(term, PrologTerm):
            return str(term)
        elif isinstance(term, dict) and term.get("functor"):
            # Legacy support for dict representation
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

        if isinstance(parsed, PrologTerm) and parsed.functor == "ok":
            # Extract the success payload - ok/1 always has exactly one argument
            return parsed.args[0] if parsed.args else None
        elif isinstance(parsed, PrologTerm) and parsed.functor == "error":
            # Raise exception with error details - error/1 always has exactly one argument
            error_struct = parsed.args[0] if parsed.args else parsed
            error_details = self._term_to_string(error_struct)
            raise GrimoireError(
                f"Grimoire operation failed: {error_details}", error_details
            )
        else:
            # Return as-is if not ok/error pattern
            return parsed

    def initialize_prolog(self) -> None:
        """Initialize Prolog and load Grimoire system"""
        try:
            # Use absolute paths based on GRIMOIRE_ROOT
            grimoire_pl = self.grimoire_root / "src" / "grimoire.pl"
            interface_pl = self.grimoire_root / "src" / "interface" / "semantics.pl"
            
            # Load the Grimoire system with absolute paths
            self.janus.query_once(f"ensure_loaded('{grimoire_pl}')")
            self.janus.query_once(f"ensure_loaded('{interface_pl}')")

        except Exception as e:
            raise GrimoireError(f"Failed to initialize Prolog: {e}") from e

    def call_perceive_query(self, query_str: str) -> PerceiveResponse:
        """Call a perceive query and return all solutions with variable bindings"""
        # Use the interface layer with proper perceive validation
        query = "python_cast(conjure(interface(perceive(QueryTerm))), Result)"
        result = janus_swi.query_once(query, {"QueryTerm": query_str})

        if result and "Result" in result:
            parsed = self._parse_interface_result(result["Result"])
            # Convert any compound terms in the solution to string representation
            if parsed is not None:
                if isinstance(parsed, dict) and not isinstance(parsed, PrologTerm):
                    # If it's a dict with variable bindings, convert compound terms to strings
                    solution = {}
                    for key, value in parsed.items():
                        solution[key] = self._term_to_string(value)
                    solutions = [solution]
                else:
                    # Single value result (could be a PrologTerm or primitive)
                    solutions = [{"result": self._term_to_string(parsed)}]
            else:
                solutions = []
            
            return PerceiveResponse(
                solutions=solutions,
                count=len(solutions),
                query=query_str,
            )

        raise GrimoireError("No results returned")

    def call_conjure_spell(self, spell_str: str) -> ConjureResponse:
        """Call a conjure spell and return result"""
        query = "python_cast(conjure(SpellStr), Result)"
        result = janus_swi.query_once(query, {"SpellStr": spell_str})

        if result and "Result" in result:
            parsed = self._parse_interface_result(result["Result"])
            # Convert to string representation
            result_str = self._term_to_string(parsed)
            return ConjureResponse(result=result_str, spell=spell_str)
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
            if isinstance(parsed, PrologTerm) and parsed.functor == "component_types":
                args = parsed.args
                entity_name = str(args[0]) if len(args) > 0 else entity
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
            if isinstance(parsed, PrologTerm) and parsed.functor == "components":
                args = parsed.args
                entity_name = str(args[0]) if len(args) > 0 else entity
                comp_type = str(args[1]) if len(args) > 1 else component_type
                components_list = args[2] if len(args) > 2 else []

                components = []
                if isinstance(components_list, list):
                    for comp_entry in components_list:
                        if isinstance(comp_entry, PrologTerm) and comp_entry.functor == "comp_entry":
                            entry_args = comp_entry.args
                            component = (
                                self._term_to_string(entry_args[0])
                                if len(entry_args) > 0
                                else str(comp_entry)
                            )
                            flag = str(entry_args[1]) if len(entry_args) > 1 else "value"
                            components.append(
                                ComponentEntry(component=component, flag=flag)
                            )
                        else:
                            # Fallback for non-comp_entry structure
                            components.append(
                                ComponentEntry(component=self._term_to_string(comp_entry), flag="value")
                            )

                return ComponentsResponse(
                    entity=entity_name,
                    component_type=comp_type,
                    components=components,
                )

        raise GrimoireError("Failed to retrieve components")

    def doc(self, entity: Any = "system") -> DocumentationResponse:
        """Get documentation for entity"""
        # Build query with entity embedded in the query string
        query = f"python_cast(conjure(interface(doc({entity}))), Result)"
        result = janus_swi.query_once(query)
        
        if result and "Result" in result:
            parsed = self._parse_interface_result(result["Result"])
            if isinstance(parsed, PrologTerm) and parsed.functor == "documentation":
                args = parsed.args
                if len(args) != 2:
                    raise GrimoireError(f"Malformed documentation result: {parsed}")
                doc_string = str(args[1])
                return DocumentationResponse(entity=str(entity), documentation=doc_string)
        
        raise GrimoireError(
            f"Failed to retrieve documentation for entity: {entity}"
        )

    def status(self) -> StatusResponse:
        """Get session status"""
        query = "python_cast(conjure(interface(status)), Result)"

        result = janus_swi.query_once(query)
        if result and "Result" in result:
            parsed = self._parse_interface_result(result["Result"])
            if isinstance(parsed, PrologTerm) and parsed.functor == "session_status":
                args = parsed.args
                status_info = args[0] if args else None

                # Extract status information from the nested structure
                if isinstance(status_info, PrologTerm) and status_info.functor == "status_info":
                    status_args = status_info.args
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
            if isinstance(parsed, PrologTerm) and parsed.functor == "entities":
                args = parsed.args
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
            # Convert to string representation
            result_str = self._term_to_string(parsed)
            return SessionCommandResponse(result=result_str)
        else:
            raise GrimoireError("Failed to execute session command")

    def load(self, entity_spec: str) -> LoadResponse:
        """Load entity into current session"""
        query = "python_cast(conjure(interface(load(EntitySpec))), Result)"
        result = janus_swi.query_once(query, {"EntitySpec": entity_spec})

        if result and "Result" in result:
            parsed = self._parse_interface_result(result["Result"])
            # Extract entity name from parsed result
            if isinstance(parsed, PrologTerm) and parsed.functor == "entity_loaded":
                args = parsed.args
                entity = str(args[0]) if args else entity_spec
            else:
                entity = str(entity_spec)

            return LoadResponse(entity=entity)
        else:
            raise GrimoireError(f"Failed to load entity {entity_spec}")

    def read_file(self, file_path: str, start: int, end: int) -> ReadFileResponse:
        """Read lines from a file using 1-based indexing"""
        # Use python_cast with interface(read_file(...))
        query = f"python_cast(conjure(interface(read_file({file_path!r}, {start}, {end}))), Result)"
        result = janus_swi.query_once(query)
        
        parsed_lines = []
        
        if result and "Result" in result:
            parsed = self._parse_interface_result(result["Result"])
            # parsed should be the ContentWithLineNumbers list
            if isinstance(parsed, list):
                for item in parsed:
                    if isinstance(item, PrologTerm) and item.functor == "line":
                        line_num = int(item.args[0])
                        line_content = str(item.args[1])
                        parsed_lines.append(LineContent(line_number=line_num, content=line_content))
                
        return ReadFileResponse(
            file_path=file_path,
            lines=parsed_lines
        )

    def edit_file(self, file_path: str, edits: List[EditOperation]) -> EditFileResponse:
        """Edit file with specified edits"""
        prolog_edits = []
        for edit in edits:
            if isinstance(edit, EditAppend):
                prolog_edits.append(f"append({edit.content!r})")
            elif isinstance(edit, EditInsert):
                prolog_edits.append(f"insert({edit.line}, {edit.content!r})")
            elif isinstance(edit, EditDelete):
                prolog_edits.append(f"delete({edit.start_line}, {edit.end_line})")
            elif isinstance(edit, EditReplace):
                prolog_edits.append(f"replace({edit.start_line}, {edit.end_line}, {edit.content!r})")
        
        edits_str = f"[{', '.join(prolog_edits)}]"
        
        # Use python_cast with interface(edit_file(...))
        query = f"python_cast(conjure(interface(edit_file({file_path!r}, {edits_str}))), Result)"
        result = janus_swi.query_once(query)
        
        if result and "Result" in result:
            parsed = self._parse_interface_result(result["Result"])
            return EditFileResponse(
                file_path=file_path,
                result=self._term_to_string(parsed)
            )
        else:
            raise GrimoireError("Failed to execute edit_file")
    
    def perceive(self, query_str: str) -> PerceiveResponse:
        """Execute a perceive query (read-only observation)"""
        return self.call_perceive_query(query_str)
    
    def conjure(self, spell_str: str) -> ConjureResponse:
        """Execute a conjure spell (mutable operation)"""
        return self.call_conjure_spell(spell_str)
    
    def exec(self, query_str: str) -> PerceiveResponse:
        """
        Execute arbitrary Prolog query with variable bindings.

        This executes a raw Prolog query using call/1 and returns all variable bindings.
        Similar to perceive but without the perceive/1 wrapper.

        Args:
            query_str: Prolog query as a string (e.g., "member(X, [1,2,3])")

        Returns:
            PerceiveResponse with all solutions and their variable bindings
        """
        # Use the python_exec_query helper from interface/semantics.pl
        query = "python_exec_query(QueryStr, Solutions)"

        result = janus_swi.query_once(query, {"QueryStr": query_str})

        if result and "Solutions" in result:
            solutions_list = result["Solutions"]

            # Convert solutions to dict format
            solutions = []
            if isinstance(solutions_list, list):
                for solution in solutions_list:
                    if isinstance(solution, dict):
                        # Solution is a Python dict from Prolog
                        sol_dict = {}
                        for var_name, var_value in solution.items():
                            # Convert the value from Python dict format
                            parsed_value = self._parse_python_dict_result(var_value)
                            sol_dict[var_name] = self._term_to_string(parsed_value)
                        solutions.append(sol_dict)
                    else:
                        # Unexpected format - try to handle gracefully
                        solutions.append({"result": str(solution)})

            return PerceiveResponse(
                solutions=solutions,
                count=len(solutions),
                query=query_str,
            )

        raise GrimoireError(f"Failed to execute query: {query_str}")

    def get_toolset(self) -> GrimoireToolset:
        """
        Return a comprehensive toolset for AI agent frameworks.

        Returns a GrimoireToolset containing:
        - tools: List of callable methods with their names
        - system_prompt: System instructions for the agent
        - tool_descriptions: Docstrings for each tool
        """
        # Core tools to expose
        tools = [
            self.compt,
            self.comp,
            self.doc,
            self.status,
            self.entities,
            self.test,
            self.session,
            self.load,
            self.read_file,
            self.edit_file,
            self.perceive,
            self.conjure,
            self.exec
        ]

        # Get docstrings for interface commands
        docstrings = self.query_interface_docstrings()

        # Build tool descriptions from docstrings and method __doc__
        tool_descriptions = {}
        for tool in tools:
            name = tool.__name__
            # Try to get from interface docstrings first, fallback to method __doc__
            if name in docstrings:
                tool_descriptions[name] = docstrings[name]
            elif tool.__doc__:
                tool_descriptions[name] = tool.__doc__
            else:
                tool_descriptions[name] = f"Execute {name} operation"

        return GrimoireToolset(
            tools=tools,
            system_prompt=self.system_instructions(),
            tool_descriptions=tool_descriptions
        )
