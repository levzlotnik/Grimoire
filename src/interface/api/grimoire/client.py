"""
Grimoire Python Client - Clean janus-swi bridge for Grimoire

Provides typed Python interface to Grimoire Prolog system using:
- python_magic_cast/3 for universal spell casting with template filling
- PrologTerm for type-safe term representation
- Pydantic models for response parsing
"""

import os
from abc import ABC, abstractmethod
from typing import Dict, List, Any, Optional, TypeVar, Type
from typing_extensions import Self
from pydantic import BaseModel
import janus_swi as janus


# ============================================================================
# EXCEPTIONS
# ============================================================================

class GrimoireError(RuntimeError):
    """Exception raised when Grimoire operations fail"""
    pass


# ============================================================================
# PROLOG TERM REPRESENTATION
# ============================================================================

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
                    # Check if it needs quoting
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
        """Create PrologTerm from dict structure returned by term_struct_to_python_dict"""
        if not isinstance(d, dict):
            # Primitive value, not a term
            return d

        term_type = d.get("type")

        if term_type in ["atom", "string", "int", "float"]:
            # Primitive - return value directly
            return d.get("value")
        elif term_type == "list":
            # List - recursively convert elements
            elements = d.get("elements", [])
            return [cls.from_dict(elem) for elem in elements]
        elif term_type == "term_struct":
            # Compound term - recursively convert args
            functor = d.get("functor")
            args_dicts = d.get("args", [])
            args = [cls.from_dict(arg) for arg in args_dicts]
            return cls(functor, args)
        else:
            # Unknown or missing type - return as-is
            return d


# ============================================================================
# RESPONSE BASE CLASS
# ============================================================================

T = TypeVar('T', bound='PrologTermRecord')

class PrologTermRecord(BaseModel, ABC):
    """Base class for Pydantic models that parse PrologTerm objects."""

    @classmethod
    @abstractmethod
    def from_prolog(cls, term: PrologTerm) -> Self:
        """Parse from PrologTerm object. Must handle ok(...) and error(...) cases."""
        pass


# ============================================================================
# RESPONSE MODELS - INTERFACE OPERATIONS
# ============================================================================

class ComponentTypesResponse(PrologTermRecord):
    """Response from component_types query"""
    types: List[str]

    @classmethod
    def from_prolog(cls, term: PrologTerm):
        if term.functor == 'ok':
            types_term = term.args[0]
            if types_term.functor != 'types':
                raise GrimoireError(f"Expected types(...), got {types_term.functor}")
            return cls(types=types_term.args[0])
        elif term.functor == 'error':
            error_details = term.args[0] if term.args else "unknown"
            raise GrimoireError(f"Failed to get component types: {error_details}")
        else:
            raise GrimoireError(f"Unexpected result structure: {term!s}")


class ComponentsResponse(PrologTermRecord):
    """Response from components query - either unique value or set"""
    is_unique: bool
    value: Optional[Any] = None
    values: Optional[List[Any]] = None

    @classmethod
    def from_prolog(cls, term: PrologTerm):
        if term.functor == 'ok':
            result_term = term.args[0]

            if result_term.functor == 'unique':
                return cls(is_unique=True, value=result_term.args[0])
            elif result_term.functor == 'set':
                return cls(is_unique=False, values=result_term.args[0])
            else:
                raise GrimoireError(f"Expected unique(...) or set(...), got {result_term.functor}")
        elif term.functor == 'error':
            error_details = term.args[0] if term.args else "unknown"
            raise GrimoireError(f"Failed to get components: {error_details}")
        else:
            raise GrimoireError(f"Unexpected result structure: {term!s}")


class DocstringResponse(PrologTermRecord):
    """Response from docstring query"""
    docstring: str

    @classmethod
    def from_prolog(cls, term: PrologTerm):
        if term.functor == 'ok':
            doc_term = term.args[0]
            if doc_term.functor != 'doc':
                raise GrimoireError(f"Expected doc(...), got {doc_term.functor}")
            return cls(docstring=doc_term.args[0])
        elif term.functor == 'error':
            error_details = term.args[0] if term.args else "unknown"
            raise GrimoireError(f"Failed to get docstring: {error_details}")
        else:
            raise GrimoireError(f"Unexpected result structure: {term!s}")


class EntitiesResponse(PrologTermRecord):
    """Response from entities query"""
    entities: List[Any]

    @classmethod
    def from_prolog(cls, term: PrologTerm):
        if term.functor == 'ok':
            entities_term = term.args[0]
            if entities_term.functor != 'entities':
                raise GrimoireError(f"Expected entities(...), got {entities_term.functor}")
            return cls(entities=entities_term.args[0])
        elif term.functor == 'error':
            error_details = term.args[0] if term.args else "unknown"
            raise GrimoireError(f"Failed to get entities: {error_details}")
        else:
            raise GrimoireError(f"Unexpected result structure: {term!s}")


class TestResponse(PrologTermRecord):
    """Response from test operation"""
    status: str  # "passed", "listed", or "failed"
    details: Optional[str] = None

    @classmethod
    def from_prolog(cls, term: PrologTerm):
        if term.functor == 'ok':
            result_term = term.args[0]
            if result_term == 'tests_passed':
                return cls(status="passed")
            elif result_term == 'tests_listed':
                return cls(status="listed")
            else:
                return cls(status="ok", details=str(result_term))
        elif term.functor == 'error':
            error_details = term.args[0] if term.args else "unknown"
            return cls(status="failed", details=str(error_details))
        else:
            raise GrimoireError(f"Unexpected result structure: {term!s}")


class SystemInstructionsResponse(PrologTermRecord):
    """Response from system_instructions query"""
    instructions: str

    @classmethod
    def from_prolog(cls, term: PrologTerm):
        if term.functor == 'ok':
            inst_term = term.args[0]
            if inst_term.functor != 'instructions':
                raise GrimoireError(f"Expected instructions(...), got {inst_term.functor}")
            return cls(instructions=inst_term.args[0])
        elif term.functor == 'error':
            error_details = term.args[0] if term.args else "unknown"
            raise GrimoireError(f"Failed to get system instructions: {error_details}")
        else:
            raise GrimoireError(f"Unexpected result structure: {term!s}")


# Generic responses for fundamental operations
class ConjureResponse(PrologTermRecord):
    """Response from conjure spell - unknown structure, stringified"""
    result: str

    @classmethod
    def from_prolog(cls, term: PrologTerm):
        if term.functor == 'ok':
            inner = term.args[0] if term.args else ""
            return cls(result=str(inner))
        elif term.functor == 'error':
            error_details = term.args[0] if term.args else "unknown"
            raise GrimoireError(f"Conjure failed: {error_details}")
        else:
            raise GrimoireError(f"Unexpected result structure: {term!s}")


class PerceiveResponse(PrologTermRecord):
    """Response from perceive query - unknown structure, stringified"""
    result: str

    @classmethod
    def from_prolog(cls, term: PrologTerm):
        if term.functor == 'ok':
            inner = term.args[0] if term.args else ""
            return cls(result=str(inner))
        elif term.functor == 'error':
            error_details = term.args[0] if term.args else "unknown"
            raise GrimoireError(f"Perceive failed: {error_details}")
        else:
            raise GrimoireError(f"Unexpected result structure: {term!s}")


# Generic response for session/prove_it/sauce_me operations (delegates to other spells)
class GenericResponse(PrologTermRecord):
    """Generic response for operations that delegate to other spells"""
    result: str

    @classmethod
    def from_prolog(cls, term: PrologTerm):
        if term.functor == 'ok':
            inner = term.args[0] if term.args else ""
            return cls(result=str(inner))
        elif term.functor == 'error':
            error_details = term.args[0] if term.args else "unknown"
            raise GrimoireError(f"Operation failed: {error_details}")
        else:
            raise GrimoireError(f"Unexpected result structure: {term!s}")


# ============================================================================
# GRIMOIRE CLIENT
# ============================================================================

class Grimoire:
    """
    Grimoire Python interface - typed methods calling interface spells.

    All methods use python_magic_cast/3 except exec (direct janus query).
    """

    def __init__(self):
        """Initialize Grimoire client and ensure GRIMOIRE_ROOT is set"""
        if not os.getenv('GRIMOIRE_ROOT'):
            raise GrimoireError("GRIMOIRE_ROOT environment variable not set")

        # Load grimoire.pl to initialize the system
        grimoire_root = os.getenv('GRIMOIRE_ROOT')
        grimoire_pl = os.path.join(grimoire_root, 'src', 'grimoire.pl')

        try:
            janus.consult(grimoire_pl)
        except Exception as e:
            raise GrimoireError(f"Failed to load grimoire.pl: {e}")

    def _magic_cast(
        self,
        spell_sig: str,
        args: Dict[str, Any],
        output_type: Type[T]
    ) -> T:
        """
        Cast spell using python_magic_cast/3 with template filling.

        Args:
            spell_sig: Spell signature like "perceive(interface(component_types))"
            args: Dict mapping template variables to values
            output_type: Response class to parse into

        Returns:
            Parsed response of output_type

        Raises:
            GrimoireError: If query fails or spell returns error
        """
        result = janus.query_once(
            "python_magic_cast(SpellSig, Args, PyResult)",
            {"SpellSig": spell_sig, "Args": args}
        )

        if not result.get('truth', False):
            raise GrimoireError(f"Query failed for spell: {spell_sig}")

        term = PrologTerm.from_dict(result['PyResult'])
        return output_type.from_prolog(term)

    # ========================================================================
    # ECS INTROSPECTION
    # ========================================================================

    def component_types(self, entity: str) -> ComponentTypesResponse:
        """List all component types for an entity"""
        return self._magic_cast(
            "perceive(interface(component_types))",
            {"Entity": entity},
            ComponentTypesResponse
        )

    def components(self, entity: str, comp_type: str) -> ComponentsResponse:
        """Get verified components with smart singleton/set detection"""
        return self._magic_cast(
            "perceive(interface(components))",
            {"Entity": entity, "Type": comp_type},
            ComponentsResponse
        )

    def docstring(self, entity: str) -> DocstringResponse:
        """Get entity docstring"""
        return self._magic_cast(
            "perceive(interface(docstring))",
            {"Entity": entity},
            DocstringResponse
        )

    def entities(self) -> EntitiesResponse:
        """List all entities in the system"""
        return self._magic_cast(
            "perceive(interface(entities))",
            {},
            EntitiesResponse
        )

    # ========================================================================
    # FUNDAMENTAL OPERATIONS
    # ========================================================================

    def conjure(self, spell_sig: str, args: Dict[str, Any]) -> ConjureResponse:
        """Execute conjuration spell (fundamental operation)"""
        return self._magic_cast(
            f"conjure({spell_sig})",
            args,
            ConjureResponse
        )

    def perceive(self, query_sig: str, args: Dict[str, Any]) -> PerceiveResponse:
        """Execute perception query (fundamental operation)"""
        return self._magic_cast(
            f"perceive({query_sig})",
            args,
            PerceiveResponse
        )

    # ========================================================================
    # TESTING
    # ========================================================================

    def test(self, args: Optional[List[str]] = None) -> TestResponse:
        """Run test suite with optional args

        Note: This calls python_interface_test/2 directly (NOT via magic_cast)
        because test is system infrastructure, not a user-facing spell.
        Using magic_cast would set in_magic_cast flag and interfere with
        tests that validate cast_impl guards.
        """
        result = janus.query_once(
            "python_interface_test(Args, PyResult)",
            {"Args": args or []}
        )

        if not result.get('truth', False):
            raise GrimoireError("Test command failed")

        term = PrologTerm.from_dict(result['PyResult'])
        return TestResponse.from_prolog(term)

    # ========================================================================
    # SESSION MANAGEMENT
    # ========================================================================

    def session_create(self, session_id: str) -> GenericResponse:
        """Create a new session"""
        return self._magic_cast(
            "conjure(interface(session_create))",
            {"SessionId": session_id},
            GenericResponse
        )

    def session_switch(self, session_id: str) -> GenericResponse:
        """Switch to different session"""
        return self._magic_cast(
            "conjure(interface(session_switch))",
            {"SessionId": session_id},
            GenericResponse
        )

    def session_delete(self, session_id: str) -> GenericResponse:
        """Delete a session"""
        return self._magic_cast(
            "conjure(interface(session_delete))",
            {"SessionId": session_id},
            GenericResponse
        )

    def session_export(self, session_id: str, destination: str) -> GenericResponse:
        """Export session to archive"""
        return self._magic_cast(
            "conjure(interface(session_export))",
            {"SessionId": session_id, "Dest": destination},
            GenericResponse
        )

    def session_import(self, archive: str) -> GenericResponse:
        """Import session from archive"""
        return self._magic_cast(
            "conjure(interface(session_import))",
            {"Archive": archive},
            GenericResponse
        )

    # ========================================================================
    # META-INTROSPECTION
    # ========================================================================

    def prove_it(self, entity: str, comp_type: str, value: Any) -> GenericResponse:
        """Component provenance - where generated and how verified"""
        return self._magic_cast(
            "perceive(interface(prove_it))",
            {"Entity": entity, "Type": comp_type, "Value": value},
            GenericResponse
        )

    def sauce_me(self, spell_ctor: str) -> GenericResponse:
        """Spell metadata - source location, implementation, formats"""
        return self._magic_cast(
            "perceive(interface(sauce_me))",
            {"SpellCtor": spell_ctor},
            GenericResponse
        )

    def system_instructions(self) -> SystemInstructionsResponse:
        """Get system instructions/prompt for AI agents"""
        return self._magic_cast(
            "perceive(interface(system_instructions))",
            {},
            SystemInstructionsResponse
        )

    # ========================================================================
    # METADATA HELPERS
    # ========================================================================

    def list_all_spells(self) -> List[str]:
        """List all registered spells using ECS queries"""
        # Get spell types (conjure, perceive)
        spell_types_resp = self.components("spell", "ctor")
        spell_types = spell_types_resp.values if not spell_types_resp.is_unique else [spell_types_resp.value]

        spells = []
        for spell_type in spell_types:
            # Get all constructors for this spell type
            ctors_resp = self.components(spell_type, "ctor")
            ctors = ctors_resp.values if not ctors_resp.is_unique else [ctors_resp.value]

            for ctor in ctors:
                spells.append(f"{spell_type}({ctor})")

        return spells

    # ========================================================================
    # DEVELOPER TOOLS - BYPASS ALL SAFETY
    # ========================================================================

    def exec(self, query_str: str) -> List[Dict[str, str]]:
        """Execute arbitrary Prolog query (developer mode - unlimited power)

        Returns list of variable binding dicts. Each dict maps variable names (as strings)
        to their values (as strings).
        If query has no variables, returns list of empty dicts (one per solution).
        """
        try:
            # Call the conjure(interface(exec)) spell using python_magic_cast/3
            result = janus.query_once(
                "python_magic_cast(SpellSig, Args, PyResult)",
                {
                    "SpellSig": 'conjure(interface(exec))',
                    "Args": {
                        'QueryString': query_str
                    }
                }
            )

            if not result.get('truth', False):
                raise GrimoireError("Exec spell invocation failed")

            py_result = result['PyResult']

            # PyResult is a Python dict from term_struct_to_python_dict
            # Expected format: {type: "term_struct", functor: "ok", args: [{type: "term_struct", functor: "solutions", args: [list of py dicts]}]}
            if not isinstance(py_result, dict):
                raise GrimoireError(f"Expected dict from python_magic_cast, got {type(py_result)}")

            # Extract ok(solutions(...))
            if py_result.get('functor') != 'ok':
                raise GrimoireError(f"Exec failed: {py_result}")

            solutions_struct = py_result['args'][0]
            if solutions_struct.get('functor') != 'solutions':
                raise GrimoireError(f"Expected solutions(...), got {solutions_struct.get('functor')}")

            # Get the list of py-tagged dicts - these come through as regular Python dicts via janus
            solutions_list = solutions_struct['args'][0]
            if not isinstance(solutions_list, dict) or solutions_list.get('type') != 'list':
                raise GrimoireError(f"Expected list of solutions, got {solutions_list}")

            # Each element is a py{Var: Val, ...} dict that janus passes through as a regular Python dict
            solutions = []
            for sol in solutions_list['elements']:
                if isinstance(sol, dict):
                    # py-tagged dicts come through as regular Python dicts
                    solutions.append(sol)
                else:
                    solutions.append({})

            return solutions
        except Exception as e:
            raise GrimoireError(f"Query execution failed: {e}")
