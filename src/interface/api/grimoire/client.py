"""
Grimoire Python Client - Clean janus-swi bridge for Grimoire

Provides typed Python interface to Grimoire Prolog system using:
- python_magic_cast/3 for universal spell casting with template filling
- _PrologTerm for internal term representation from term_to_json/2
- Pydantic models for response parsing
"""

import os
from dataclasses import dataclass
from abc import ABC, abstractmethod
from typing import Dict, List, Any, Optional, TypeVar, Type, Literal
from typing_extensions import Self
from pydantic import BaseModel
import janus_swi as janus

PrologTermType = Literal['atom', 'int', 'float', 'string', 'list', 'compound']


# ============================================================================
# EXCEPTIONS
# ============================================================================

class GrimoireError(RuntimeError):
    """Exception raised when Grimoire operations fail"""
    pass


# ============================================================================
# INTERNAL PROLOG TERM REPRESENTATION
# ============================================================================

class _PrologTerm(ABC):
    """Base class for internal Prolog term representation from term_to_json/2"""
    @abstractmethod
    def to_canonical(self) -> str:
        """Convert to Prolog syntax for atom_to_term/3"""

    @abstractmethod
    def term_type(self) -> PrologTermType:
        """Return the type of this term"""

@dataclass
class _PrologTermAtom(_PrologTerm):
    atom: str

    def term_type(self) -> Literal['atom']:
        return 'atom'

    def to_canonical(self) -> str:
        # Quote if needed (spaces, uppercase start, special chars, operators)
        needs_quote = (
            ' ' in self.atom or
            '(' in self.atom or
            ')' in self.atom or
            (self.atom and self.atom[0].isupper()) or
            self.atom in [':-', ',', ';', '|', '!', '->']
        )
        return f"'{self.atom}'" if needs_quote else self.atom

@dataclass
class _PrologTermInt(_PrologTerm):
    value: int

    def term_type(self) -> Literal['int']:
        return 'int'

    def to_canonical(self) -> str:
        return str(self.value)

@dataclass
class _PrologTermFloat(_PrologTerm):
    value: float

    def term_type(self) -> Literal['float']:
        return 'float'

    def to_canonical(self) -> str:
        return str(self.value)

@dataclass
class _PrologTermString(_PrologTerm):
    value: str

    def term_type(self) -> Literal['string']:
        return 'string'

    def to_canonical(self) -> str:
        # Escape quotes and backslashes
        escaped = self.value.replace('\\', '\\\\').replace('"', '\\"')
        return f'"{escaped}"'

@dataclass
class _PrologTermList(_PrologTerm):
    items: List['_PrologTerm']

    def term_type(self) -> Literal['list']:
        return 'list'

    def to_canonical(self) -> str:
        if not self.items:
            return "[]"
        items_str = ', '.join(item.to_canonical() for item in self.items)
        return f"[{items_str}]"

@dataclass
class _PrologTermDict(_PrologTerm):
    items: Dict[str, '_PrologTerm']

    def term_type(self) -> Literal['dict']:
        return 'dict'

    def to_canonical(self) -> str:
        if not self.items:
            return "_{}"
        pairs = ', '.join(f"{k}: {v.to_canonical()}" for k, v in self.items.items())
        return f"_{{{pairs}}}"

@dataclass
class _PrologTermCompound(_PrologTerm):
    functor: str
    args: List['_PrologTerm']

    def term_type(self) -> Literal['compound']:
        return 'compound'

    @property
    def arity(self) -> int:
        return len(self.args)

    def to_canonical(self) -> str:
        # Compounds always have args (0-arity is represented as atom)
        args_str = ', '.join(arg.to_canonical() for arg in self.args)
        return f"{self.functor}({args_str})"

def _parse_prolog_term(json_data: Any) -> _PrologTerm:
    """Parse term_to_json/2 output into _PrologTerm hierarchy"""
    # Handle JSON arrays (Prolog lists)
    if isinstance(json_data, list):
        return _PrologTermList([_parse_prolog_term(item) for item in json_data])

    # Handle non-dict primitives (shouldn't happen with term_to_json, but be safe)
    if not isinstance(json_data, dict):
        if isinstance(json_data, str):
            return _PrologTermAtom(json_data)
        elif isinstance(json_data, int):
            return _PrologTermInt(json_data)
        elif isinstance(json_data, float):
            return _PrologTermFloat(json_data)
        else:
            raise GrimoireError(f"Unknown primitive type: {type(json_data)}")

    term_type = json_data.get("type")

    if term_type == "atom":
        return _PrologTermAtom(json_data["value"])
    elif term_type == "integer":
        return _PrologTermInt(json_data["value"])
    elif term_type == "float":
        return _PrologTermFloat(json_data["value"])
    elif term_type == "string":
        return _PrologTermString(json_data["value"])
    elif term_type == "compound":
        functor = json_data["functor"]
        args_json = json_data.get("args", [])
        args = [_parse_prolog_term(arg) for arg in args_json]
        return _PrologTermCompound(functor, args)
    elif term_type == "dict":
        # Prolog dicts: {"type": "dict", "value": {key: val, ...}}
        value_dict = json_data.get("value", {})
        # Convert nested values
        items = {k: _parse_prolog_term(v) for k, v in value_dict.items()}
        return _PrologTermDict(items)
    else:
        raise GrimoireError(f"Unknown term type from term_to_json: {term_type}")


# ============================================================================
# RESPONSE BASE CLASS
# ============================================================================

T = TypeVar('T', bound='FromPrologTerm')

class FromPrologTerm(BaseModel, ABC):
    """Base class for Pydantic models that parse _PrologTerm objects."""

    @classmethod
    @abstractmethod
    def validate(cls, term: _PrologTerm) -> None:
        """Validate term structure deeply. Raise GrimoireError if invalid."""
        pass

    @classmethod
    @abstractmethod
    def from_prolog(cls, term: _PrologTerm) -> Self:
        """Parse from _PrologTerm object after validation."""
        pass


# ============================================================================
# RESPONSE MODELS - INTERFACE OPERATIONS
# ============================================================================

class ComponentTypesResponse(FromPrologTerm):
    """Response from component_types query"""
    types: List[str]

    @classmethod
    def validate(cls, term: _PrologTerm) -> None:
        # Validate: ok(types([...])) structure with exact arity
        if term.term_type() != 'compound' or term.functor != 'ok' or len(term.args) != 1:
            raise GrimoireError(f"Expected ok/1, got {term.to_canonical()}")

        types_term = term.args[0]
        if types_term.term_type() != 'compound' or types_term.functor != 'types' or len(types_term.args) != 1:
            raise GrimoireError(f"Expected types/1, got {types_term.to_canonical()}")

        types_list = types_term.args[0]
        if types_list.term_type() != 'list':
            raise GrimoireError(f"Expected list, got {types_list.to_canonical()}")

    @classmethod
    def from_prolog(cls, term: _PrologTerm):
        cls.validate(term)
        types_list = term.args[0].args[0]
        types = [item.to_canonical() for item in types_list.items]
        return cls(types=types)

    def pformat(self) -> str:
        """Pretty format for human-readable output"""
        if not self.types:
            return "No component types found."
        return '\n'.join(f"  - {t}" for t in self.types)


class ComponentsResponse(FromPrologTerm):
    """Response from components query - either unique value or set"""
    is_unique: bool
    value: Optional[str] = None
    values: Optional[List[str]] = None

    @classmethod
    def validate(cls, term: _PrologTerm) -> None:
        if term.term_type() != 'compound' or term.functor != 'ok' or len(term.args) != 1:
            raise GrimoireError(f"Expected ok/1, got {term.to_canonical()}")

        result_term = term.args[0]
        if result_term.term_type() != 'compound' or len(result_term.args) != 1:
            raise GrimoireError(f"Expected compound/1, got {result_term.to_canonical()}")

        if result_term.functor == 'unique':
            pass  # unique/1 is valid
        elif result_term.functor == 'set':
            if result_term.args[0].term_type() != 'list':
                raise GrimoireError(f"Expected list in set/1, got {result_term.args[0].to_canonical()}")
        else:
            raise GrimoireError(f"Expected unique/1 or set/1, got {result_term.functor}/{len(result_term.args)}")

    @classmethod
    def from_prolog(cls, term: _PrologTerm):
        cls.validate(term)
        result_term = term.args[0]

        if result_term.functor == 'unique':
            return cls(is_unique=True, value=result_term.args[0].to_canonical())
        else:  # set
            values = [item.to_canonical() for item in result_term.args[0].items]
            return cls(is_unique=False, values=values)

    def pformat(self) -> str:
        """Pretty format for human-readable output"""
        if self.is_unique:
            return f"{self.value}"
        elif self.values:
            return '\n'.join(f"  - {v}" for v in self.values)
        else:
            return "No components found."


class DocstringResponse(FromPrologTerm):
    """Response from docstring query"""
    docstring: str

    @classmethod
    def validate(cls, term: _PrologTerm) -> None:
        # Check: ok(doc(DocString))
        if term.term_type() != 'compound' or term.functor != 'ok' or len(term.args) != 1:
            raise GrimoireError(f"Expected ok/1, got {term.to_canonical()}")

        doc_term = term.args[0]
        if doc_term.term_type() != 'compound' or doc_term.functor != 'doc' or len(doc_term.args) != 1:
            raise GrimoireError(f"Expected doc/1, got {doc_term.to_canonical()}")

    @classmethod
    def from_prolog(cls, term: _PrologTerm):
        cls.validate(term)

        doc_term = term.args[0]
        doc_value = doc_term.args[0]

        # Docstrings are strings, extract value
        if doc_value.term_type() == 'string':
            docstring_str = doc_value.value
        elif doc_value.term_type() == 'atom':
            docstring_str = doc_value.atom
        else:
            docstring_str = doc_value.to_canonical()

        return cls(docstring=docstring_str)

    def pformat(self) -> str:
        """Pretty format for human-readable output"""
        return self.docstring


class EntitiesResponse(FromPrologTerm):
    """Response from entities query"""
    entities: List[str]

    @classmethod
    def validate(cls, term: _PrologTerm) -> None:
        # Check: ok(entities([...]))
        if term.term_type() != 'compound' or term.functor != 'ok' or len(term.args) != 1:
            raise GrimoireError(f"Expected ok/1, got {term.to_canonical()}")

        entities_term = term.args[0]
        if entities_term.term_type() != 'compound' or entities_term.functor != 'entities' or len(entities_term.args) != 1:
            raise GrimoireError(f"Expected entities/1, got {entities_term.to_canonical()}")

        entities_list = entities_term.args[0]
        if entities_list.term_type() != 'list':
            raise GrimoireError(f"Expected list, got {entities_list.to_canonical()}")

    @classmethod
    def from_prolog(cls, term: _PrologTerm):
        cls.validate(term)

        entities_term = term.args[0]
        entities_list = entities_term.args[0]

        # Convert all entities to canonical strings
        entities = [entity.to_canonical() for entity in entities_list.items]
        return cls(entities=entities)

    def pformat(self) -> str:
        """Pretty format for human-readable output"""
        if not self.entities:
            return "No entities found."
        return '\n'.join(f"  - {e}" for e in self.entities)


class TestResponse(FromPrologTerm):
    """Response from test operation"""
    status: str  # "passed", "listed", or "failed"
    details: Optional[str] = None

    @classmethod
    def validate(cls, term: _PrologTerm) -> None:
        if term.term_type() != 'compound' or len(term.args) != 1:
            raise GrimoireError(f"Expected compound/1, got {term.to_canonical()}")
        if term.functor not in ['ok', 'error']:
            raise GrimoireError(f"Expected ok/1 or error/1, got {term.functor}/{len(term.args)}")

    @classmethod
    def from_prolog(cls, term: _PrologTerm):
        cls.validate(term)
        result_term = term.args[0]

        if term.functor == 'ok':
            if result_term.term_type() == 'atom':
                if result_term.atom == 'tests_passed':
                    return cls(status="passed")
                elif result_term.atom == 'tests_listed':
                    return cls(status="listed")
            return cls(status="ok", details=result_term.to_canonical())
        else:  # error
            return cls(status="failed", details=result_term.to_canonical())

    def pformat(self) -> str:
        """Pretty format for human-readable output"""
        if self.status == "passed":
            return "✓ All tests passed"
        elif self.status == "listed":
            return "Test units listed"
        elif self.status == "failed":
            return f"✗ Tests failed: {self.details}" if self.details else "✗ Tests failed"
        else:
            return f"{self.status}: {self.details}" if self.details else self.status


class SystemInstructionsResponse(FromPrologTerm):
    """Response from system_instructions query"""
    instructions: str

    @classmethod
    def validate(cls, term: _PrologTerm) -> None:
        if term.term_type() != 'compound' or term.functor != 'ok' or len(term.args) != 1:
            raise GrimoireError(f"Expected ok/1, got {term.to_canonical()}")

        inst_term = term.args[0]
        if inst_term.term_type() != 'compound' or inst_term.functor != 'instructions' or len(inst_term.args) != 1:
            raise GrimoireError(f"Expected instructions/1, got {inst_term.to_canonical()}")

    @classmethod
    def from_prolog(cls, term: _PrologTerm):
        cls.validate(term)
        inst_value = term.args[0].args[0]

        if inst_value.term_type() == 'string':
            return cls(instructions=inst_value.value)
        elif inst_value.term_type() == 'atom':
            return cls(instructions=inst_value.atom)
        else:
            return cls(instructions=inst_value.to_canonical())

    def pformat(self) -> str:
        """Pretty format for human-readable output"""
        return self.instructions


class ExecResponse(FromPrologTerm):
    """Response from exec query with variable bindings"""
    solutions: List[Dict[str, str]]

    @classmethod
    def validate(cls, term: _PrologTerm) -> None:
        # ok(solutions([...]))
        if term.term_type() != 'compound' or term.functor != 'ok':
            raise GrimoireError(f"Expected ok(...), got {term.to_canonical()}")

        if len(term.args) != 1:
            raise GrimoireError(f"Expected ok/1, got ok/{len(term.args)}")

        solutions_term = term.args[0]
        if solutions_term.term_type() != 'compound' or solutions_term.functor != 'solutions':
            raise GrimoireError(f"Expected solutions(...), got {solutions_term.to_canonical()}")

    @classmethod
    def from_prolog(cls, term: _PrologTerm):
        cls.validate(term)

        solutions_term = term.args[0]
        solutions_list = solutions_term.args[0]

        if solutions_list.term_type() != 'list':
            raise GrimoireError(f"Expected list, got {solutions_list.term_type()}")

        # Prolog dicts become Python dicts automatically
        solutions = []
        for sol in solutions_list.items:
            if sol.term_type() == 'dict':
                # Extract string values from dict
                sol_dict = {k: v.to_canonical() for k, v in sol.items.items()}
                solutions.append(sol_dict)
            else:
                solutions.append({})

        return cls(solutions=solutions)

    def pformat(self) -> str:
        """Pretty format for human-readable output"""
        if not self.solutions:
            return "No solutions"
        return '\n'.join(str(sol) for sol in self.solutions)


# Generic responses for fundamental operations
class ConjureResponse(FromPrologTerm):
    """Response from conjure spell - unknown structure, stringified"""
    result: str

    @classmethod
    def validate(cls, term: _PrologTerm) -> None:
        if term.term_type() != 'compound' or len(term.args) != 1:
            raise GrimoireError(f"Expected compound/1, got {term.to_canonical()}")
        if term.functor not in ['ok', 'error']:
            raise GrimoireError(f"Expected ok/1 or error/1, got {term.functor}/{len(term.args)}")

    @classmethod
    def from_prolog(cls, term: _PrologTerm):
        cls.validate(term)

        if term.functor == 'ok':
            return cls(result=term.args[0].to_canonical())
        else:  # error
            raise GrimoireError(f"Conjure failed: {term.args[0].to_canonical()}")

    def pformat(self) -> str:
        """Pretty format for human-readable output"""
        return self.result


class PerceiveResponse(FromPrologTerm):
    """Response from perceive query - unknown structure, stringified"""
    result: str

    @classmethod
    def validate(cls, term: _PrologTerm) -> None:
        if term.term_type() != 'compound' or len(term.args) != 1:
            raise GrimoireError(f"Expected compound/1, got {term.to_canonical()}")
        if term.functor not in ['ok', 'error']:
            raise GrimoireError(f"Expected ok/1 or error/1, got {term.functor}/{len(term.args)}")

    @classmethod
    def from_prolog(cls, term: _PrologTerm):
        cls.validate(term)

        if term.functor == 'ok':
            return cls(result=term.args[0].to_canonical())
        else:  # error
            raise GrimoireError(f"Perceive failed: {term.args[0].to_canonical()}")

    def pformat(self) -> str:
        """Pretty format for human-readable output"""
        return self.result


# Generic response for session/prove_it/sauce_me operations (delegates to other spells)
class GenericResponse(FromPrologTerm):
    """Generic response for operations that delegate to other spells"""
    result: str

    @classmethod
    def validate(cls, term: _PrologTerm) -> None:
        if term.term_type() != 'compound' or len(term.args) != 1:
            raise GrimoireError(f"Expected compound/1, got {term.to_canonical()}")
        if term.functor not in ['ok', 'error']:
            raise GrimoireError(f"Expected ok/1 or error/1, got {term.functor}/{len(term.args)}")

    @classmethod
    def from_prolog(cls, term: _PrologTerm):
        cls.validate(term)

        if term.functor == 'ok':
            return cls(result=term.args[0].to_canonical())
        else:  # error
            raise GrimoireError(f"Operation failed: {term.args[0].to_canonical()}")

    def pformat(self) -> str:
        """Pretty format for human-readable output"""
        return self.result


class SessionContextResponse(FromPrologTerm):
    """Response from session context query with focused entity and activity"""
    session_id: str
    focused_entity: Optional[str] = None
    component_values: List[Dict[str, Any]] = []
    common_activity: List[Dict[str, Any]] = []

    @classmethod
    def validate(cls, term: _PrologTerm) -> None:
        # Check: ok(session(...), focused(...), common_activity(...))
        if term.term_type() != 'compound' or term.functor != 'ok' or len(term.args) != 3:
            raise GrimoireError(f"Expected ok/3, got {term.to_canonical()}")

        session_term = term.args[0]
        if session_term.term_type() != 'compound' or session_term.functor != 'session' or len(session_term.args) != 1:
            raise GrimoireError(f"Expected session/1, got {session_term.to_canonical()}")

        focused_term = term.args[1]
        if focused_term.term_type() != 'compound' or focused_term.functor != 'focused' or len(focused_term.args) != 2:
            raise GrimoireError(f"Expected focused/2, got {focused_term.to_canonical()}")

        activity_term = term.args[2]
        if activity_term.term_type() != 'compound' or activity_term.functor != 'common_activity' or len(activity_term.args) != 1:
            raise GrimoireError(f"Expected common_activity/1, got {activity_term.to_canonical()}")

    @classmethod
    def from_prolog(cls, term: _PrologTerm):
        cls.validate(term)

        # Extract session ID
        session_id = term.args[0].args[0].to_canonical()

        # Extract focused entity
        focused_term = term.args[1]
        entity_term = focused_term.args[0]
        if entity_term.term_type() == 'atom' and entity_term.atom == 'none':
            focused_entity = None
            component_values = []
        else:
            # focused(entity(Entity), components_values([...]))
            if entity_term.term_type() == 'compound' and entity_term.functor == 'entity':
                focused_entity = entity_term.args[0].to_canonical()
            else:
                focused_entity = entity_term.to_canonical()

            components_term = focused_term.args[1]
            if components_term.term_type() == 'compound' and components_term.functor == 'components_values':
                components_list = components_term.args[0]
                if components_list.term_type() == 'list':
                    # Parse component_value(type(...), verified(...), broken(...))
                    component_values = []
                    for comp in components_list.items:
                        if comp.term_type() == 'compound' and comp.functor == 'component_value':
                            comp_dict = {
                                'type': comp.args[0].to_canonical() if comp.args else 'unknown',
                                'verified': comp.args[1].to_canonical() if len(comp.args) > 1 else 'unknown',
                                'broken': comp.args[2].to_canonical() if len(comp.args) > 2 else '[]'
                            }
                            component_values.append(comp_dict)
                else:
                    component_values = []
            else:
                component_values = []

        # Extract common activity
        activity_list = term.args[2].args[0]
        if activity_list.term_type() == 'list':
            common_activity = [{'spell': item.to_canonical()} for item in activity_list.items]
        else:
            common_activity = []

        return cls(
            session_id=session_id,
            focused_entity=focused_entity,
            component_values=component_values,
            common_activity=common_activity
        )

    def pformat(self) -> str:
        """Pretty format for human-readable output"""
        lines = [f"Session: {self.session_id}"]

        if self.focused_entity:
            lines.append(f"\nFocused Entity: {self.focused_entity}")
            if self.component_values:
                lines.append("  Components:")
                for comp in self.component_values:
                    lines.append(f"    - {comp['type']}: {comp['verified']}")
        else:
            lines.append("\nNo focused entity")

        if self.common_activity:
            lines.append(f"\nCommon Activity ({len(self.common_activity)} spells):")
            for activity in self.common_activity[:10]:  # Limit to 10
                lines.append(f"  - {activity['spell']}")
            if len(self.common_activity) > 10:
                lines.append(f"  ... and {len(self.common_activity) - 10} more")

        return '\n'.join(lines)


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

    def _make_term(self, term_template: str, args: Dict[str, Any]) -> 'janus.Term':
        """Construct a Prolog term from a template and argument dictionary.

        Args:
            term_template: Prolog term with uppercase variable placeholders
                          e.g., "perceive(interface(component_types(entity(Entity))))"
            args: Dictionary mapping variable names to values
                  e.g., {"Entity": "git"}

        Returns:
            janus.Term object representing the fully ground Prolog term

        Raises:
            GrimoireError: If term construction fails or required variables are missing
        """
        try:
            result = janus.query_once(f"PyTerm = prolog({term_template})", args)

            if not result.get('truth', False):
                raise GrimoireError(f"Failed to construct term: {term_template}")

            return result['PyTerm']

        except janus.PrologError as e:
            error_msg = str(e)
            if 'not sufficiently instantiated' in error_msg:
                # Extract variable names to report which are missing
                import re
                template_vars = set(re.findall(r'\b([A-Z][a-zA-Z0-9_]*)\b', term_template))
                provided_vars = set(args.keys())
                missing_vars = template_vars - provided_vars

                if missing_vars:
                    raise GrimoireError(
                        f"Missing required variables: {', '.join(sorted(missing_vars))}. "
                        f"Template requires: {', '.join(sorted(template_vars))}"
                    )
                raise GrimoireError(f"Variables not sufficiently instantiated: {error_msg}")

            # Re-raise with context
            raise GrimoireError(f"Failed to construct term '{term_template}': {error_msg}")

    def _magic_cast(
        self,
        spell_sig: str,
        args: Dict[str, Any],
        output_type: Type[T]
    ) -> T:
        """
        Cast spell using janus.Term construction and python_magic_cast/2.

        Args:
            spell_sig: Full spell signature with placeholders
                      e.g., "perceive(interface(component_types(entity(Entity))))"
            args: Dict mapping variable names to values e.g., {"Entity": "git"}
            output_type: Response class to parse into

        Returns:
            Parsed response of output_type

        Raises:
            GrimoireError: If term construction or query fails
        """
        # Construct fully ground Prolog term
        spell_term = self._make_term(spell_sig, args)

        # Pass term directly to simplified python_magic_cast/2
        result = janus.query_once(
            "python_magic_cast(SpellTerm, PyResult)",
            {"SpellTerm": spell_term}
        )

        if not result.get('truth', False):
            raise GrimoireError(f"Query failed for spell: {spell_sig}")

        # Convert JSON result to _PrologTerm
        prolog_term = _parse_prolog_term(result['PyResult'])

        # Parse into response class
        return output_type.from_prolog(prolog_term)

    def _resolve_entity(self, entity: Optional[str]) -> str:
        """
        Resolve entity from optional parameter.

        If entity is provided, return it.
        If entity is None, query focused entity from session.
        If no focus set, default to 'system'.

        Args:
            entity: Optional entity name

        Returns:
            Resolved entity name
        """
        if entity is not None:
            return entity

        # Try to get focused entity
        try:
            focused_result = self.session_get_focused()
            if focused_result.is_success:
                # Extract entity from focused_entity(entity(...), ...)
                focused_data = focused_result.result
                if hasattr(focused_data, 'functor') and focused_data.functor == 'focused_entity':
                    entity_term = focused_data.args[0]
                    if hasattr(entity_term, 'functor') and entity_term.functor == 'entity':
                        return str(entity_term.args[0])
        except:
            pass

        # Default to system
        return 'system'

    # ========================================================================
    # ECS INTROSPECTION
    # ========================================================================

    def component_types(self, entity: Optional[str] = None) -> ComponentTypesResponse:
        """List all component types for an entity (defaults to focused entity or 'system')"""
        resolved_entity = self._resolve_entity(entity)
        return self._magic_cast(
            "perceive(interface(component_types(entity(Entity))))",
            {"Entity": resolved_entity},
            ComponentTypesResponse
        )

    def components(self, entity: Optional[str] = None, comp_type: str = "") -> ComponentsResponse:
        """Get verified components with smart singleton/set detection (defaults to focused entity or 'system')"""
        resolved_entity = self._resolve_entity(entity)
        return self._magic_cast(
            "perceive(interface(components(entity(Entity), type(Type))))",
            {"Entity": resolved_entity, "Type": comp_type},
            ComponentsResponse
        )

    def docstring(self, entity: Optional[str] = None) -> DocstringResponse:
        """Get entity docstring (defaults to focused entity or 'system')"""
        resolved_entity = self._resolve_entity(entity)
        return self._magic_cast(
            "perceive(interface(docstring(entity(Entity))))",
            {"Entity": resolved_entity},
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

        # Parse JSON from term_to_json/2
        term = _parse_prolog_term(result['PyResult'])
        return TestResponse.from_prolog(term)

    # ========================================================================
    # SESSION MANAGEMENT
    # ========================================================================

    def session_create(self, session_id: str) -> GenericResponse:
        """Create a new session"""
        return self._magic_cast(
            "conjure(interface(session_create(session_id(SessionId))))",
            {"SessionId": session_id},
            GenericResponse
        )

    def session_switch(self, session_id: str) -> GenericResponse:
        """Switch to different session"""
        return self._magic_cast(
            "conjure(interface(session_switch(session_id(SessionId))))",
            {"SessionId": session_id},
            GenericResponse
        )

    def session_delete(self, session_id: str) -> GenericResponse:
        """Delete a session"""
        return self._magic_cast(
            "conjure(interface(session_delete(session_id(SessionId))))",
            {"SessionId": session_id},
            GenericResponse
        )

    def session_export(self, session_id: str, destination: str) -> GenericResponse:
        """Export session to archive"""
        return self._magic_cast(
            "conjure(interface(session_export(session_id(SessionId), destination(Dest))))",
            {"SessionId": session_id, "Dest": destination},
            GenericResponse
        )

    def session_import(self, archive: str) -> GenericResponse:
        """Import session from archive"""
        return self._magic_cast(
            "conjure(interface(session_import(archive(Archive))))",
            {"Archive": archive},
            GenericResponse
        )

    def session_focus_entity(self, entity: str) -> GenericResponse:
        """Focus on entity by name"""
        return self._magic_cast(
            "conjure(interface(session_focus_entity(entity(Entity))))",
            {"Entity": entity},
            GenericResponse
        )

    def session_focus_path(self, path: str) -> GenericResponse:
        """Focus on entity by path"""
        return self._magic_cast(
            "conjure(interface(session_focus_path(path(Path))))",
            {"Path": path},
            GenericResponse
        )

    def session_get_focused(self) -> GenericResponse:
        """Get focused entity with structured information"""
        return self._magic_cast(
            "perceive(interface(session_focused))",
            {},
            GenericResponse
        )

    def session_unfocus(self) -> GenericResponse:
        """Clear focused entity"""
        return self._magic_cast(
            "conjure(interface(session_unfocus))",
            {},
            GenericResponse
        )

    def session_status(self) -> GenericResponse:
        """Get session status including focused entity"""
        return self._magic_cast(
            "perceive(interface(session_status))",
            {},
            GenericResponse
        )

    def session_context(self) -> SessionContextResponse:
        """Get comprehensive session context for LLM state recovery"""
        return self._magic_cast(
            "perceive(interface(session_context))",
            {},
            SessionContextResponse
        )

    # ========================================================================
    # SKILL SYSTEM
    # ========================================================================

    def do(self, skill_term: str, entity: Optional[str] = None) -> GenericResponse:
        """Invoke a skill on an entity (defaults to focused entity or 'system')

        Args:
            skill_term: Skill term as a string (e.g., "nix(build(foo))")
            entity: Optional entity name (defaults to focused entity or 'system')

        Returns:
            GenericResponse with skill execution result

        Example:
            grimoire.do("nix(build(my_package))", entity="my_project")
            grimoire.do("git(commit)")  # Uses focused entity
        """
        resolved_entity = self._resolve_entity(entity)
        return self._magic_cast(
            "conjure(interface(invoke_skill(entity(Entity), skill(SkillTerm))))",
            {"Entity": resolved_entity, "SkillTerm": skill_term},
            GenericResponse
        )

    def skills(self, entity: Optional[str] = None) -> GenericResponse:
        """List all available skills for an entity (defaults to focused entity or 'system')

        Args:
            entity: Optional entity name (defaults to focused entity or 'system')

        Returns:
            GenericResponse with list of skills

        Example:
            grimoire.skills("my_project")
            grimoire.skills()  # Uses focused entity
        """
        resolved_entity = self._resolve_entity(entity)
        return self._magic_cast(
            "perceive(interface(skills(entity(Entity))))",
            {"Entity": resolved_entity},
            GenericResponse
        )

    # ========================================================================
    # PROJECT INITIALIZATION
    # ========================================================================

    def init(self, path: str = '.', force: bool = False) -> GenericResponse:
        """Initialize Grimoire for existing project

        Creates semantics.pl/.plt files, detects git/nix infrastructure,
        creates 'default' session, and focuses on the new entity.

        Args:
            path: Project directory path (default: current directory)
            force: Overwrite existing semantics.pl if it exists

        Returns:
            GenericResponse with initialization result
        """
        import os
        abs_path = os.path.abspath(path)
        options = ['force'] if force else []

        return self._magic_cast(
            "conjure(interface(init(folder(Folder), options(Options))))",
            {"Folder": abs_path, "Options": options},
            GenericResponse
        )

    # ========================================================================
    # META-INTROSPECTION
    # ========================================================================

    def prove_it(self, entity: str, comp_type: str, value: Any) -> GenericResponse:
        """Component provenance - where generated and how verified"""
        return self._magic_cast(
            "perceive(interface(prove_it(entity(Entity), type(Type), value(Value))))",
            {"Entity": entity, "Type": comp_type, "Value": value},
            GenericResponse
        )

    def sauce_me(self, spell_ctor: str) -> GenericResponse:
        """Spell metadata - source location, implementation, formats"""
        return self._magic_cast(
            "perceive(interface(sauce_me(spell_ctor(SpellCtor))))",
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
    # CRUD COMPONENT OPERATIONS
    # ========================================================================

    def add_component(self, component_type: str, value: str, entity: Optional[str] = None) -> GenericResponse:
        """Add component to entity (defaults to focused entity)

        Args:
            component_type: Component type to add
            value: Component value (Prolog term as string)
            entity: Optional entity name (defaults to focused entity)

        Returns:
            GenericResponse with operation result

        Example:
            grimoire.add_component("config_key", "value123")
            grimoire.add_component("config_key", "value123", entity="my_entity")
        """
        # Use focused_entity if no entity specified
        target_entity = entity if entity else "focused_entity"
        return self._magic_cast(
            "conjure(interface(add_component(entity(Entity), component_type(Type), value(Value))))",
            {"Entity": target_entity, "Type": component_type, "Value": value},
            GenericResponse
        )

    def remove_component(self, component_type: str, value: str, entity: Optional[str] = None) -> GenericResponse:
        """Remove component from entity (defaults to focused entity)

        Args:
            component_type: Component type to remove
            value: Component value (Prolog term as string)
            entity: Optional entity name (defaults to focused entity)

        Returns:
            GenericResponse with operation result

        Example:
            grimoire.remove_component("config_key", "value123")
            grimoire.remove_component("config_key", "value123", entity="my_entity")
        """
        # Use focused_entity if no entity specified
        target_entity = entity if entity else "focused_entity"
        return self._magic_cast(
            "conjure(interface(remove_component(entity(Entity), component_type(Type), value(Value))))",
            {"Entity": target_entity, "Type": component_type, "Value": value},
            GenericResponse
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
        response = self._magic_cast(
            "conjure(interface(exec(query(QueryString))))",
            {"QueryString": query_str},
            ExecResponse
        )
        return response.solutions
