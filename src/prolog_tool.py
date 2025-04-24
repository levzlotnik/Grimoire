from itertools import chain
import re
from pyswip import Prolog as p
from pyswip.prolog import PrologError
from typing import List, Dict, Any, Optional, Union
from pydantic import BaseModel, Field
import os

from common import wrap_tool_error


class CommandResult(BaseModel):
    """Result of executing a system action"""

    success: bool = Field(..., description="Whether the action was successful")
    message: Optional[str] = Field(
        None, description="Error message if the action failed"
    )


class TransactionResult(BaseModel):
    """Result of executing a transaction"""

    success: bool = Field(..., description="Whether the transaction succeeded")
    results: List[Any] = Field(
        default_factory=list, description="Results from each command"
    )
    error: Optional[str] = Field(
        None, description="Error message if transaction failed"
    )

    @staticmethod
    def from_command_result(command_result: CommandResult) -> "TransactionResult":
        return TransactionResult(
            success=command_result.success,
            results=[command_result.message],
            error=None if command_result.success else command_result.message,
        )


def _parse_command_result(result: str) -> CommandResult:
    command_result_regex = r"^(ok|error)\((.*)\)$"
    # Match the result against the regex
    match = re.match(command_result_regex, result)
    if match:
        status = match.group(1)
        message = match.group(2)
        if status == "ok":
            return CommandResult(success=True, message=message)
        else:
            return CommandResult(success=False, message=message)
    else:
        raise PrologError(f"Invalid command result format: {result}")


class EntityInfo(BaseModel):
    name: str = Field(..., description="Entity name")
    docstring: Optional[str] = Field(None, description="Entity docstring")


class EntitiesResult(BaseModel):
    entities: List[EntityInfo]


class ComponentsTypesResult(BaseModel):
    component_types: List[str]


class ComponentsResult(BaseModel):
    components: List[Any]


class ConstructorsResult(BaseModel):
    constructors: List[str]


class DocstringResult(BaseModel):
    docstring: Optional[str]


class BooleanResult(BaseModel):
    result: bool


class MountedSemanticsResult(BaseModel):
    paths: List[str]


class SystemConceptsResult(BaseModel):
    concepts: Dict[str, Optional[str]]


class PrologToolset:
    """Engine for executing queries and transactions in the semantic layer"""

    def __init__(self, ask_user_consent: bool = True):
        self.ask_user_consent = ask_user_consent
        rules_dir = os.path.join(os.path.dirname(__file__), "prolog")
        p.consult(os.path.join(rules_dir, "mypaos.pl"))

    def _get_user_consent(self, transaction: str) -> bool:
        if not self.ask_user_consent:
            return True
        print("\nProposed transaction:")
        print(transaction)
        response = input("\nExecute this transaction? [y/N] ").lower()
        return response == "y"

    @wrap_tool_error(exc_types=(PrologError,))
    def commit_transaction(self, commands: List[str]) -> TransactionResult:
        """Execute a transaction containing multiple commands atomically.

        Args:
            commands: List of Prolog commands to execute in the transaction.

        Example:
            {
                "tool_name": "commit_transaction",
                "parameters": {
                    "commands": [
                        "mkdir('test_dir')",
                        "mkfile('test_dir/test_file.txt')",
                        "edit_file('test_dir/test_file.txt', [append('Hello World!')])"
                    ]
                },
                "reason": "Execute multiple commands in a transaction."
            }
        """
        transaction = f"transaction([{','.join(commands)}])"

        if not self._get_user_consent(transaction):
            return TransactionResult(
                success=False, error="Transaction cancelled by user"
            )

        results = list(p.query(f"execute({transaction}, RetVal)"))
        if not results:
            return TransactionResult(
                success=False, error="Transaction execution failed"
            )

        result = results[0]["RetVal"]
        return TransactionResult.from_command_result(_parse_command_result(result))

    @wrap_tool_error(exc_types=(PrologError,))
    def list_entities(self) -> EntitiesResult:
        """List all entities in the system with docstrings.

        Args:
            None
        Example:
            {
                "tool_name": "list_entities",
                "parameters": {},
                "reason": "List all entities in the system"
            }
        """
        results = p.query("entity(E)")
        if not results:
            results = []
        else:
            results = list(results)
        entities = []
        for r in results:
            entity = r["E"]
            doc_result = self.get_docstring(entity)
            entities.append(EntityInfo(name=entity, docstring=doc_result.docstring))
        return EntitiesResult(entities=entities)

    @wrap_tool_error(exc_types=(PrologError,))
    def list_components_types(self, entity: str) -> ComponentsTypesResult:
        """List all components and their values for an entity.

        Args:
            entity: The entity ID to list components for.

        Example:
            {
                "tool_name": "list_components_types",
                "parameters": {"entity": "git"},
                "reason": "List all components of the git subsystem"
            }
        """
        results = p.query(f"component({entity}, CName, _)")
        types = [r["CName"] for r in results] if results else []
        return ComponentsTypesResult(component_types=types)

    @wrap_tool_error(exc_types=(PrologError,))
    def list_components(self, entity: str, component_type: str) -> ComponentsResult:
        """List all components and their values for an entity.

        Args:
            entity: The entity ID to list components for.
            component_type: The component type to list.

        Example:
            {
                "tool_name": "list_components",
                "parameters": {"entity": "git", "component_type": "branch"},
                "reason": "List all branches of the git repository."
            }
        """
        results = p.query(f"component({entity}, {component_type}, Component)")
        comps = [r["Component"] for r in results] if results else []
        return ComponentsResult(components=comps)

    @wrap_tool_error(exc_types=(PrologError,))
    def get_docstring(self, entity: str) -> DocstringResult:
        """Get documentation for an entity, component, or constructor.

        Args:
            entity: The entity ID to get documentation for.

        Example:
            {
                "tool_name": "get_docstring",
                "parameters": {"entity": "git"},
                "reason": "Get documentation for the git subsystem."
            }
        """
        results = p.query(f"docstring({entity}, Doc)")
        if not results:
            results = []
        else:
            results = list(results)
        # Result is a byte string, convert to string
        doc = results[0]["Doc"].decode("utf-8") if results else None
        return DocstringResult(docstring=doc)

    # @wrap_tool_error(exc_types=(PrologError,))
    # def mount_semantic(self, path: str) -> BooleanResult:
    #     """Mount a semantic knowledge domain"""
    #     results = p.query(f"mount_semantic({path})")
    #     return BooleanResult(result=bool(results))

    # @wrap_tool_error(exc_types=(PrologError,))
    # def list_mounted_semantics(self) -> MountedSemanticsResult:
    #     """List all mounted semantic domains"""
    #     results = p.query("list_mounted_semantics(Paths)")
    #     paths = results[0]["Paths"] if results else []
    #     return MountedSemanticsResult(paths=paths)

    @wrap_tool_error(exc_types=(PrologError,))
    def load_entity_source(self, entity_id: str) -> BooleanResult:
        """Load the source code for an entity.
        This is applicable to entities that have a `source` component -
        typically, this will contain additional components for that entity and loading
        the source will make them available for querying.

        Args:
            entity_id: The entity ID to load the source code for.

        Example:
            {
                "tool_name": "load_entity_source",
                "parameters": {"entity_id": "git"},
                "reason": "Load the component tree for the git subsystem."
            }
        """
        results = p.query(f"load_entity_source({entity_id})")
        return BooleanResult(result=bool(results))

    @wrap_tool_error(exc_types=(PrologError,))
    def get_system_concepts(self) -> SystemConceptsResult:
        """Get all system concepts"""
        comps_result = self.list_components("system", "concept")
        comps = comps_result.components
        concept_docs = {c: self.get_docstring(c).docstring for c in comps}
        return SystemConceptsResult(concepts=concept_docs)

    @wrap_tool_error(exc_types=(PrologError,))
    def is_entity(self, entity: str) -> BooleanResult:
        """Check if something is an entity in the system.

        Args:
            entity: The entity ID to check.
        Example:
            {
                "tool_name": "is_entity",
                "parameters": {"entity": "git"},
                "reason": "Check if git is an entity."
            }
        """
        results = list(p.query(f"entity({entity})"))
        if len(results) == 0:
            return BooleanResult(result=False)
        else:
            return BooleanResult(result=True)


if __name__ == "__main__":
    prolog_tool = PrologToolset()
    print(prolog_tool.list_entities())
    print(prolog_tool.list_components_types("system"))
    print(prolog_tool.list_components("system", "concept"))
    print(0)
