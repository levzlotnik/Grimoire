"""
Core domain service implementation.

This class demonstrates the Python side of the bridge pattern.
It provides deterministic construction from Prolog data and
exposes methods that can be called from the Prolog bridge.
"""

from typing import List, Dict, Any
from pydantic import BaseModel


class TaskResult(BaseModel):
    """Result of domain task execution."""
    success: bool
    output: Any
    metadata: Dict[str, Any] = {}


class DomainService:
    """
    Example domain service that can be instantiated from Prolog.
    
    Demonstrates:
    - Deterministic construction from entity identifiers
    - Tool/operation discovery interface
    - Task execution with structured results
    """
    
    def __init__(self, entity_id: str):
        self.entity_id = entity_id
        self.operations = {
            "process_data": "Process input data according to domain rules",
            "validate_input": "Validate input against domain constraints", 
            "transform": "Transform data using domain-specific logic"
        }
    
    def get_available_operations(self) -> List[Dict[str, str]]:
        """
        Return available operations for this service.
        
        This method is called from Prolog to discover capabilities.
        Format matches what LLM providers expect for tool definitions.
        """
        return [
            {
                "name": op_name,
                "description": description,
                "parameters": {
                    "type": "object",
                    "properties": {
                        "input": {"type": "string", "description": "Input data to process"}
                    },
                    "required": ["input"]
                }
            }
            for op_name, description in self.operations.items()
        ]
    
    def execute_task(self, task_input: Any) -> TaskResult:
        """
        Execute a domain task with the given input.
        
        Args:
            task_input: Input data for the task
            
        Returns:
            TaskResult with success status and output
        """
        # Example domain logic
        if isinstance(task_input, str) and task_input.strip():
            return TaskResult(
                success=True,
                output=f"Processed: {task_input}",
                metadata={"entity_id": self.entity_id, "method": "execute_task"}
            )
        else:
            return TaskResult(
                success=False,
                output="Invalid input: empty or non-string",
                metadata={"entity_id": self.entity_id, "error": "validation_failed"}
            )
    
    def process_data(self, input_data: str) -> Dict[str, Any]:
        """Specific operation implementation."""
        return {
            "result": f"Data processed: {input_data}",
            "entity": self.entity_id,
            "operation": "process_data"
        }
    
    def validate_input(self, input_data: str) -> Dict[str, Any]:
        """Input validation operation."""
        is_valid = isinstance(input_data, str) and len(input_data.strip()) > 0
        return {
            "valid": is_valid,
            "input": input_data,
            "entity": self.entity_id,
            "operation": "validate_input"
        }
    
    def transform(self, input_data: str) -> Dict[str, Any]:
        """Data transformation operation."""
        return {
            "transformed": input_data.upper() if isinstance(input_data, str) else str(input_data),
            "original": input_data,
            "entity": self.entity_id,
            "operation": "transform"
        }