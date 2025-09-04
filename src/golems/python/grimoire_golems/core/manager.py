"""
Golem task execution manager.

Handles task execution orchestration, session management,
and coordination between Prolog and Python layers.
"""

from typing import Dict, List, Any, Optional
from .golem import Golem, TaskResult


class GolemManager:
    """
    Manages golem task execution and lifecycle.
    
    Provides the main entry point for task execution from Prolog,
    handling golem instantiation, task orchestration, and result management.
    """
    
    def __init__(self):
        self.active_golems = {}  # golem_id -> Golem instance
        self.active_tasks = {}   # task_id -> task info
    
    def execute_task(self, golem_id: str, llm_config: Dict, role: str, 
                    input_schema: List, output_schema: List, inputs: List) -> Dict[str, Any]:
        """
        Execute a task with the specified golem configuration.
        
        Called from Prolog via execute_golem_task/7.
        
        Args:
            golem_id: Unique identifier for the golem
            llm_config: LLM configuration dictionary
            role: Role description for the golem
            input_schema: Expected input schema
            output_schema: Expected output schema
            inputs: Actual input data
            
        Returns:
            Result dictionary indicating success/failure and task information
        """
        try:
            # Create golem instance if not exists
            if golem_id not in self.active_golems:
                # For now, use a placeholder session_id
                session_id = f"session_{golem_id}"
                golem = Golem(golem_id, llm_config, role, session_id)
                self.active_golems[golem_id] = golem
            else:
                golem = self.active_golems[golem_id]
            
            # Start task execution
            task_result = golem.start_task(inputs, input_schema, output_schema)
            
            if task_result.success:
                # Store task info for tracking
                task_id = f"task_{golem_id}_{len(self.active_tasks)}"
                self.active_tasks[task_id] = {
                    "golem_id": golem_id,
                    "inputs": inputs,
                    "result": task_result,
                    "status": "completed"
                }
                
                return {
                    "type": "success", 
                    "task_id": task_id,
                    "output": task_result.output,
                    "metadata": task_result.metadata
                }
            else:
                return {
                    "type": "error",
                    "reason": "task_execution_failed",
                    "details": task_result.metadata
                }
                
        except Exception as e:
            return {
                "type": "error",
                "reason": "exception",
                "details": str(e)
            }
    
    def get_golem_tools(self, golem_id: str, llm_config: Dict, role: str) -> List[Dict]:
        """
        Get available tools for a golem.
        
        Args:
            golem_id: Unique identifier for the golem
            llm_config: LLM configuration dictionary  
            role: Role description for the golem
            
        Returns:
            List of tool definitions
        """
        try:
            # Create temporary golem instance for tool discovery
            session_id = f"temp_{golem_id}"
            golem = Golem(golem_id, llm_config, role, session_id)
            return golem.tools()
        except Exception as e:
            # Return empty tools list on error
            return []
    
    def cleanup_golem(self, golem_id: str) -> bool:
        """
        Cleanup golem instance and associated tasks.
        
        Args:
            golem_id: Unique identifier for the golem to cleanup
            
        Returns:
            True if cleanup successful, False otherwise
        """
        try:
            if golem_id in self.active_golems:
                del self.active_golems[golem_id]
            
            # Cleanup associated tasks
            tasks_to_remove = [task_id for task_id, task_info in self.active_tasks.items()
                             if task_info.get("golem_id") == golem_id]
            
            for task_id in tasks_to_remove:
                del self.active_tasks[task_id]
                
            return True
        except Exception:
            return False


# Global manager instance for use by Prolog bridge
_global_manager = GolemManager()

def execute_task(golem_id: str, llm_config: Dict, role: str, 
                input_schema: List, output_schema: List, inputs: List) -> Dict[str, Any]:
    """Global function for task execution called from Prolog."""
    return _global_manager.execute_task(golem_id, llm_config, role, input_schema, output_schema, inputs)