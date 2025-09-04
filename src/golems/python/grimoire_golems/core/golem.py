"""
Core golem class with GrimoireInterface integration and LLM provider support.

This class demonstrates the Python side of the golem bridge pattern.
It integrates with the existing GrimoireInterface to auto-discover available tools
and provides LLM provider abstraction for different AI services.
"""

import os
import sys
import inspect
from typing import Dict, List, Any, Optional
from pydantic import BaseModel

# Add the Grimoire interface path to sys.path
grimoire_root = os.getenv('GRIMOIRE_ROOT', '/home/levz/Projects/Grimoire')
interface_path = os.path.join(grimoire_root, 'src', 'interface', 'api')
if interface_path not in sys.path:
    sys.path.insert(0, interface_path)

from grimoire_interface import GrimoireInterface


class TaskResult(BaseModel):
    """Result of golem task execution."""
    success: bool
    output: Any
    metadata: Dict[str, Any] = {}


class Golem:
    """
    Core golem class with GrimoireInterface integration and LLM provider.
    
    Demonstrates:
    - Integration with existing GrimoireInterface for tool discovery
    - LLM provider abstraction for multiple AI services
    - Structured task execution with type-safe results
    - Session management integration
    """
    
    def __init__(self, golem_id: str, llm_config: Dict, role: str, session_id: str):
        self.golem_id = golem_id
        self.role = role
        self.session_id = session_id
        self.llm_config = llm_config
        
        # Initialize Grimoire interface
        self.grimoire_interface = GrimoireInterface()
        
        # Initialize LLM provider from config (will implement providers next)
        self.llm_provider = None  # Placeholder
    
    def tools(self) -> List[Dict]:
        """Return tools in a format that can be converted to Prolog and processed by LLM provider"""
        tools = []
        
        # Auto-discover public methods from GrimoireInterface
        for name, method in inspect.getmembers(self.grimoire_interface, predicate=inspect.ismethod):
            if name.startswith('_'):
                continue
                
            # Extract method metadata
            sig = inspect.signature(method)
            doc = inspect.getdoc(method) or f"Execute {name}"
            
            # Create tool definition compatible with both Prolog and LLM providers
            tool = {
                'name': f'grimoire_{name}',
                'description': doc.split('\n')[0],  # First line of docstring
                'parameters': self._sig_to_schema(sig),
                'prolog_callable': name  # For Prolog bridge
            }
            tools.append(tool)
        
        return tools
    
    def _sig_to_schema(self, sig: inspect.Signature) -> Dict:
        """Convert Python signature to JSON schema"""
        properties = {}
        required = []
        
        for param_name, param in sig.parameters.items():
            if param_name == 'self':
                continue
            
            # Infer type from annotation
            param_type = 'string'  # default
            if param.annotation != inspect.Parameter.empty:
                if param.annotation == int:
                    param_type = 'integer'
                elif param.annotation == bool:
                    param_type = 'boolean'
                elif hasattr(param.annotation, '__origin__'):
                    if param.annotation.__origin__ == list:
                        param_type = 'array'
            
            properties[param_name] = {'type': param_type}
            
            if param.default == inspect.Parameter.empty:
                required.append(param_name)
        
        return {
            'type': 'object',
            'properties': properties,
            'required': required
        }

    def execute_tool(self, tool_name: str, parameters: Dict[str, Any]) -> Dict[str, Any]:
        """Execute a tool by calling the corresponding GrimoireInterface method"""
        # Remove 'grimoire_' prefix to get actual method name
        method_name = tool_name.replace('grimoire_', '', 1)
        
        if hasattr(self.grimoire_interface, method_name):
            try:
                method = getattr(self.grimoire_interface, method_name)
                result = method(**parameters)
                return {"success": True, "result": result}
            except Exception as e:
                return {"success": False, "error": str(e)}
        else:
            return {"success": False, "error": f"Unknown tool: {tool_name}"}

    def start_task(self, inputs: List, input_schema: List, output_schema: List):
        """Start executing a task with the LLM provider"""
        # For now, return a placeholder implementation
        return TaskResult(
            success=True,
            output=f"Golem {self.golem_id} would process inputs: {inputs}",
            metadata={
                "golem_id": self.golem_id,
                "session_id": self.session_id,
                "role": self.role,
                "input_schema": input_schema,
                "output_schema": output_schema,
                "tools_available": len(self.tools())
            }
        )