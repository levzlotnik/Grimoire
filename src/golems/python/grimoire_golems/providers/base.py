"""
Base LLM provider interface.

Defines the common interface that all LLM providers must implement,
ensuring consistent behavior across different AI services.
"""

from abc import ABC, abstractmethod
from typing import Dict, List, Any, Optional
from pydantic import BaseModel


class LLMMessage(BaseModel):
    """Structured message for LLM communication."""
    role: str  # 'system', 'user', 'assistant', 'tool'
    content: str
    tool_calls: Optional[List[Dict]] = None
    tool_call_id: Optional[str] = None


class LLMResponse(BaseModel):
    """Structured response from LLM provider."""
    content: str
    tool_calls: Optional[List[Dict]] = None
    finish_reason: str  # 'stop', 'tool_calls', 'length', 'content_filter'
    usage: Optional[Dict] = None


class BaseLLMProvider(ABC):
    """
    Base class for all LLM providers.
    
    Provides a consistent interface for different AI services
    while allowing provider-specific implementations.
    """
    
    def __init__(self, config: Dict[str, Any]):
        self.config = config
        self.model = config.get('model')
        self.max_tokens = config.get('max_tokens', 4096)
        self.temperature = config.get('temperature', 0.1)
        
    @abstractmethod
    def chat_completion(self, messages: List[LLMMessage], tools: Optional[List[Dict]] = None) -> LLMResponse:
        """
        Generate a chat completion response.
        
        Args:
            messages: List of messages in the conversation
            tools: Optional list of available tools/functions
            
        Returns:
            LLMResponse with the model's response
        """
        pass
    
    @abstractmethod
    def supports_tools(self) -> bool:
        """Return whether this provider supports tool/function calling."""
        pass
    
    @abstractmethod
    def supports_streaming(self) -> bool:
        """Return whether this provider supports streaming responses."""
        pass
    
    def execute_task(self, role: str, inputs: List, tools: List[Dict], 
                    input_schema: List, output_schema: List) -> Dict[str, Any]:
        """
        Execute a task using this LLM provider.
        
        This is a default implementation that can be overridden by specific providers.
        
        Args:
            role: Role description for the LLM
            inputs: Input data for the task
            tools: Available tools/functions
            input_schema: Expected input schema
            output_schema: Expected output schema
            
        Returns:
            Task execution result
        """
        try:
            # Build system message with role and schema information
            system_message = self._build_system_message(role, tools, input_schema, output_schema)
            
            # Build user message with inputs
            user_message = self._build_user_message(inputs)
            
            messages = [
                LLMMessage(role="system", content=system_message),
                LLMMessage(role="user", content=user_message)
            ]
            
            # Get response from LLM
            response = self.chat_completion(messages, tools if self.supports_tools() else None)
            
            return {
                "success": True,
                "content": response.content,
                "tool_calls": response.tool_calls,
                "finish_reason": response.finish_reason,
                "usage": response.usage
            }
            
        except Exception as e:
            return {
                "success": False,
                "error": str(e),
                "error_type": type(e).__name__
            }
    
    def _build_system_message(self, role: str, tools: List[Dict], 
                            input_schema: List, output_schema: List) -> str:
        """Build system message with role and schema information."""
        message_parts = [f"Role: {role}"]
        
        if input_schema:
            schema_str = ', '.join(str(schema) for schema in input_schema)
            message_parts.append(f"Input Schema: {schema_str}")
        
        if output_schema:
            schema_str = ', '.join(str(schema) for schema in output_schema)
            message_parts.append(f"Output Schema: {schema_str}")
        
        if tools:
            message_parts.append(f"Available Tools: {len(tools)} tools available for task execution")
            
        return '\n\n'.join(message_parts)
    
    def _build_user_message(self, inputs: List) -> str:
        """Build user message from inputs."""
        if len(inputs) == 1:
            return str(inputs[0])
        else:
            return f"Task inputs: {inputs}"