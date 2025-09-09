"""
Anthropic (Claude) LLM provider implementation.
"""

import os
from typing import Dict, List, Any, Optional
from anthropic import Anthropic
from .base import BaseLLMProvider, LLMMessage, LLMResponse


class AnthropicProvider(BaseLLMProvider):
    """
    Anthropic (Claude) provider implementation.
    
    Supports Claude's native tool calling and structured outputs.
    """
    
    def __init__(self, config: Dict[str, Any]):
        super().__init__(config)
        api_key = os.getenv('ANTHROPIC_API_KEY')
        if not api_key:
            raise ValueError("ANTHROPIC_API_KEY environment variable not set")
        self.client = Anthropic(api_key=api_key)
    
    def chat_completion(self, messages: List[LLMMessage], tools: Optional[List[Dict]] = None) -> LLMResponse:
        """Generate a chat completion using Claude."""
        # Convert messages to Anthropic format
        anthropic_messages = []
        system_message = None
        
        for msg in messages:
            if msg.role == "system":
                system_message = msg.content
            else:
                anthropic_messages.append({
                    "role": msg.role,
                    "content": msg.content
                })
        
        # Prepare request parameters
        request_params = {
            "model": self.model,
            "messages": anthropic_messages,
            "max_tokens": self.max_tokens,
            "temperature": self.temperature
        }
        
        if system_message:
            request_params["system"] = system_message
        
        # Add tools if provided
        if tools:
            request_params["tools"] = self._convert_tools_to_anthropic_format(tools)
        
        # Make API call
        response = self.client.messages.create(**request_params)
        
        # Extract content and tool calls
        content = ""
        tool_calls = []
        
        for block in response.content:
            if block.type == "text":
                content = block.text
            elif block.type == "tool_use":
                tool_calls.append({
                    "id": block.id,
                    "name": block.name,
                    "arguments": block.input
                })
        
        return LLMResponse(
            content=content,
            tool_calls=tool_calls if tool_calls else None,
            finish_reason=response.stop_reason or "stop",
            usage={
                "input_tokens": response.usage.input_tokens,
                "output_tokens": response.usage.output_tokens,
                "total_tokens": response.usage.input_tokens + response.usage.output_tokens
            }
        )
    
    def supports_tools(self) -> bool:
        """Anthropic supports native tool calling."""
        return True
    
    def supports_streaming(self) -> bool:
        """Anthropic supports streaming responses."""
        return True
    
    def _convert_tools_to_anthropic_format(self, tools: List[Dict]) -> List[Dict]:
        """Convert generic tool format to Anthropic's format."""
        anthropic_tools = []
        
        for tool in tools:
            anthropic_tool = {
                "name": tool.get("name"),
                "description": tool.get("description"),
                "input_schema": tool.get("parameters", {
                    "type": "object",
                    "properties": {},
                    "required": []
                })
            }
            anthropic_tools.append(anthropic_tool)
        
        return anthropic_tools