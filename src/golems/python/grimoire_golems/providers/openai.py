"""
OpenAI LLM provider implementation.
"""

import os
from typing import Dict, List, Any, Optional
from openai import OpenAI
from .base import BaseLLMProvider, LLMMessage, LLMResponse


class OpenAIProvider(BaseLLMProvider):
    """
    OpenAI provider implementation.
    
    Supports GPT models with native function calling and JSON mode.
    """
    
    def __init__(self, config: Dict[str, Any]):
        super().__init__(config)
        api_key = os.getenv('OPENAI_API_KEY')
        if not api_key:
            raise ValueError("OPENAI_API_KEY environment variable not set")
        self.client = OpenAI(api_key=api_key)
    
    def chat_completion(self, messages: List[LLMMessage], tools: Optional[List[Dict]] = None) -> LLMResponse:
        """Generate a chat completion using OpenAI."""
        # Convert messages to OpenAI format
        openai_messages = []
        
        for msg in messages:
            message_dict = {
                "role": msg.role,
                "content": msg.content
            }
            if msg.tool_calls:
                message_dict["tool_calls"] = msg.tool_calls
            if msg.tool_call_id:
                message_dict["tool_call_id"] = msg.tool_call_id
            openai_messages.append(message_dict)
        
        # Prepare request parameters
        request_params = {
            "model": self.model,
            "messages": openai_messages,
            "max_tokens": self.max_tokens,
            "temperature": self.temperature
        }
        
        # Add tools if provided
        if tools:
            request_params["tools"] = self._convert_tools_to_openai_format(tools)
            request_params["tool_choice"] = "auto"
        
        # Make API call
        response = self.client.chat.completions.create(**request_params)
        
        # Extract response
        choice = response.choices[0]
        message = choice.message
        
        # Extract tool calls if present
        tool_calls = None
        if message.tool_calls:
            tool_calls = [
                {
                    "id": tc.id,
                    "name": tc.function.name,
                    "arguments": tc.function.arguments
                }
                for tc in message.tool_calls
            ]
        
        return LLMResponse(
            content=message.content or "",
            tool_calls=tool_calls,
            finish_reason=choice.finish_reason,
            usage={
                "prompt_tokens": response.usage.prompt_tokens,
                "completion_tokens": response.usage.completion_tokens,
                "total_tokens": response.usage.total_tokens
            }
        )
    
    def supports_tools(self) -> bool:
        """OpenAI supports native function calling."""
        return True
    
    def supports_streaming(self) -> bool:
        """OpenAI supports streaming responses."""
        return True
    
    def _convert_tools_to_openai_format(self, tools: List[Dict]) -> List[Dict]:
        """Convert generic tool format to OpenAI's format."""
        openai_tools = []
        
        for tool in tools:
            openai_tool = {
                "type": "function",
                "function": {
                    "name": tool.get("name"),
                    "description": tool.get("description"),
                    "parameters": tool.get("parameters", {
                        "type": "object",
                        "properties": {},
                        "required": []
                    })
                }
            }
            openai_tools.append(openai_tool)
        
        return openai_tools