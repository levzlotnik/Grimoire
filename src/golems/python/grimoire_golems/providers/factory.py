"""
LLM provider factory for creating provider instances.

Handles provider instantiation based on configuration,
with graceful fallback for missing dependencies.
"""

from typing import Dict, Any, Optional
from .base import BaseLLMProvider


class MockLLMProvider(BaseLLMProvider):
    """
    Mock LLM provider for testing and development.
    
    Used when actual provider dependencies are not available
    or for testing scenarios.
    """
    
    def chat_completion(self, messages, tools=None):
        from .base import LLMResponse
        
        # Generate mock response based on the last user message
        last_message = messages[-1] if messages else None
        content = f"Mock response to: {last_message.content if last_message else 'empty'}"
        
        return LLMResponse(
            content=content,
            tool_calls=None,
            finish_reason="stop",
            usage={"total_tokens": 50, "prompt_tokens": 25, "completion_tokens": 25}
        )
    
    def supports_tools(self) -> bool:
        return True
    
    def supports_streaming(self) -> bool:
        return False


def llm_provider_factory(config: Dict[str, Any]) -> BaseLLMProvider:
    """
    Create an LLM provider instance based on configuration.
    
    Args:
        config: Provider configuration dictionary with 'provider' key
        
    Returns:
        BaseLLMProvider instance (may be mock if dependencies unavailable)
    """
    provider_name = config.get('provider', 'mock').lower()
    
    try:
        if provider_name == 'openai':
            return _create_openai_provider(config)
        elif provider_name == 'anthropic':
            return _create_anthropic_provider(config)
        elif provider_name == 'groq':
            return _create_groq_provider(config)
        elif provider_name == 'ollama':
            return _create_ollama_provider(config)
        else:
            # Default to mock provider
            return MockLLMProvider(config)
    except ImportError as e:
        # Fallback to mock provider if dependencies are missing
        print(f"Warning: Failed to import {provider_name} provider ({e}), using mock provider")
        return MockLLMProvider(config)
    except Exception as e:
        # Fallback to mock provider for any other errors
        print(f"Warning: Failed to create {provider_name} provider ({e}), using mock provider")
        return MockLLMProvider(config)


def _create_openai_provider(config: Dict[str, Any]) -> BaseLLMProvider:
    """Create OpenAI provider."""
    from .openai import OpenAIProvider
    return OpenAIProvider(config)


def _create_anthropic_provider(config: Dict[str, Any]) -> BaseLLMProvider:
    """Create Anthropic provider."""
    from .anthropic import AnthropicProvider
    return AnthropicProvider(config)


def _create_groq_provider(config: Dict[str, Any]) -> BaseLLMProvider:
    """Create Groq provider (placeholder - will implement when needed)."""
    # For now, return mock provider
    # TODO: Implement actual Groq provider
    return MockLLMProvider(config)


def _create_ollama_provider(config: Dict[str, Any]) -> BaseLLMProvider:
    """Create Ollama provider (placeholder - will implement when needed)."""
    # For now, return mock provider
    # TODO: Implement actual Ollama provider
    return MockLLMProvider(config)