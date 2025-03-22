import os
from abc import ABC, abstractmethod
from typing import List, Dict, Optional, Union, Literal, Any
import warnings
import openai
import anthropic
import groq
from dataclasses import dataclass


@dataclass
class LLMResponse:
    """Standardized response from any LLM provider"""

    content: str
    model: str
    usage: Dict[str, int]  # tokens used for input, output, and total
    tool_calls: Optional[Any] = None  # Store provider-specific tool call format


class LLMProvider(ABC):
    """Abstract base class for LLM providers"""

    @abstractmethod
    def chat_completion(
        self,
        messages: List[Dict[str, str]],
        model: str,
        tools: Optional[List[Dict]] = None,
        temperature: float = 0.7,
        max_tokens: Optional[int] = None,
    ) -> LLMResponse:
        """
        Send a chat completion request to the LLM provider

        Args:
            messages: List of message dictionaries with 'role' and 'content' keys
            model: Model identifier string
            tools: List of tool dictionaries
            temperature: Sampling temperature (0-1)
            max_tokens: Maximum tokens to generate

        Returns:
            LLMResponse object containing the response and usage statistics
        """


class OpenAIProvider(LLMProvider):
    """OpenAI API provider implementation"""

    def __init__(self, api_key: Optional[str] = None):
        self.client = openai.OpenAI(api_key=api_key or os.getenv("OPENAI_API_KEY"))

    def chat_completion(
        self,
        messages: List[Dict[str, str]],
        model: str = "gpt-4o",
        tools: Optional[List[Dict]] = None,
        temperature: float = 0.7,
        max_tokens: Optional[int] = None,
    ) -> LLMResponse:
        response = self.client.chat.completions.create(
            model=model,
            messages=messages,
            tools=tools,
            temperature=temperature,
            max_tokens=max_tokens,
        )

        return LLMResponse(
            content=response.choices[0].message.content,
            model=response.model,
            usage={
                "prompt_tokens": response.usage.prompt_tokens,
                "completion_tokens": response.usage.completion_tokens,
                "total_tokens": response.usage.total_tokens,
            },
            tool_calls=response.choices[0].message.tool_calls,
        )


class ClaudeProvider(LLMProvider):
    """Anthropic Claude API provider implementation"""

    def __init__(self, api_key: Optional[str] = None):
        self.client = anthropic.Anthropic(
            api_key=api_key or os.getenv("ANTHROPIC_API_KEY")
        )

    def chat_completion(
        self,
        messages: List[Dict[str, str]],
        model: str = "claude-3-sonnet-20240229",
        tools: Optional[List[Dict]] = None,
        temperature: float = 0.7,
        max_tokens: Optional[int] = None,
    ) -> LLMResponse:
        system_message = next(
            (m["content"] for m in messages if m["role"] == "system"), None
        )
        messages = [m for m in messages if m["role"] != "system"]

        response = self.client.messages.create(
            model=model,
            messages=[{"role": m["role"], "content": m["content"]} for m in messages],
            system=system_message,
            tools=tools,
            temperature=temperature,
            max_tokens=max_tokens,
        )

        return LLMResponse(
            content=response.content[0].text,
            model=response.model,
            usage={
                "prompt_tokens": response.usage.input_tokens,
                "completion_tokens": response.usage.output_tokens,
                "total_tokens": response.usage.input_tokens
                + response.usage.output_tokens,
            },
            tool_calls=response.tool_calls if hasattr(response, "tool_calls") else None,
        )


class GroqProvider(LLMProvider):
    """Groq API provider implementation"""

    def __init__(self, api_key: Optional[str] = None):
        self.client = groq.Groq(api_key=api_key or os.getenv("GROQ_API_KEY"))

    def chat_completion(
        self,
        messages: List[Dict[str, str]],
        model: str = "mixtral-8x7b-32768",
        tools: Optional[List[Dict]] = None,
        temperature: float = 0.7,
        max_tokens: Optional[int] = None,
    ) -> LLMResponse:
        response = self.client.chat.completions.create(
            model=model,
            messages=messages,
            temperature=temperature,
            max_tokens=max_tokens,
            tools=tools,
        )

        return LLMResponse(
            content=response.choices[0].message.content,
            model=response.model,
            usage={
                "prompt_tokens": response.usage.prompt_tokens,
                "completion_tokens": response.usage.completion_tokens,
                "total_tokens": response.usage.total_tokens,
            },
            tool_calls=response.choices[0].message.tool_calls,
        )


class OllamaProvider(LLMProvider):
    """Local Ollama API provider implementation"""

    def __init__(self, base_url: str = "http://localhost:11434"):
        import requests

        self.base_url = base_url
        self.session = requests.Session()

    def chat_completion(
        self,
        messages: List[Dict[str, str]],
        model: str = "llama3.1:8b",
        temperature: float = 0.7,
        max_tokens: Optional[int] = None,
        tools: Optional[List[Dict]] = None,
    ) -> LLMResponse:
        if tools:
            warnings.warn("Ollama does not support tools")

        response = self.session.post(
            f"{self.base_url}/api/chat",
            json={
                "model": model,
                "messages": messages,
                "temperature": temperature,
                "max_tokens": max_tokens,
            },
        )
        response.raise_for_status()
        result = response.json()

        # Ollama doesn't provide token counts, so we'll estimate
        content = result["message"]["content"]
        estimated_tokens = len(content.split()) * 1.3  # Rough estimation

        return LLMResponse(
            content=content,
            model=model,
            usage={
                "prompt_tokens": 0,  # Ollama doesn't provide these metrics
                "completion_tokens": int(estimated_tokens),
                "total_tokens": int(estimated_tokens),
            },
            tool_calls=None,
        )


ProviderType = Literal["openai", "claude", "groq", "ollama"]


def get_llm_provider(provider: ProviderType = "openai", **kwargs) -> LLMProvider:
    """
    Factory function to get an LLM provider instance

    Args:
        provider: Provider name ("openai", "claude", "groq", "ollama")
        **kwargs: Additional arguments passed to provider constructor

    Returns:
        LLMProvider instance
    """
    providers = {
        "openai": OpenAIProvider,
        "claude": ClaudeProvider,
        "groq": GroqProvider,
        "ollama": OllamaProvider,
    }

    if provider not in providers:
        raise ValueError(
            f"Unknown provider: {provider}. Available providers: {list(providers.keys())}"
        )

    return providers[provider](**kwargs)
