"""
PyTorch Custom Operations Extension

This module provides custom C++ operations for PyTorch, demonstrating:
1. Custom activation functions with autograd support
2. Fused attention-like operations 
3. CPU optimization with OpenMP
4. Proper gradient computation
"""

import torch
import torch.library
from typing import Tuple, Optional

# Load the C++ extension - fail loudly if not available
try:
    # Import the compiled extension module
    # This will be created by setuptools when building the package
    import pytorch_custom_ops._C
except ImportError as e:
    raise ImportError(
        f"Failed to load pytorch_custom_ops C++ extension: {e}\n"
        f"Make sure the extension is built. Try:\n"
        f"  pip install -e .\n"
        f"  or\n"
        f"  python setup.py build_ext --inplace"
    ) from e

def _parametric_swish_backward(ctx, grad_output: torch.Tensor) -> Tuple[torch.Tensor, None]:
    """Backward pass for parametric swish activation using C++ implementation"""
    saved_input, = ctx.saved_tensors
    beta = ctx.beta
    # Use C++ backward implementation directly
    grad_input = torch.ops.pytorch_custom_ops.parametric_swish_backward(grad_output, saved_input, beta)
    return grad_input, None

def _parametric_swish_setup_context(ctx, inputs, output):
    """Setup context for parametric swish autograd"""
    input_tensor, beta = inputs
    ctx.save_for_backward(input_tensor)
    ctx.beta = beta

# Register autograd for parametric_swish
torch.library.register_autograd(
    "pytorch_custom_ops::parametric_swish",
    _parametric_swish_backward,
    setup_context=_parametric_swish_setup_context
)

def _fused_attention_backward(ctx, grad_output: torch.Tensor, grad_weights: torch.Tensor) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor, None]:
    """Backward pass for fused attention using C++ implementation"""
    saved_query, saved_key, saved_value, saved_attention_weights = ctx.saved_tensors
    scale = ctx.scale
    # Use C++ backward implementation directly
    grad_q, grad_k, grad_v = torch.ops.pytorch_custom_ops.fused_attention_backward(
        grad_output, saved_query, saved_key, saved_value, saved_attention_weights, scale
    )
    return grad_q, grad_k, grad_v, None

def _fused_attention_setup_context(ctx, inputs, output):
    """Setup context for fused attention autograd"""
    query, key, value, scale = inputs
    output_tensor, attention_weights = output
    ctx.save_for_backward(query, key, value, attention_weights)
    ctx.scale = scale

# Register autograd for fused_attention
torch.library.register_autograd(
    "pytorch_custom_ops::fused_attention",
    _fused_attention_backward,
    setup_context=_fused_attention_setup_context
)

# Public API
class ParametricSwish(torch.nn.Module):
    """
    Parametric Swish activation function: f(x) = x * sigmoid(beta * x)
    
    Args:
        beta (float): Parameter controlling the steepness of the activation
    """
    
    def __init__(self, beta: float = 1.0):
        super().__init__()
        self.beta = beta
    
    def forward(self, x: torch.Tensor) -> torch.Tensor:
        return torch.ops.pytorch_custom_ops.parametric_swish(x, self.beta)

def parametric_swish(input: torch.Tensor, beta: float = 1.0) -> torch.Tensor:
    """
    Parametric Swish activation function.
    
    Args:
        input: Input tensor
        beta: Parameter controlling the steepness
        
    Returns:
        Output tensor with same shape as input
    """
    return torch.ops.pytorch_custom_ops.parametric_swish(input, beta)

def fused_attention(query: torch.Tensor, 
                   key: torch.Tensor, 
                   value: torch.Tensor,
                   scale: Optional[float] = None) -> Tuple[torch.Tensor, torch.Tensor]:
    """
    Fused attention operation: softmax(Q @ K^T / scale) @ V
    
    Args:
        query: Query tensor [batch, seq_len, d_model]
        key: Key tensor [batch, seq_len, d_model] 
        value: Value tensor [batch, seq_len, d_model]
        scale: Scaling factor (default: 1/sqrt(d_model))
        
    Returns:
        Tuple of (output, attention_weights)
    """
    if scale is None:
        scale = 1.0 / (query.size(-1) ** 0.5)
    
    return torch.ops.pytorch_custom_ops.fused_attention(query, key, value, scale)

class FusedAttention(torch.nn.Module):
    """
    Fused attention module with optional scaling.
    """
    
    def __init__(self, scale: Optional[float] = None):
        super().__init__()
        self.scale = scale
    
    def forward(self, query: torch.Tensor, key: torch.Tensor, value: torch.Tensor) -> Tuple[torch.Tensor, torch.Tensor]:
        scale = self.scale
        if scale is None:
            scale = 1.0 / (query.size(-1) ** 0.5)
        return fused_attention(query, key, value, scale)

__all__ = [
    'ParametricSwish',
    'FusedAttention', 
    'parametric_swish',
    'fused_attention',
]
