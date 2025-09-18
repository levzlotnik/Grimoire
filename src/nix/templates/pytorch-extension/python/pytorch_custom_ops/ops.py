"""
Low-level operations module providing direct access to backward passes.
This module is used internally by the autograd registration.
"""

import torch
from typing import Tuple

def parametric_swish_backward(grad_output: torch.Tensor, 
                            input: torch.Tensor, 
                            beta: float) -> torch.Tensor:
    """
    Backward pass for parametric swish activation.
    
    Computes: grad_input = grad_output * d/dx[x * sigmoid(beta * x)]
    Where d/dx[x * sigmoid(beta * x)] = sigmoid(beta * x) + x * sigmoid(beta * x) * (1 - sigmoid(beta * x)) * beta
    """
    sigmoid_beta_x = torch.sigmoid(beta * input)
    derivative = sigmoid_beta_x + input * sigmoid_beta_x * (1.0 - sigmoid_beta_x) * beta
    return grad_output * derivative

def fused_attention_backward(grad_output: torch.Tensor,
                           query: torch.Tensor,
                           key: torch.Tensor,
                           value: torch.Tensor, 
                           attention_weights: torch.Tensor,
                           scale: float) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
    """
    Backward pass for fused attention operation.
    
    Args:
        grad_output: Gradient w.r.t. output [batch, seq_len, d_value]
        query: Query tensor [batch, seq_len, d_model]
        key: Key tensor [batch, seq_len, d_model] 
        value: Value tensor [batch, seq_len, d_value]
        attention_weights: Attention weights [batch, seq_len, seq_len]
        scale: Scaling factor
        
    Returns:
        Tuple of (grad_query, grad_key, grad_value)
    """
    batch_size, seq_len, d_model = query.shape
    d_value = value.size(-1)
    
    # Gradient w.r.t. value: attention_weights^T @ grad_output
    grad_value = torch.matmul(attention_weights.transpose(-2, -1), grad_output)
    
    # Gradient w.r.t. attention_weights: grad_output @ value^T  
    grad_attention_weights = torch.matmul(grad_output, value.transpose(-2, -1))
    
    # Gradient w.r.t. scores (before softmax)
    # Softmax backward: grad_scores = attention_weights * (grad_attention_weights - sum_term)
    sum_term = torch.sum(attention_weights * grad_attention_weights, dim=-1, keepdim=True)
    grad_scores = attention_weights * (grad_attention_weights - sum_term)
    
    # Scale the gradient
    grad_scores = grad_scores * scale
    
    # Gradient w.r.t. query: grad_scores @ key
    grad_query = torch.matmul(grad_scores, key)
    
    # Gradient w.r.t. key: grad_scores^T @ query
    grad_key = torch.matmul(grad_scores.transpose(-2, -1), query)
    
    return grad_query, grad_key, grad_value