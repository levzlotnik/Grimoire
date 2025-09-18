"""
Tests for fused attention operation.
Compares custom C++ implementation against PyTorch reference.
"""

import pytest
import torch
import numpy as np
from pytorch_custom_ops import fused_attention, FusedAttention

class TestFusedAttention:
    """Test suite for fused attention operation."""
    
    @pytest.fixture(params=[1.0, 0.1, 2.0])
    def scale(self, request):
        """Test different scaling factors."""
        return request.param
    
    @pytest.fixture(params=[
        (1, 4, 8),   # batch=1, seq_len=4, d_model=8
        (2, 8, 16),  # batch=2, seq_len=8, d_model=16  
        (1, 16, 32), # batch=1, seq_len=16, d_model=32
        (3, 6, 12),  # batch=3, seq_len=6, d_model=12
    ])
    def attention_shape(self, request):
        """Test different attention tensor shapes."""
        return request.param
    
    def reference_fused_attention(self, query, key, value, scale):
        """Reference implementation using pure PyTorch."""
        # Compute attention scores
        scores = torch.matmul(query, key.transpose(-2, -1)) * scale
        
        # Apply softmax
        attention_weights = torch.softmax(scores, dim=-1)
        
        # Apply attention to values
        output = torch.matmul(attention_weights, value)
        
        return output, attention_weights
    
    def test_forward_correctness(self, attention_shape, scale):
        """Test forward pass correctness against reference."""
        batch_size, seq_len, d_model = attention_shape
        
        # Create random inputs
        query = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32)
        key = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32)
        value = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32)
        
        # Compute outputs
        custom_output, custom_weights = fused_attention(query, key, value, scale)
        reference_output, reference_weights = self.reference_fused_attention(query, key, value, scale)
        
        # Check shapes match
        assert custom_output.shape == reference_output.shape
        assert custom_weights.shape == reference_weights.shape
        
        # Check values are close
        torch.testing.assert_close(custom_output, reference_output, rtol=1e-5, atol=1e-6)
        torch.testing.assert_close(custom_weights, reference_weights, rtol=1e-5, atol=1e-6)
    
    def test_backward_correctness(self, attention_shape, scale):
        """Test backward pass correctness against reference."""
        batch_size, seq_len, d_model = attention_shape
        
        # Create random inputs that require grad
        query = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32, requires_grad=True)
        key = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32, requires_grad=True) 
        value = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32, requires_grad=True)
        
        # Reference inputs
        query_ref = query.clone().detach().requires_grad_(True)
        key_ref = key.clone().detach().requires_grad_(True)
        value_ref = value.clone().detach().requires_grad_(True)
        
        # Forward pass
        custom_output, _ = fused_attention(query, key, value, scale)
        reference_output, _ = self.reference_fused_attention(query_ref, key_ref, value_ref, scale)
        
        # Create random gradient for backward pass
        grad_output = torch.randn_like(custom_output)
        
        # Backward pass
        custom_output.backward(grad_output)
        reference_output.backward(grad_output)
        
        # Check gradients match
        torch.testing.assert_close(query.grad, query_ref.grad, rtol=1e-4, atol=1e-5)
        torch.testing.assert_close(key.grad, key_ref.grad, rtol=1e-4, atol=1e-5)
        torch.testing.assert_close(value.grad, value_ref.grad, rtol=1e-4, atol=1e-5)
    
    def test_module_interface(self, attention_shape, scale):
        """Test the nn.Module interface."""
        batch_size, seq_len, d_model = attention_shape
        
        # Create module and inputs
        attention_module = FusedAttention(scale)
        query = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32)
        key = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32)
        value = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32)
        
        # Test forward pass
        output, weights = attention_module(query, key, value)
        reference_output, reference_weights = self.reference_fused_attention(query, key, value, scale)
        
        torch.testing.assert_close(output, reference_output, rtol=1e-5, atol=1e-6)
        torch.testing.assert_close(weights, reference_weights, rtol=1e-5, atol=1e-6)
    
    def test_default_scaling(self):
        """Test default scaling (1/sqrt(d_model))."""
        batch_size, seq_len, d_model = 2, 4, 8
        
        query = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32)
        key = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32)
        value = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32)
        
        # Test default scaling
        output_default, _ = fused_attention(query, key, value)
        
        # Test explicit scaling
        expected_scale = 1.0 / (d_model ** 0.5)
        output_explicit, _ = fused_attention(query, key, value, expected_scale)
        
        torch.testing.assert_close(output_default, output_explicit, rtol=1e-6, atol=1e-7)
    
    def test_attention_weights_properties(self, attention_shape):
        """Test that attention weights have correct properties."""
        batch_size, seq_len, d_model = attention_shape
        scale = 1.0
        
        query = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32)
        key = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32)
        value = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32)
        
        output, attention_weights = fused_attention(query, key, value, scale)
        
        # Check attention weights shape
        expected_weights_shape = (batch_size, seq_len, seq_len)
        assert attention_weights.shape == expected_weights_shape
        
        # Check that attention weights sum to 1 along the last dimension
        weights_sum = torch.sum(attention_weights, dim=-1)
        expected_sum = torch.ones(batch_size, seq_len)
        torch.testing.assert_close(weights_sum, expected_sum, rtol=1e-5, atol=1e-6)
        
        # Check that attention weights are non-negative
        assert torch.all(attention_weights >= 0)
    
    def test_mismatched_dimensions(self):
        """Test error handling for mismatched tensor dimensions."""
        # Mismatched batch sizes
        query = torch.randn(2, 4, 8)
        key = torch.randn(3, 4, 8)  # Different batch size
        value = torch.randn(2, 4, 8)
        
        with pytest.raises(RuntimeError):
            fused_attention(query, key, value)
        
        # Mismatched sequence lengths
        query = torch.randn(2, 4, 8)
        key = torch.randn(2, 6, 8)  # Different sequence length
        value = torch.randn(2, 4, 8)
        
        with pytest.raises(RuntimeError):
            fused_attention(query, key, value)
        
        # Mismatched model dimensions for query/key
        query = torch.randn(2, 4, 8)
        key = torch.randn(2, 4, 12)  # Different model dimension
        value = torch.randn(2, 4, 8)
        
        with pytest.raises(RuntimeError):
            fused_attention(query, key, value)
    
    def test_different_value_dimension(self):
        """Test that value can have different dimension than query/key."""
        batch_size, seq_len = 2, 4
        d_model, d_value = 8, 12
        
        query = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32)
        key = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32)
        value = torch.randn(batch_size, seq_len, d_value, dtype=torch.float32)  # Different dimension
        
        output, attention_weights = fused_attention(query, key, value, 1.0)
        
        # Output should have value's dimension
        expected_output_shape = (batch_size, seq_len, d_value)
        assert output.shape == expected_output_shape
        
        # Attention weights should still be seq_len x seq_len
        expected_weights_shape = (batch_size, seq_len, seq_len)
        assert attention_weights.shape == expected_weights_shape
    
    def test_gradcheck(self):
        """Use torch.autograd.gradcheck for rigorous gradient testing."""
        def func(query, key, value):
            output, _ = fused_attention(query, key, value, 1.0)
            return output
        
        # Small inputs for gradcheck
        batch_size, seq_len, d_model = 1, 3, 4
        query = torch.randn(batch_size, seq_len, d_model, dtype=torch.float64, requires_grad=True)
        key = torch.randn(batch_size, seq_len, d_model, dtype=torch.float64, requires_grad=True)
        value = torch.randn(batch_size, seq_len, d_model, dtype=torch.float64, requires_grad=True)
        
        # gradcheck uses finite differences to verify gradients
        assert torch.autograd.gradcheck(func, (query, key, value), eps=1e-6, atol=1e-4)
    
    def test_self_attention(self):
        """Test self-attention case where query=key=value."""
        batch_size, seq_len, d_model = 2, 6, 8
        
        # Self-attention: Q = K = V
        qkv = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32)
        
        output, attention_weights = fused_attention(qkv, qkv, qkv, 1.0)
        reference_output, reference_weights = self.reference_fused_attention(qkv, qkv, qkv, 1.0)
        
        torch.testing.assert_close(output, reference_output, rtol=1e-5, atol=1e-6)
        torch.testing.assert_close(attention_weights, reference_weights, rtol=1e-5, atol=1e-6)

if __name__ == "__main__":
    pytest.main([__file__])