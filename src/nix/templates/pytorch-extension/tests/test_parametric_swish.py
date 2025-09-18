"""
Tests for parametric swish activation function.
Compares custom C++ implementation against PyTorch reference.
"""

import pytest
import torch
import numpy as np
from pytorch_custom_ops import parametric_swish, ParametricSwish

class TestParametricSwish:
    """Test suite for parametric swish activation function."""
    
    @pytest.fixture(params=[1.0, 0.5, 2.0])
    def beta(self, request):
        """Test different beta values."""
        return request.param
    
    @pytest.fixture(params=[
        (10,),
        (4, 8),
        (2, 3, 5),
        (1, 16, 32),
    ])
    def input_shape(self, request):
        """Test different tensor shapes."""
        return request.param
    
    def reference_parametric_swish(self, x: torch.Tensor, beta: float) -> torch.Tensor:
        """Reference implementation using pure PyTorch."""
        return x * torch.sigmoid(beta * x)
    
    def reference_parametric_swish_grad(self, x: torch.Tensor, beta: float) -> torch.Tensor:
        """Reference gradient computation."""
        sigmoid_beta_x = torch.sigmoid(beta * x)
        return sigmoid_beta_x + x * sigmoid_beta_x * (1.0 - sigmoid_beta_x) * beta
    
    def test_forward_correctness(self, input_shape, beta):
        """Test forward pass correctness against reference."""
        # Create random input
        x = torch.randn(input_shape, dtype=torch.float32)
        
        # Compute outputs
        custom_output = parametric_swish(x, beta)
        reference_output = self.reference_parametric_swish(x, beta)
        
        # Check shapes match
        assert custom_output.shape == reference_output.shape
        
        # Check values are close (allowing for small numerical differences)
        torch.testing.assert_close(custom_output, reference_output, rtol=1e-5, atol=1e-6)
    
    def test_backward_correctness(self, input_shape, beta):
        """Test backward pass correctness against reference."""
        # Create random input that requires grad
        x = torch.randn(input_shape, dtype=torch.float32, requires_grad=True)
        x_ref = x.clone().detach().requires_grad_(True)
        
        # Forward pass
        custom_output = parametric_swish(x, beta)
        reference_output = self.reference_parametric_swish(x_ref, beta)
        
        # Create random gradient for backward pass
        grad_output = torch.randn_like(custom_output)
        
        # Backward pass
        custom_output.backward(grad_output)
        reference_output.backward(grad_output)
        
        # Check gradients match
        torch.testing.assert_close(x.grad, x_ref.grad, rtol=1e-4, atol=1e-5)
    
    def test_module_interface(self, input_shape, beta):
        """Test the nn.Module interface."""
        # Create module and input
        activation = ParametricSwish(beta)
        x = torch.randn(input_shape, dtype=torch.float32)
        
        # Test forward pass
        output = activation(x)
        reference_output = self.reference_parametric_swish(x, beta)
        
        torch.testing.assert_close(output, reference_output, rtol=1e-5, atol=1e-6)
    
    def test_edge_cases(self, beta):
        """Test edge cases like zeros, very large/small values."""
        # Test zeros
        zeros = torch.zeros(5, 5)
        output = parametric_swish(zeros, beta)
        expected = torch.zeros(5, 5)
        torch.testing.assert_close(output, expected, rtol=1e-6, atol=1e-7)
        
        # Test very large positive values (should saturate to x)
        large_pos = torch.full((5, 5), 10.0)
        output = parametric_swish(large_pos, beta) 
        expected = self.reference_parametric_swish(large_pos, beta)
        torch.testing.assert_close(output, expected, rtol=1e-4, atol=1e-5)
        
        # Test very large negative values (should approach zero)
        large_neg = torch.full((5, 5), -10.0)
        output = parametric_swish(large_neg, beta)
        expected = self.reference_parametric_swish(large_neg, beta)
        torch.testing.assert_close(output, expected, rtol=1e-4, atol=1e-5)
    
    def test_gradcheck(self, beta):
        """Use torch.autograd.gradcheck for rigorous gradient testing."""
        def func(x):
            return parametric_swish(x, beta)
        
        # Small input for gradcheck
        x = torch.randn(3, 4, dtype=torch.float64, requires_grad=True)
        
        # gradcheck uses finite differences to verify gradients
        assert torch.autograd.gradcheck(func, x, eps=1e-6, atol=1e-4)
    
    def test_contiguity_requirements(self, beta):
        """Test that non-contiguous tensors are handled properly."""
        # Create non-contiguous tensor
        x = torch.randn(4, 8).t()  # Transpose makes it non-contiguous
        assert not x.is_contiguous()
        
        # Should still work (implementation should handle this)
        try:
            output = parametric_swish(x, beta)
            reference = self.reference_parametric_swish(x, beta)
            torch.testing.assert_close(output, reference, rtol=1e-5, atol=1e-6)
        except RuntimeError as e:
            if "contiguous" in str(e):
                pytest.skip("Implementation requires contiguous tensors")
            else:
                raise
    
    def test_dtype_support(self, beta):
        """Test different data types."""
        shapes = [(5, 5)]
        
        for shape in shapes:
            # Test float32 (primary support)
            x_f32 = torch.randn(shape, dtype=torch.float32)
            output_f32 = parametric_swish(x_f32, beta)
            reference_f32 = self.reference_parametric_swish(x_f32, beta)
            torch.testing.assert_close(output_f32, reference_f32, rtol=1e-5, atol=1e-6)
            
            # Test float64 if supported
            try:
                x_f64 = torch.randn(shape, dtype=torch.float64) 
                output_f64 = parametric_swish(x_f64, beta)
                reference_f64 = self.reference_parametric_swish(x_f64, beta)
                torch.testing.assert_close(output_f64, reference_f64, rtol=1e-10, atol=1e-11)
            except RuntimeError:
                pytest.skip("float64 not supported in this implementation")

if __name__ == "__main__":
    pytest.main([__file__])
