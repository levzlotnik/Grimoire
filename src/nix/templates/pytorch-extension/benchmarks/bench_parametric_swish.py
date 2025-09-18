"""
Benchmark parametric swish activation function.
Compares performance of custom C++ implementation vs PyTorch reference.
"""

import pytest
import torch
import time
import numpy as np
from pytorch_custom_ops import parametric_swish

class TestParametricSwishBenchmarks:
    """Benchmark suite for parametric swish activation function."""
    
    def reference_parametric_swish(self, x: torch.Tensor, beta: float) -> torch.Tensor:
        """Reference implementation using pure PyTorch."""
        return x * torch.sigmoid(beta * x)
    
    @pytest.mark.benchmark(group="parametric_swish_forward")
    @pytest.mark.parametrize("shape,beta", [
        ((1000,), 1.0),
        ((1000, 1000), 1.0),
        ((100, 100, 100), 1.0),
        ((10, 1000, 100), 1.0),
    ])
    def test_forward_custom(self, benchmark, shape, beta):
        """Benchmark custom implementation forward pass."""
        x = torch.randn(shape, dtype=torch.float32)
        result = benchmark(parametric_swish, x, beta)
        assert result.shape == shape
    
    @pytest.mark.benchmark(group="parametric_swish_forward")
    @pytest.mark.parametrize("shape,beta", [
        ((1000,), 1.0),
        ((1000, 1000), 1.0),
        ((100, 100, 100), 1.0),
        ((10, 1000, 100), 1.0),
    ])
    def test_forward_reference(self, benchmark, shape, beta):
        """Benchmark reference implementation forward pass."""
        x = torch.randn(shape, dtype=torch.float32)
        result = benchmark(self.reference_parametric_swish, x, beta)
        assert result.shape == shape
    
    @pytest.mark.benchmark(group="parametric_swish_backward")
    @pytest.mark.parametrize("shape,beta", [
        ((1000,), 1.0),
        ((1000, 1000), 1.0),
        ((100, 100, 100), 1.0),
    ])
    def test_backward_custom(self, benchmark, shape, beta):
        """Benchmark custom implementation backward pass."""
        x = torch.randn(shape, dtype=torch.float32, requires_grad=True)
        
        def run_backward():
            y = parametric_swish(x, beta)
            grad_output = torch.randn_like(y)
            y.backward(grad_output)
            return x.grad
        
        # Clear gradients before benchmark
        if x.grad is not None:
            x.grad.zero_()
        
        result = benchmark(run_backward)
        assert result.shape == shape
    
    @pytest.mark.benchmark(group="parametric_swish_backward")
    @pytest.mark.parametrize("shape,beta", [
        ((1000,), 1.0),
        ((1000, 1000), 1.0),
        ((100, 100, 100), 1.0),
    ])
    def test_backward_reference(self, benchmark, shape, beta):
        """Benchmark reference implementation backward pass."""
        x = torch.randn(shape, dtype=torch.float32, requires_grad=True)
        
        def run_backward():
            y = self.reference_parametric_swish(x, beta)
            grad_output = torch.randn_like(y)
            y.backward(grad_output)
            return x.grad
        
        # Clear gradients before benchmark
        if x.grad is not None:
            x.grad.zero_()
        
        result = benchmark(run_backward)
        assert result.shape == shape
    
    @pytest.mark.slow
    def test_performance_comparison(self):
        """Compare performance between custom and reference implementations."""
        shapes = [
            (10000,),
            (1000, 1000),
            (100, 100, 100),
        ]
        beta = 1.0
        num_trials = 100
        
        results = {}
        
        for shape in shapes:
            print(f"\nBenchmarking shape {shape}:")
            
            # Warm up
            x = torch.randn(shape, dtype=torch.float32)
            for _ in range(10):
                _ = parametric_swish(x, beta)
                _ = self.reference_parametric_swish(x, beta)
            
            # Benchmark custom implementation
            x = torch.randn(shape, dtype=torch.float32)
            start_time = time.perf_counter()
            for _ in range(num_trials):
                result_custom = parametric_swish(x, beta)
            custom_time = time.perf_counter() - start_time
            
            # Benchmark reference implementation
            x = torch.randn(shape, dtype=torch.float32)
            start_time = time.perf_counter()
            for _ in range(num_trials):
                result_reference = self.reference_parametric_swish(x, beta)
            reference_time = time.perf_counter() - start_time
            
            # Calculate speedup
            speedup = reference_time / custom_time
            
            results[shape] = {
                'custom_time': custom_time,
                'reference_time': reference_time,
                'speedup': speedup
            }
            
            print(f"  Custom: {custom_time:.4f}s")
            print(f"  Reference: {reference_time:.4f}s") 
            print(f"  Speedup: {speedup:.2f}x")
            
            # Verify correctness
            torch.testing.assert_close(result_custom, result_reference, rtol=1e-5, atol=1e-6)
        
        # Print summary
        print("\n" + "="*50)
        print("PERFORMANCE SUMMARY")
        print("="*50)
        for shape, result in results.items():
            print(f"Shape {shape}: {result['speedup']:.2f}x speedup")

if __name__ == "__main__":
    pytest.main([__file__, "--benchmark-only", "-v"])
