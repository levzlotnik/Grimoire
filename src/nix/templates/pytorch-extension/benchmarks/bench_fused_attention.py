"""
Benchmark fused attention operation.
Compares performance of custom C++ implementation vs PyTorch reference.
"""

import pytest
import torch
import time
import numpy as np
from pytorch_custom_ops import fused_attention

class TestFusedAttentionBenchmarks:
    """Benchmark suite for fused attention operation."""
    
    def reference_fused_attention(self, query, key, value, scale):
        """Reference implementation using pure PyTorch."""
        scores = torch.matmul(query, key.transpose(-2, -1)) * scale
        attention_weights = torch.softmax(scores, dim=-1)
        output = torch.matmul(attention_weights, value)
        return output, attention_weights
    
    @pytest.mark.benchmark(group="fused_attention_forward")
    @pytest.mark.parametrize("batch_size,seq_len,d_model", [
        (1, 32, 64),
        (2, 64, 128),
        (4, 128, 256),
        (1, 256, 512),
        (8, 32, 128),
    ])
    def test_forward_custom(self, benchmark, batch_size, seq_len, d_model):
        """Benchmark custom implementation forward pass."""
        query = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32)
        key = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32)
        value = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32)
        scale = 1.0 / (d_model ** 0.5)
        
        output, weights = benchmark(fused_attention, query, key, value, scale)
        assert output.shape == (batch_size, seq_len, d_model)
        assert weights.shape == (batch_size, seq_len, seq_len)
    
    @pytest.mark.benchmark(group="fused_attention_forward")
    @pytest.mark.parametrize("batch_size,seq_len,d_model", [
        (1, 32, 64),
        (2, 64, 128),
        (4, 128, 256),
        (1, 256, 512),
        (8, 32, 128),
    ])
    def test_forward_reference(self, benchmark, batch_size, seq_len, d_model):
        """Benchmark reference implementation forward pass."""
        query = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32)
        key = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32)
        value = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32)
        scale = 1.0 / (d_model ** 0.5)
        
        output, weights = benchmark(self.reference_fused_attention, query, key, value, scale)
        assert output.shape == (batch_size, seq_len, d_model)
        assert weights.shape == (batch_size, seq_len, seq_len)
    
    @pytest.mark.benchmark(group="fused_attention_backward")
    @pytest.mark.parametrize("batch_size,seq_len,d_model", [
        (1, 32, 64),
        (2, 64, 128),
        (4, 128, 256),
    ])
    def test_backward_custom(self, benchmark, batch_size, seq_len, d_model):
        """Benchmark custom implementation backward pass."""
        query = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32, requires_grad=True)
        key = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32, requires_grad=True)
        value = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32, requires_grad=True)
        scale = 1.0 / (d_model ** 0.5)
        
        def run_backward():
            output, _ = fused_attention(query, key, value, scale)
            grad_output = torch.randn_like(output)
            output.backward(grad_output)
            return query.grad, key.grad, value.grad
        
        # Clear gradients before benchmark
        if query.grad is not None:
            query.grad.zero_()
        if key.grad is not None:
            key.grad.zero_()
        if value.grad is not None:
            value.grad.zero_()
        
        grad_q, grad_k, grad_v = benchmark(run_backward)
        assert grad_q.shape == query.shape
        assert grad_k.shape == key.shape
        assert grad_v.shape == value.shape
    
    @pytest.mark.benchmark(group="fused_attention_backward")
    @pytest.mark.parametrize("batch_size,seq_len,d_model", [
        (1, 32, 64),
        (2, 64, 128),
        (4, 128, 256),
    ])
    def test_backward_reference(self, benchmark, batch_size, seq_len, d_model):
        """Benchmark reference implementation backward pass."""
        query = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32, requires_grad=True)
        key = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32, requires_grad=True)
        value = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32, requires_grad=True)
        scale = 1.0 / (d_model ** 0.5)
        
        def run_backward():
            output, _ = self.reference_fused_attention(query, key, value, scale)
            grad_output = torch.randn_like(output)
            output.backward(grad_output)
            return query.grad, key.grad, value.grad
        
        # Clear gradients before benchmark
        if query.grad is not None:
            query.grad.zero_()
        if key.grad is not None:
            key.grad.zero_()
        if value.grad is not None:
            value.grad.zero_()
        
        grad_q, grad_k, grad_v = benchmark(run_backward)
        assert grad_q.shape == query.shape
        assert grad_k.shape == key.shape
        assert grad_v.shape == value.shape
    
    @pytest.mark.slow
    def test_memory_efficiency(self):
        """Test memory efficiency of fused vs non-fused operations."""
        batch_size, seq_len, d_model = 4, 128, 256
        
        query = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32)
        key = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32)
        value = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32)
        scale = 1.0 / (d_model ** 0.5)
        
        # Measure memory for fused operation
        torch.cuda.empty_cache() if torch.cuda.is_available() else None
        initial_memory = torch.cuda.memory_allocated() if torch.cuda.is_available() else 0
        
        output_fused, _ = fused_attention(query, key, value, scale)
        
        fused_memory = torch.cuda.memory_allocated() if torch.cuda.is_available() else 0
        fused_peak = torch.cuda.max_memory_allocated() if torch.cuda.is_available() else 0
        
        # Measure memory for reference operation
        torch.cuda.empty_cache() if torch.cuda.is_available() else None
        torch.cuda.reset_peak_memory_stats() if torch.cuda.is_available() else None
        
        output_ref, _ = self.reference_fused_attention(query, key, value, scale)
        
        ref_memory = torch.cuda.memory_allocated() if torch.cuda.is_available() else 0
        ref_peak = torch.cuda.max_memory_allocated() if torch.cuda.is_available() else 0
        
        print(f"\nMemory usage comparison:")
        print(f"Fused - Current: {fused_memory / 1024**2:.2f} MB, Peak: {fused_peak / 1024**2:.2f} MB")
        print(f"Reference - Current: {ref_memory / 1024**2:.2f} MB, Peak: {ref_peak / 1024**2:.2f} MB")
        
        # Verify correctness
        torch.testing.assert_close(output_fused, output_ref, rtol=1e-5, atol=1e-6)
    
    @pytest.mark.slow
    def test_performance_comparison(self):
        """Compare performance between custom and reference implementations."""
        test_configs = [
            (1, 32, 64),
            (2, 64, 128),
            (4, 128, 256),
            (1, 256, 512),
        ]
        num_trials = 50
        
        results = {}
        
        for batch_size, seq_len, d_model in test_configs:
            print(f"\nBenchmarking shape ({batch_size}, {seq_len}, {d_model}):")
            
            query = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32)
            key = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32)
            value = torch.randn(batch_size, seq_len, d_model, dtype=torch.float32)
            scale = 1.0 / (d_model ** 0.5)
            
            # Warm up
            for _ in range(10):
                _ = fused_attention(query, key, value, scale)
                _ = self.reference_fused_attention(query, key, value, scale)
            
            # Benchmark custom implementation
            start_time = time.perf_counter()
            for _ in range(num_trials):
                output_custom, weights_custom = fused_attention(query, key, value, scale)
            custom_time = time.perf_counter() - start_time
            
            # Benchmark reference implementation
            start_time = time.perf_counter()
            for _ in range(num_trials):
                output_reference, weights_reference = self.reference_fused_attention(query, key, value, scale)
            reference_time = time.perf_counter() - start_time
            
            # Calculate speedup
            speedup = reference_time / custom_time
            
            config_name = f"{batch_size}x{seq_len}x{d_model}"
            results[config_name] = {
                'custom_time': custom_time,
                'reference_time': reference_time,
                'speedup': speedup,
                'ops_per_second_custom': num_trials / custom_time,
                'ops_per_second_reference': num_trials / reference_time,
            }
            
            print(f"  Custom: {custom_time:.4f}s ({num_trials / custom_time:.1f} ops/s)")
            print(f"  Reference: {reference_time:.4f}s ({num_trials / reference_time:.1f} ops/s)")
            print(f"  Speedup: {speedup:.2f}x")
            
            # Verify correctness
            torch.testing.assert_close(output_custom, output_reference, rtol=1e-5, atol=1e-6)
            torch.testing.assert_close(weights_custom, weights_reference, rtol=1e-5, atol=1e-6)
        
        # Print summary
        print("\n" + "="*60)
        print("PERFORMANCE SUMMARY")
        print("="*60)
        for config, result in results.items():
            print(f"Config {config}: {result['speedup']:.2f}x speedup")

if __name__ == "__main__":
    pytest.main([__file__, "--benchmark-only", "-v"])
