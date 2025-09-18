#pragma once

#include <torch/torch.h>
#include <tuple>

namespace custom_ops {

// Fused attention-like operation: matmul -> softmax -> weighted sum
// Forward: Y = softmax(Q @ K^T) @ V
// Returns both output and intermediate attention weights for backward pass
std::tuple<torch::Tensor, torch::Tensor> fused_attention_forward(
    const torch::Tensor& query,     // [batch, seq_len, d_model] 
    const torch::Tensor& key,       // [batch, seq_len, d_model]
    const torch::Tensor& value,     // [batch, seq_len, d_model]
    double scale = 1.0
);

// Backward pass for fused attention
std::tuple<torch::Tensor, torch::Tensor, torch::Tensor> fused_attention_backward(
    const torch::Tensor& grad_output,     // [batch, seq_len, d_model]
    const torch::Tensor& query,           // [batch, seq_len, d_model]
    const torch::Tensor& key,             // [batch, seq_len, d_model] 
    const torch::Tensor& value,           // [batch, seq_len, d_model]
    const torch::Tensor& attention_weights, // [batch, seq_len, seq_len]
    double scale = 1.0
);

// CPU implementations
std::tuple<torch::Tensor, torch::Tensor> fused_attention_cpu_forward(
    const torch::Tensor& query,
    const torch::Tensor& key,
    const torch::Tensor& value,
    double scale
);

std::tuple<torch::Tensor, torch::Tensor, torch::Tensor> fused_attention_cpu_backward(
    const torch::Tensor& grad_output,
    const torch::Tensor& query,
    const torch::Tensor& key,
    const torch::Tensor& value,
    const torch::Tensor& attention_weights,
    double scale
);

} // namespace custom_ops
