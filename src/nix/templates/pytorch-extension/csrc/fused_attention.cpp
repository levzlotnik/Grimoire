#include "fused_attention.h"
#include <torch/torch.h>
#include <omp.h>
#include <cmath>

namespace custom_ops {

std::tuple<torch::Tensor, torch::Tensor> fused_attention_cpu_forward(
    const torch::Tensor& query,
    const torch::Tensor& key,
    const torch::Tensor& value,
    double scale) {
    
    TORCH_CHECK(query.is_cpu() && key.is_cpu() && value.is_cpu(), 
                "All tensors must be on CPU");
    TORCH_CHECK(query.dim() == 3 && key.dim() == 3 && value.dim() == 3,
                "Expected 3D tensors [batch, seq_len, d_model]");
    TORCH_CHECK(query.size(0) == key.size(0) && key.size(0) == value.size(0),
                "Batch sizes must match");
    TORCH_CHECK(query.size(1) == key.size(1) && key.size(1) == value.size(1),
                "Sequence lengths must match");
    TORCH_CHECK(query.size(2) == key.size(2),
                "Query and key dimensions must match");
    
    auto batch_size = query.size(0);
    auto seq_len = query.size(1);
    auto d_model = query.size(2);
    auto d_value = value.size(2);
    
    // Step 1: Compute attention scores Q @ K^T
    auto scores = torch::matmul(query, key.transpose(-2, -1)) * scale;
    
    // Step 2: Apply softmax
    auto attention_weights = torch::softmax(scores, /*dim=*/-1);
    
    // Step 3: Apply attention to values
    auto output = torch::matmul(attention_weights, value);
    
    return std::make_tuple(output, attention_weights);
}

std::tuple<torch::Tensor, torch::Tensor, torch::Tensor> fused_attention_cpu_backward(
    const torch::Tensor& grad_output,
    const torch::Tensor& query,
    const torch::Tensor& key,
    const torch::Tensor& value,
    const torch::Tensor& attention_weights,
    double scale) {
    
    TORCH_CHECK(grad_output.is_cpu() && query.is_cpu() && key.is_cpu() && 
                value.is_cpu() && attention_weights.is_cpu(),
                "All tensors must be on CPU");
    
    auto batch_size = query.size(0);
    auto seq_len = query.size(1);
    auto d_model = query.size(2);
    auto d_value = value.size(2);
    
    // Gradient w.r.t. value: attention_weights^T @ grad_output
    auto grad_value = torch::matmul(attention_weights.transpose(-2, -1), grad_output);
    
    // Gradient w.r.t. attention_weights: grad_output @ value^T
    auto grad_attention_weights = torch::matmul(grad_output, value.transpose(-2, -1));
    
    // Gradient w.r.t. scores (before softmax)
    // For softmax backward: grad_scores = attention_weights * (grad_attention_weights - sum(attention_weights * grad_attention_weights, dim=-1, keepdim=True))
    auto sum_term = torch::sum(attention_weights * grad_attention_weights, /*dim=*/-1, /*keepdim=*/true);
    auto grad_scores = attention_weights * (grad_attention_weights - sum_term);
    
    // Scale the gradient
    grad_scores = grad_scores * scale;
    
    // Gradient w.r.t. query: grad_scores @ key
    auto grad_query = torch::matmul(grad_scores, key);
    
    // Gradient w.r.t. key: grad_scores^T @ query  
    auto grad_key = torch::matmul(grad_scores.transpose(-2, -1), query);
    
    return std::make_tuple(grad_query, grad_key, grad_value);
}

// Dispatcher functions
std::tuple<torch::Tensor, torch::Tensor> fused_attention_forward(
    const torch::Tensor& query,
    const torch::Tensor& key,
    const torch::Tensor& value,
    double scale) {
    
    if (query.is_cpu()) {
        return fused_attention_cpu_forward(query, key, value, scale);
    } else {
        TORCH_CHECK(false, "CUDA implementation not available in this template");
    }
}

std::tuple<torch::Tensor, torch::Tensor, torch::Tensor> fused_attention_backward(
    const torch::Tensor& grad_output,
    const torch::Tensor& query,
    const torch::Tensor& key,
    const torch::Tensor& value,
    const torch::Tensor& attention_weights,
    double scale) {
    
    if (query.is_cpu()) {
        return fused_attention_cpu_backward(grad_output, query, key, value, attention_weights, scale);
    } else {
        TORCH_CHECK(false, "CUDA implementation not available in this template");
    }
}

} // namespace custom_ops
