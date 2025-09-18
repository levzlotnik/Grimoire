#pragma once

#include <torch/torch.h>

namespace custom_ops {

// Parametric Swish activation: x * sigmoid(beta * x)
// Forward: f(x) = x * sigmoid(beta * x)
// Backward: f'(x) = sigmoid(beta * x) + x * sigmoid(beta * x) * (1 - sigmoid(beta * x)) * beta
torch::Tensor parametric_swish_forward(const torch::Tensor& input, double beta);
torch::Tensor parametric_swish_backward(const torch::Tensor& grad_output, const torch::Tensor& input, double beta);

// CPU implementation
torch::Tensor parametric_swish_cpu_forward(const torch::Tensor& input, double beta);
torch::Tensor parametric_swish_cpu_backward(const torch::Tensor& grad_output, const torch::Tensor& input, double beta);

} // namespace custom_ops