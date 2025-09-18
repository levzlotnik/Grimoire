#include "custom_activation.h"
#include <torch/torch.h>
#include <omp.h>
#include <cmath>

namespace custom_ops {

torch::Tensor parametric_swish_cpu_forward(const torch::Tensor& input, double beta) {
    TORCH_CHECK(input.is_cpu(), "Input tensor must be on CPU");
    TORCH_CHECK(input.is_contiguous(), "Input tensor must be contiguous");
    
    auto output = torch::empty_like(input);
    
    // Use OpenMP for parallelization
    auto input_data = input.data_ptr<float>();
    auto output_data = output.data_ptr<float>();
    auto numel = input.numel();
    
    #pragma omp parallel for
    for (int64_t i = 0; i < numel; ++i) {
        float x = input_data[i];
        float sigmoid_beta_x = 1.0f / (1.0f + std::exp(-beta * x));
        output_data[i] = x * sigmoid_beta_x;
    }
    
    return output;
}

torch::Tensor parametric_swish_cpu_backward(const torch::Tensor& grad_output, const torch::Tensor& input, double beta) {
    TORCH_CHECK(grad_output.is_cpu(), "Gradient tensor must be on CPU");
    TORCH_CHECK(input.is_cpu(), "Input tensor must be on CPU");
    TORCH_CHECK(grad_output.is_contiguous(), "Gradient tensor must be contiguous");
    TORCH_CHECK(input.is_contiguous(), "Input tensor must be contiguous");
    TORCH_CHECK(grad_output.sizes() == input.sizes(), "Gradient and input must have same shape");
    
    auto grad_input = torch::empty_like(input);
    
    auto grad_output_data = grad_output.data_ptr<float>();
    auto input_data = input.data_ptr<float>();
    auto grad_input_data = grad_input.data_ptr<float>();
    auto numel = input.numel();
    
    #pragma omp parallel for
    for (int64_t i = 0; i < numel; ++i) {
        float x = input_data[i];
        float sigmoid_beta_x = 1.0f / (1.0f + std::exp(-beta * x));
        
        // Derivative: sigmoid(beta*x) + x * sigmoid(beta*x) * (1 - sigmoid(beta*x)) * beta
        float derivative = sigmoid_beta_x + x * sigmoid_beta_x * (1.0f - sigmoid_beta_x) * beta;
        grad_input_data[i] = grad_output_data[i] * derivative;
    }
    
    return grad_input;
}

// Dispatcher functions
torch::Tensor parametric_swish_forward(const torch::Tensor& input, double beta) {
    if (input.is_cpu()) {
        return parametric_swish_cpu_forward(input, beta);
    } else {
        TORCH_CHECK(false, "CUDA implementation not available in this template");
    }
}

torch::Tensor parametric_swish_backward(const torch::Tensor& grad_output, const torch::Tensor& input, double beta) {
    if (input.is_cpu()) {
        return parametric_swish_cpu_backward(grad_output, input, beta);
    } else {
        TORCH_CHECK(false, "CUDA implementation not available in this template");
    }
}

} // namespace custom_ops