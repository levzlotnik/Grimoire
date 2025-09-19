#include "custom_activation.h"
#include <torch/torch.h>
#include <omp.h>
#include <cmath>

namespace custom_ops {

torch::Tensor parametric_swish_cpu_forward(const torch::Tensor& input, double beta) {
    TORCH_CHECK(input.is_cpu(), "Input tensor must be on CPU");
    TORCH_CHECK(input.dtype() == torch::kFloat32, "Input tensor must be float32, got ", input.dtype());
    
    // Make input contiguous if it isn't already
    auto contiguous_input = input.contiguous();
    auto output = torch::empty_like(contiguous_input);
    
    // Use OpenMP for parallelization
    auto input_data = contiguous_input.data_ptr<float>();
    auto output_data = output.data_ptr<float>();
    auto numel = contiguous_input.numel();
    
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
    TORCH_CHECK(grad_output.sizes() == input.sizes(), "Gradient and input must have same shape");
    TORCH_CHECK(input.dtype() == torch::kFloat32, "Input tensor must be float32, got ", input.dtype());
    TORCH_CHECK(grad_output.dtype() == torch::kFloat32, "Gradient tensor must be float32, got ", grad_output.dtype());
    
    // Make tensors contiguous if they aren't already
    auto contiguous_grad_output = grad_output.contiguous();
    auto contiguous_input = input.contiguous();
    auto grad_input = torch::empty_like(contiguous_input);
    
    auto grad_output_data = contiguous_grad_output.data_ptr<float>();
    auto input_data = contiguous_input.data_ptr<float>();
    auto grad_input_data = grad_input.data_ptr<float>();
    auto numel = contiguous_input.numel();
    
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
