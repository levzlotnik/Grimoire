#include "custom_activation.h"
#include "fused_attention.h"
#include <torch/torch.h>
#include <torch/extension.h>

// Register operations with PyTorch
TORCH_LIBRARY(pytorch_custom_ops, m) {
    // Parametric Swish activation
    m.def("parametric_swish(Tensor input, float beta) -> Tensor");
    m.def("parametric_swish_backward(Tensor grad_output, Tensor input, float beta) -> Tensor");
    
    // Fused attention operation  
    m.def("fused_attention(Tensor query, Tensor key, Tensor value, float scale=1.0) -> (Tensor, Tensor)");
    m.def("fused_attention_backward(Tensor grad_output, Tensor query, Tensor key, Tensor value, Tensor attention_weights, float scale) -> (Tensor, Tensor, Tensor)");
}

TORCH_LIBRARY_IMPL(pytorch_custom_ops, CPU, m) {
    // CPU implementations
    m.impl("parametric_swish", custom_ops::parametric_swish_forward);
    m.impl("parametric_swish_backward", custom_ops::parametric_swish_backward);
    m.impl("fused_attention", custom_ops::fused_attention_forward);
    m.impl("fused_attention_backward", custom_ops::fused_attention_backward);
}

// Autograd registration happens in Python via torch.library.register_autograd

// Python module definition
PYBIND11_MODULE(_C, m) {
    m.doc() = "PyTorch Custom Operations C++ Extension";
    
    // The TORCH_LIBRARY macros above handle the actual registration
    // This module just needs to exist for Python import to work
}
