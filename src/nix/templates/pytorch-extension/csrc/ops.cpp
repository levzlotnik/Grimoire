#include "custom_activation.h"
#include "fused_attention.h"
#include <torch/torch.h>

// Register operations with PyTorch
TORCH_LIBRARY(pytorch_custom_ops, m) {
    // Parametric Swish activation
    m.def("parametric_swish(Tensor input, float beta) -> Tensor");
    
    // Fused attention operation  
    m.def("fused_attention(Tensor query, Tensor key, Tensor value, float scale=1.0) -> (Tensor, Tensor)");
}

TORCH_LIBRARY_IMPL(pytorch_custom_ops, CPU, m) {
    // CPU implementations
    m.impl("parametric_swish", custom_ops::parametric_swish_forward);
    m.impl("fused_attention", custom_ops::fused_attention_forward);
}

// Autograd registration happens in Python via torch.library.register_autograd