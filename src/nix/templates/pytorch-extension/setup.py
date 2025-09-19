#!/usr/bin/env python3

import sys
from setuptools import setup
from torch.utils.cpp_extension import CppExtension, BuildExtension

# Define compilation arguments
extra_compile_args = ['-O3', '-fopenmp']
extra_link_args = ['-fopenmp']

# Add platform-specific flags
if sys.platform.startswith('darwin'):
    # macOS specific flags
    extra_compile_args.extend(['-Xpreprocessor', '-fopenmp'])
    extra_link_args.extend(['-lomp'])

# Minimal setup.py - most configuration is in pyproject.toml
setup(
    ext_modules=[
        CppExtension(
            name='pytorch_custom_ops._C',
            sources=[
                'csrc/ops.cpp',
                'csrc/custom_activation.cpp', 
                'csrc/fused_attention.cpp',
            ],
            extra_compile_args=extra_compile_args,
            extra_link_args=extra_link_args,
        )
    ],
    cmdclass={'build_ext': BuildExtension},
    # Ensure Python files are included
    package_dir={'pytorch_custom_ops': 'python/pytorch_custom_ops'},
    packages=['pytorch_custom_ops'],
    package_data={'pytorch_custom_ops': ['*.py']},
    include_package_data=True,
)
