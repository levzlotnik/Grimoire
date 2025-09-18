#!/usr/bin/env python3

import os
import sys
import torch
from torch.utils.cpp_extension import CppExtension, setup

# Define compilation arguments
extra_compile_args = ['-O3', '-fopenmp']
extra_link_args = ['-fopenmp']

# Add platform-specific flags
if sys.platform.startswith('darwin'):
    # macOS specific flags
    extra_compile_args.extend(['-Xpreprocessor', '-fopenmp'])
    extra_link_args.extend(['-lomp'])

setup(
    name='pytorch_custom_ops',
    version='0.1.0',
    author='Grimoire Template',
    author_email='template@grimoire.dev',
    description='PyTorch C++ Extension Template with Custom Operations',
    long_description=open('README.md').read() if os.path.exists('README.md') else '',
    long_description_content_type='text/markdown',
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
    packages=['pytorch_custom_ops'],
    package_dir={'pytorch_custom_ops': 'python/pytorch_custom_ops'},
    cmdclass={'build_ext': torch.utils.cpp_extension.BuildExtension},
    python_requires='>=3.8',
    install_requires=[
        'torch>=1.12.0',
        'numpy',
    ],
    classifiers=[
        'Development Status :: 3 - Alpha',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: MIT License',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.8',
        'Programming Language :: Python :: 3.9',
        'Programming Language :: Python :: 3.10',
        'Programming Language :: Python :: 3.11',
        'Topic :: Scientific/Engineering :: Artificial Intelligence',
    ],
)