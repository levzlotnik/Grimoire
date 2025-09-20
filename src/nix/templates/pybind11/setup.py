#!/usr/bin/env python3

import sys
from setuptools import setup
from pybind11.setup_helpers import Pybind11Extension, build_ext
from pybind11 import get_cmake_dir
import pybind11

# Define the extension module
ext_modules = [
    Pybind11Extension(
        "pybind_demo._core",
        [
            "src/functions.cpp",
            "src/classes.cpp", 
            "src/numpy_demo.cpp",
            "src/pybind_module.cpp",
        ],
        include_dirs=[
            "include",
            # Path to pybind11 headers
            pybind11.get_include(),
        ],
        cxx_std=17,
    ),
]

# Minimal setup.py - most configuration is in pyproject.toml
setup(
    ext_modules=ext_modules,
    cmdclass={"build_ext": build_ext},
    zip_safe=False,
)