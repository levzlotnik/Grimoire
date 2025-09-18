"""
PyBind11 Comprehensive Demo Package

A complete demonstration of PyBind11 features including:
- Function binding with various argument types
- Class binding with inheritance and polymorphism  
- Type conversions between C++ STL and Python
- Exception handling
- NumPy integration
- Smart pointer support
- Operator overloading

This package showcases best practices for creating high-performance
Python extensions using PyBind11 and C++.
"""

# Import the compiled C++ extension
try:
    from pybind_demo import *
except ImportError:
    # Fall back for development when module isn't built yet
    pass

__version__ = "1.0.0"
__author__ = "Grimoire Template"
__email__ = "template@grimoire.dev"

__all__ = [
    # Core classes
    "Calculator", 
    "Rectangle", 
    "Circle", 
    "Shape", 
    "DataContainer", 
    "ResourceManager",
    
    # Submodules
    "functions",
    "numpy_demo", 
    
    # Exceptions
    "PyBindDemoException",
    
]
