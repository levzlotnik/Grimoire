#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include <pybind11/functional.h>
#include <pybind11/numpy.h>
#include <pybind11/operators.h>

#include "../include/functions.h"
#include "../include/classes.h"
#include "../include/numpy_demo.h"

namespace py = pybind11;
using namespace pybind_demo;

PYBIND11_MODULE(_core, m) {
    m.doc() = "PyBind11 comprehensive demo module showcasing all major features";
    
    // Custom exception
    py::register_exception<PyBindDemoException>(m, "PyBindDemoException");
    
    // =======================
    // FUNCTIONS MODULE
    // =======================
    py::module_ functions_module = m.def_submodule("functions", "Function binding demonstrations");
    
    // Basic function
    functions_module.def("add", &functions::add, "Add two integers",
                        py::arg("a"), py::arg("b"));
    
    // Function with default arguments
    functions_module.def("greet", &functions::greet, "Greet someone",
                        py::arg("name"), py::arg("greeting") = "Hello");
    
    // Overloaded functions
    functions_module.def("multiply", py::overload_cast<double, double>(&functions::multiply),
                        "Multiply two doubles", py::arg("a"), py::arg("b"));
    functions_module.def("multiply", py::overload_cast<int, int>(&functions::multiply),
                        "Multiply two integers", py::arg("a"), py::arg("b"));
    functions_module.def("multiply", py::overload_cast<const std::vector<double>&, double>(&functions::multiply),
                        "Multiply vector by scalar", py::arg("vec"), py::arg("scalar"));
    
    // Function with keyword arguments
    functions_module.def("format_message", &functions::format_message,
                        "Format a message with prefix and optional uppercase",
                        py::arg("message"), py::arg("prefix") = "[INFO]", py::arg("uppercase") = false);
    
    // Functions working with STL containers
    functions_module.def("process_list", &functions::process_list,
                        "Process a list of integers (square each + 1)",
                        py::arg("input"));
    functions_module.def("process_dict", &functions::process_dict,
                        "Process a dictionary (uppercase keys, double values)",
                        py::arg("input"));
    
    // Function that can throw exceptions
    functions_module.def("divide", &functions::divide, "Divide two integers (can throw)",
                        py::arg("a"), py::arg("b"));
    
    // Function with complex return type
    functions_module.def("complex_computation", &functions::complex_computation,
                        "Perform complex computation returning tuple of (filtered_data, stats)",
                        py::arg("data"), py::arg("threshold"));
    
    // Functions working with vectors
    functions_module.def("create_vector", &functions::create_vector,
                        "Create a vector with given size and default value",
                        py::arg("size"), py::arg("default_value") = 0);
    functions_module.def("modify_vector_inplace", &functions::modify_vector_inplace,
                        "Modify vector in place",
                        py::arg("vec"), py::arg("multiplier"));
    
    // Function with callback
    functions_module.def("process_with_callback", &functions::process_with_callback,
                        "Process data with a callback function",
                        py::arg("data"), py::arg("callback"));
    
    
    // =======================
    // CALCULATOR CLASS
    // =======================
    py::class_<Calculator>(m, "Calculator", "A calculator class demonstrating various PyBind11 features")
        // Constructors
        .def(py::init<>(), "Default constructor")
        .def(py::init<double>(), "Constructor with initial value", py::arg("initial_value"))
        .def(py::init<double, const std::string&>(), "Constructor with value and name",
             py::arg("initial_value"), py::arg("name"))
        
        // Properties (getter/setter pairs)
        .def_property("value", &Calculator::get_value, &Calculator::set_value, "Calculator value")
        .def_property("name", &Calculator::get_name, &Calculator::set_name, "Calculator name")
        
        // Methods
        .def("add", &Calculator::add, "Add value", py::arg("value"))
        .def("subtract", &Calculator::subtract, "Subtract value", py::arg("value"))
        .def("multiply", &Calculator::multiply, "Multiply by value", py::arg("value"))
        .def("divide", &Calculator::divide, "Divide by value", py::arg("value"))
        
        // Operators
        .def(py::self + py::self)
        .def(py::self - py::self)
        .def(py::self * double())
        .def(py::self == py::self)
        .def(py::self != py::self)
        
        // Static methods
        .def_static("create_named", &Calculator::create_named, 
                   "Create named calculator", py::arg("name"), py::arg("value") = 0.0)
        .def_static("pi", &Calculator::pi, "Get pi constant")
        
        // Other methods
        .def("apply_to_vector", &Calculator::apply_to_vector, 
             "Apply calculator value to vector", py::arg("values"))
        .def("to_string", &Calculator::to_string, "String representation")
        .def("__str__", &Calculator::to_string)
        .def("__repr__", &Calculator::to_string);
    
    // =======================
    // SHAPE HIERARCHY
    // =======================
    py::class_<Shape, std::shared_ptr<Shape>>(m, "Shape", "Abstract shape base class")
        .def_property_readonly("name", &Shape::get_name, "Shape name")
        .def("area", &Shape::area, "Calculate area (pure virtual)")
        .def("perimeter", &Shape::perimeter, "Calculate perimeter (pure virtual)")
        .def("description", &Shape::description, "Get shape description")
        .def_static("create_rectangle", &Shape::create_rectangle,
                   "Create rectangle", py::arg("width"), py::arg("height"), py::arg("name") = "rectangle")
        .def_static("create_circle", &Shape::create_circle,
                   "Create circle", py::arg("radius"), py::arg("name") = "circle");
    
    py::class_<Rectangle, Shape, std::shared_ptr<Rectangle>>(m, "Rectangle", "Rectangle shape")
        .def(py::init<double, double, const std::string&>(),
             "Create rectangle", py::arg("width"), py::arg("height"), py::arg("name") = "rectangle")
        .def_property("width", &Rectangle::get_width, &Rectangle::set_width, "Rectangle width")
        .def_property("height", &Rectangle::get_height, &Rectangle::set_height, "Rectangle height")
        .def("area", &Rectangle::area, "Calculate rectangle area")
        .def("perimeter", &Rectangle::perimeter, "Calculate rectangle perimeter")
        .def("description", &Rectangle::description, "Get rectangle description");
    
    py::class_<Circle, Shape, std::shared_ptr<Circle>>(m, "Circle", "Circle shape")
        .def(py::init<double, const std::string&>(),
             "Create circle", py::arg("radius"), py::arg("name") = "circle")
        .def_property("radius", &Circle::get_radius, &Circle::set_radius, "Circle radius")
        .def("area", &Circle::area, "Calculate circle area")
        .def("perimeter", &Circle::perimeter, "Calculate circle perimeter")
        .def("description", &Circle::description, "Get circle description");
    
    // =======================
    // DATA CONTAINER CLASS
    // =======================
    py::class_<DataContainer>(m, "DataContainer", "Container demonstrating property-like access and indexing")
        .def(py::init<>(), "Default constructor")
        .def(py::init<const std::vector<double>&>(), "Constructor with initial data", py::arg("initial_data"))
        
        // Properties
        .def_property("data", &DataContainer::get_data, &DataContainer::set_data, "Container data")
        .def_property("label", &DataContainer::get_label, &DataContainer::set_label, "Container label")
        .def_property("read_only", &DataContainer::is_read_only, &DataContainer::set_read_only, "Read-only flag")
        
        // Data manipulation
        .def("append", &DataContainer::append, "Append value", py::arg("value"))
        .def("extend", &DataContainer::extend, "Extend with values", py::arg("values"))
        .def("clear", &DataContainer::clear, "Clear all data")
        .def("size", &DataContainer::size, "Get size")
        .def("empty", &DataContainer::empty, "Check if empty")
        
        // Statistical methods
        .def("mean", &DataContainer::mean, "Calculate mean")
        .def("sum", &DataContainer::sum, "Calculate sum")
        .def("min", &DataContainer::min, "Find minimum")
        .def("max", &DataContainer::max, "Find maximum")
        
        // Python special methods
        .def("__len__", &DataContainer::size)
        .def("__getitem__", [](const DataContainer& dc, size_t index) { return dc[index]; })
        .def("__setitem__", [](DataContainer& dc, size_t index, double value) { dc[index] = value; })
        .def("__iter__", [](const DataContainer& dc) {
            return py::make_iterator(dc.begin(), dc.end());
        }, py::keep_alive<0, 1>());
    
    // =======================
    // RESOURCE MANAGER CLASS
    // =======================
    py::class_<ResourceManager>(m, "ResourceManager", "Demonstrates smart pointer usage")
        .def(py::init<>(), "Default constructor")
        .def(py::init<const std::string&>(), "Constructor with name", py::arg("name"))
        
        .def_property("data", &ResourceManager::get_data, &ResourceManager::set_data,
                     "Vector data")
        .def_property("name", &ResourceManager::get_name, &ResourceManager::set_name, "Manager name")
        .def_property("shared_data", &ResourceManager::get_shared_data, &ResourceManager::set_shared_data, "Shared data")
        
        .def("create_shared_resource", &ResourceManager::create_shared_resource,
             "Create shared resource", py::arg("size"), py::arg("default_value") = 0.0)
        .def("has_shared_resource", &ResourceManager::has_shared_resource, "Check if has shared resource")
        .def("shared_resource_use_count", &ResourceManager::shared_resource_use_count,
             "Get shared resource use count");
    
    // =======================
    // NUMPY DEMO MODULE
    // =======================
    py::module_ numpy_module = m.def_submodule("numpy_demo", "NumPy integration demonstrations");
    
    // Helper class for pixel indexing
    class ImagePixelAccessor {
    public:
        ImagePixelAccessor(numpy_demo::Image* img) : img_(img) {}
        
        unsigned char getitem(py::tuple index) {
            if (index.size() == 2) {
                return img_->pixel(index[0].cast<size_t>(), index[1].cast<size_t>());
            } else if (index.size() == 3) {
                return img_->pixel(index[0].cast<size_t>(), index[1].cast<size_t>(), index[2].cast<size_t>());
            }
            throw std::runtime_error("Pixel access requires 2 or 3 indices");
        }
        
        void setitem(py::tuple index, unsigned char value) {
            if (index.size() == 2) {
                img_->pixel(index[0].cast<size_t>(), index[1].cast<size_t>()) = value;
            } else if (index.size() == 3) {
                img_->pixel(index[0].cast<size_t>(), index[1].cast<size_t>(), index[2].cast<size_t>()) = value;
            } else {
                throw std::runtime_error("Pixel access requires 2 or 3 indices");
            }
        }
        
    private:
        numpy_demo::Image* img_;
    };
    
    py::class_<ImagePixelAccessor>(numpy_module, "ImagePixelAccessor")
        .def("__getitem__", &ImagePixelAccessor::getitem)
        .def("__setitem__", &ImagePixelAccessor::setitem);
    
    // Basic array operations
    numpy_module.def("square_array", &numpy_demo::square_array, "Square all elements", py::arg("input"));
    numpy_module.def("add_arrays", &numpy_demo::add_arrays, "Add two arrays", py::arg("a"), py::arg("b"));
    numpy_module.def("dot_product", &numpy_demo::dot_product, "Dot product", py::arg("a"), py::arg("b"));
    
    // Matrix class
    py::class_<numpy_demo::Matrix>(numpy_module, "Matrix", "Matrix class for linear algebra")
        .def(py::init<size_t, size_t, double>(), "Create matrix",
             py::arg("rows"), py::arg("cols"), py::arg("fill_value") = 0.0)
        .def(py::init<const std::vector<std::vector<double>>&>(), "Create from nested list",
             py::arg("matrix_data"))
        
        .def_readonly("rows", &numpy_demo::Matrix::rows, "Number of rows")
        .def_readonly("cols", &numpy_demo::Matrix::cols, "Number of columns")
        .def_readonly("data", &numpy_demo::Matrix::data, "Raw data vector")
        
        // Python-style indexing with [row, col] - the only way to access elements
        .def("__getitem__", [](const numpy_demo::Matrix& m, py::tuple index) -> double {
            if (index.size() != 2)
                throw std::runtime_error("Matrix requires 2 indices");
            return m(index[0].cast<size_t>(), index[1].cast<size_t>());
        }, "Get element with [row, col]")
        .def("__setitem__", [](numpy_demo::Matrix& m, py::tuple index, double value) {
            if (index.size() != 2)
                throw std::runtime_error("Matrix requires 2 indices");
            m(index[0].cast<size_t>(), index[1].cast<size_t>()) = value;
        }, "Set element with [row, col] = value")
        
        .def("to_nested_vector", &numpy_demo::Matrix::to_nested_vector, "Convert to nested Python list")
        .def("to_string", &numpy_demo::Matrix::to_string, "String representation")
        .def("__str__", &numpy_demo::Matrix::to_string)
        .def("__repr__", &numpy_demo::Matrix::to_string);
    
    // Matrix operations
    numpy_module.def("matrix_multiply", &numpy_demo::matrix_multiply,
                    "Matrix multiplication", py::arg("a"), py::arg("b"));
    numpy_module.def("matrix_transpose", &numpy_demo::matrix_transpose,
                    "Matrix transpose", py::arg("matrix"));
    numpy_module.def("matrix_add", &numpy_demo::matrix_add,
                    "Matrix addition", py::arg("a"), py::arg("b"));
    numpy_module.def("matrix_vector_multiply", &numpy_demo::matrix_vector_multiply,
                    "Matrix-vector multiplication", py::arg("matrix"), py::arg("vector"));
    
    // Statistical operations
    numpy_module.def("mean", &numpy_demo::mean, "Calculate mean", py::arg("data"));
    numpy_module.def("variance", &numpy_demo::variance, "Calculate variance", py::arg("data"));
    numpy_module.def("standard_deviation", &numpy_demo::standard_deviation,
                    "Calculate standard deviation", py::arg("data"));
    numpy_module.def("normalize", &numpy_demo::normalize, "Normalize data", py::arg("data"));
    
    // Signal processing
    numpy_module.def("moving_average", &numpy_demo::moving_average,
                    "Moving average", py::arg("data"), py::arg("window_size"));
    numpy_module.def("diff", &numpy_demo::diff, "Difference between consecutive elements", py::arg("data"));
    numpy_module.def("cumsum", &numpy_demo::cumsum, "Cumulative sum", py::arg("data"));
    
    // Advanced operations
    numpy_module.def("apply_function", &numpy_demo::apply_function,
                    "Apply function to each element", py::arg("input"), py::arg("func"));
    numpy_module.def("element_wise_operation", &numpy_demo::element_wise_operation,
                    "Element-wise binary operation", py::arg("a"), py::arg("b"), py::arg("op"));
    
    // Image class
    py::class_<numpy_demo::Image>(numpy_module, "Image", "Simple image class for processing demo")
        .def(py::init<size_t, size_t, size_t>(), "Create image",
             py::arg("width"), py::arg("height"), py::arg("channels") = 1)
        
        .def_readonly("width", &numpy_demo::Image::width, "Image width")
        .def_readonly("height", &numpy_demo::Image::height, "Image height")
        .def_readonly("channels", &numpy_demo::Image::channels, "Number of channels")
        .def_readonly("pixels", &numpy_demo::Image::pixels, "Raw pixel data")
        
        // Create a pixel property that returns an indexable accessor
        .def_property_readonly("pixel", [](numpy_demo::Image& img) {
            return ImagePixelAccessor(&img);
        }, py::return_value_policy::reference_internal, "Pixel accessor with [x, y] or [x, y, channel] indexing")
        
        .def("to_double_array", &numpy_demo::Image::to_double_array, "Convert to double array [0,1]")
        .def("from_double_array", &numpy_demo::Image::from_double_array,
             "Load from double array [0,1]", py::arg("data"));
    
    // Image operations
    numpy_module.def("gaussian_blur", &numpy_demo::gaussian_blur,
                    "Apply Gaussian blur", py::arg("image"), py::arg("sigma"));
    numpy_module.def("edge_detection", &numpy_demo::edge_detection,
                    "Apply edge detection", py::arg("image"));
    numpy_module.def("resize", &numpy_demo::resize,
                    "Resize image", py::arg("image"), py::arg("new_width"), py::arg("new_height"));
}
