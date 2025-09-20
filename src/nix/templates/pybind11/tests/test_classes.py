"""
Tests for PyBind11 class bindings.

This module tests all class binding features including:
- Basic classes with constructors, methods, and properties
- Inheritance hierarchies with virtual methods
- Abstract base classes and pure virtual methods
- Operator overloading
- Static methods and properties
- Property access patterns
- Resource management with smart pointers
"""

import pytest
import math
from typing import List

import pybind_demo


class TestCalculatorClass:
    """Test basic Calculator class functionality."""
    
    def test_default_constructor(self):
        """Test default constructor."""
        calc = pybind_demo.Calculator()
        assert calc.value == 0.0
        assert calc.name == ""
    
    def test_value_constructor(self):
        """Test constructor with initial value."""
        calc = pybind_demo.Calculator(42.5)
        assert calc.value == 42.5
        assert calc.name == ""
    
    def test_full_constructor(self):
        """Test constructor with value and name."""
        calc = pybind_demo.Calculator(10.0, "test_calc")
        assert calc.value == 10.0
        assert calc.name == "test_calc"
    
    def test_property_access(self):
        """Test property getters and setters."""
        calc = pybind_demo.Calculator()
        
        # Test value property
        calc.value = 25.5
        assert calc.value == 25.5
        
        # Test name property
        calc.name = "my_calculator"
        assert calc.name == "my_calculator"
    
    def test_property_types(self):
        """Test property type validation."""
        calc = pybind_demo.Calculator()
        
        # Value should accept numeric types
        calc.value = 10
        assert calc.value == 10.0
        
        calc.value = 3.14159
        assert abs(calc.value - 3.14159) < 1e-10
        
        # Name should accept strings
        calc.name = "test"
        assert calc.name == "test"
    
    def test_arithmetic_methods(self):
        """Test arithmetic operation methods."""
        calc = pybind_demo.Calculator(10.0, "test")
        
        # Test add
        result = calc.add(5.0)
        assert calc.value == 15.0
        assert result == calc  # Should return self for chaining
        
        # Test subtract
        calc.subtract(3.0)
        assert calc.value == 12.0
        
        # Test multiply
        calc.multiply(2.0)
        assert calc.value == 24.0
        
        # Test divide
        calc.divide(4.0)
        assert calc.value == 6.0
    
    def test_method_chaining(self):
        """Test method chaining capability."""
        calc = pybind_demo.Calculator(10.0)
        
        # Chain multiple operations
        result = calc.add(5.0).multiply(2.0).subtract(10.0).divide(2.0)
        
        # Compute expected value: ((10 + 5) * 2 - 10) / 2
        expected = ((10.0 + 5.0) * 2.0 - 10.0) / 2.0
        assert calc.value == expected
        assert result == calc  # Should return self
    
    def test_divide_by_zero_handling(self):
        """Test division by zero in Calculator."""
        calc = pybind_demo.Calculator(10.0)
        
        with pytest.raises(pybind_demo.PyBindDemoException) as exc_info:
            calc.divide(0.0)
        
        assert "Division by zero" in str(exc_info.value)
        # Value should remain unchanged after exception
        assert calc.value == 10.0
    
    def test_operator_overloading(self):
        """Test operator overloading."""
        calc1 = pybind_demo.Calculator(10.0, "calc1")
        calc2 = pybind_demo.Calculator(5.0, "calc2")
        
        # Test addition
        calc3 = calc1 + calc2
        assert calc3.value == 15.0
        assert calc3.name == "calc1 + calc2"
        
        # Test subtraction
        calc4 = calc1 - calc2
        assert calc4.value == 5.0
        assert calc4.name == "calc1 - calc2"
        
        # Test scalar multiplication
        calc5 = calc1 * 3.0
        assert calc5.value == 30.0
        
        # Original calculators should be unchanged
        assert calc1.value == 10.0
        assert calc2.value == 5.0
    
    def test_equality_operators(self):
        """Test equality and inequality operators."""
        calc1 = pybind_demo.Calculator(10.0, "test")
        calc2 = pybind_demo.Calculator(10.0, "test")
        calc3 = pybind_demo.Calculator(5.0, "test")
        calc4 = pybind_demo.Calculator(10.0, "different")
        
        # Test equality
        assert calc1 == calc2
        assert not (calc1 == calc3)
        assert not (calc1 == calc4)
        
        # Test inequality
        assert not (calc1 != calc2)
        assert calc1 != calc3
        assert calc1 != calc4
    
    def test_static_methods(self):
        """Test static methods."""
        # Test create_named
        calc = pybind_demo.Calculator.create_named("static_calc", 42.0)
        assert calc.name == "static_calc"
        assert calc.value == 42.0
        
        # Test create_named with default value
        calc_default = pybind_demo.Calculator.create_named("default_calc")
        assert calc_default.name == "default_calc"
        assert calc_default.value == 0.0
        
        # Test pi constant
        pi_value = pybind_demo.Calculator.pi()
        assert abs(pi_value - math.pi) < 1e-10
    
    def test_apply_to_vector(self):
        """Test vector operations."""
        calc = pybind_demo.Calculator(5.0)
        
        # Test with list
        values = [1.0, 2.0, 3.0, 4.0]
        result = calc.apply_to_vector(values)
        expected = [x + 5.0 for x in values]
        
        assert len(result) == len(expected)
        for r, e in zip(result, expected):
            assert abs(r - e) < 1e-10
    
    def test_string_representation(self):
        """Test string representation methods."""
        calc = pybind_demo.Calculator(42.5, "test_calc")
        
        str_repr = calc.to_string()
        assert "test_calc" in str_repr
        assert "42.5" in str_repr
        
        # Test __str__ and __repr__
        assert str(calc) == str_repr
        assert repr(calc) == str_repr


class TestShapeHierarchy:
    """Test inheritance hierarchy with Shape base class."""
    
    def test_shape_base_class_abstract(self):
        """Test that Shape is abstract and cannot be instantiated directly."""
        # Shape should not be directly instantiable (it's abstract)
        with pytest.raises(TypeError):
            pybind_demo.Shape()
    
    def test_rectangle_creation(self):
        """Test Rectangle creation and basic functionality."""
        rect = pybind_demo.Rectangle(5.0, 3.0, "test_rect")
        
        assert rect.width == 5.0
        assert rect.height == 3.0
        assert rect.name == "test_rect"
    
    def test_rectangle_default_name(self):
        """Test Rectangle creation with default name."""
        rect = pybind_demo.Rectangle(4.0, 6.0)
        
        assert rect.width == 4.0
        assert rect.height == 6.0
        assert rect.name == "rectangle"
    
    def test_rectangle_properties(self):
        """Test Rectangle property access."""
        rect = pybind_demo.Rectangle(5.0, 3.0)
        
        # Test width property
        rect.width = 10.0
        assert rect.width == 10.0
        
        # Test height property
        rect.height = 7.0
        assert rect.height == 7.0
    
    def test_rectangle_area_perimeter(self):
        """Test Rectangle area and perimeter calculations."""
        rect = pybind_demo.Rectangle(5.0, 3.0)
        
        # Test area
        area = rect.area()
        assert area == 15.0
        
        # Test perimeter
        perimeter = rect.perimeter()
        assert perimeter == 16.0
        
        # Test after property changes
        rect.width = 10.0
        rect.height = 4.0
        assert rect.area() == 40.0
        assert rect.perimeter() == 28.0
    
    def test_circle_creation(self):
        """Test Circle creation and basic functionality."""
        circle = pybind_demo.Circle(3.0, "test_circle")
        
        assert circle.radius == 3.0
        assert circle.name == "test_circle"
    
    def test_circle_default_name(self):
        """Test Circle creation with default name."""
        circle = pybind_demo.Circle(2.5)
        
        assert circle.radius == 2.5
        assert circle.name == "circle"
    
    def test_circle_properties(self):
        """Test Circle property access."""
        circle = pybind_demo.Circle(3.0)
        
        # Test radius property
        circle.radius = 5.0
        assert circle.radius == 5.0
    
    def test_circle_area_perimeter(self):
        """Test Circle area and perimeter calculations."""
        circle = pybind_demo.Circle(2.0)
        
        # Test area (π * r²)
        area = circle.area()
        expected_area = math.pi * 4.0
        assert abs(area - expected_area) < 1e-10
        
        # Test perimeter (2 * π * r)
        perimeter = circle.perimeter()
        expected_perimeter = 2 * math.pi * 2.0
        assert abs(perimeter - expected_perimeter) < 1e-10
        
        # Test after radius change
        circle.radius = 3.0
        area = circle.area()
        expected_area = math.pi * 9.0
        assert abs(area - expected_area) < 1e-10
    
    def test_shape_polymorphism(self):
        """Test polymorphic behavior through Shape interface."""
        # Create shapes through factory methods
        rect = pybind_demo.Shape.create_rectangle(4.0, 3.0, "poly_rect")
        circle = pybind_demo.Shape.create_circle(2.0, "poly_circle")
        
        # Both should behave as Shape objects
        assert rect.name == "poly_rect"
        assert circle.name == "poly_circle"
        
        # Virtual methods should work correctly
        assert rect.area() == 12.0
        assert abs(circle.area() - (math.pi * 4.0)) < 1e-10
        
        assert rect.perimeter() == 14.0
        assert abs(circle.perimeter() - (4.0 * math.pi)) < 1e-10
    
    def test_shape_factory_methods(self):
        """Test Shape factory methods."""
        # Test create_rectangle
        rect1 = pybind_demo.Shape.create_rectangle(5.0, 4.0)
        assert rect1.name == "rectangle"  # Default name
        assert isinstance(rect1, pybind_demo.Rectangle)
        
        rect2 = pybind_demo.Shape.create_rectangle(3.0, 2.0, "custom_rect")
        assert rect2.name == "custom_rect"
        
        # Test create_circle
        circle1 = pybind_demo.Shape.create_circle(3.0)
        assert circle1.name == "circle"  # Default name
        assert isinstance(circle1, pybind_demo.Circle)
        
        circle2 = pybind_demo.Shape.create_circle(1.5, "custom_circle")
        assert circle2.name == "custom_circle"
    
    def test_shape_description(self):
        """Test virtual description method."""
        rect = pybind_demo.Rectangle(5.0, 3.0, "test_rect")
        circle = pybind_demo.Circle(2.0, "test_circle")
        
        rect_desc = rect.description()
        circle_desc = circle.description()
        
        # Description should include shape type and name
        assert "Rectangle" in rect_desc
        assert "test_rect" in rect_desc
        
        assert "Circle" in circle_desc
        assert "test_circle" in circle_desc
    
    def test_inheritance_isinstance(self):
        """Test isinstance relationships in inheritance."""
        rect = pybind_demo.Rectangle(3.0, 4.0)
        circle = pybind_demo.Circle(2.0)
        
        # Should be instances of their specific types
        assert isinstance(rect, pybind_demo.Rectangle)
        assert isinstance(circle, pybind_demo.Circle)
        
        # Should also be instances of Shape base class
        assert isinstance(rect, pybind_demo.Shape)
        assert isinstance(circle, pybind_demo.Shape)
        
        # Should not be instances of each other's types
        assert not isinstance(rect, pybind_demo.Circle)
        assert not isinstance(circle, pybind_demo.Rectangle)


class TestDataContainer:
    """Test DataContainer class with indexing and iteration."""
    
    def test_default_constructor(self):
        """Test default constructor."""
        container = pybind_demo.DataContainer()
        
        assert container.size() == 0
        assert container.empty()
        assert container.label == ""
        assert not container.read_only
    
    def test_data_constructor(self):
        """Test constructor with initial data."""
        data = [1.0, 2.0, 3.0, 4.0, 5.0]
        container = pybind_demo.DataContainer(data)
        
        assert container.size() == 5
        assert not container.empty()
        assert list(container.data) == data
    
    def test_properties(self):
        """Test property access."""
        container = pybind_demo.DataContainer()
        
        # Test data property
        data = [10.0, 20.0, 30.0]
        container.data = data
        assert list(container.data) == data
        
        # Test label property
        container.label = "test_container"
        assert container.label == "test_container"
        
        # Test read_only property
        container.read_only = True
        assert container.read_only
        
        container.read_only = False
        assert not container.read_only
    
    def test_data_manipulation(self):
        """Test data manipulation methods."""
        container = pybind_demo.DataContainer()
        
        # Test append
        container.append(1.0)
        assert container.size() == 1
        assert container[0] == 1.0
        
        container.append(2.0)
        container.append(3.0)
        assert container.size() == 3
        
        # Test extend
        container.extend([4.0, 5.0, 6.0])
        assert container.size() == 6
        assert list(container.data) == [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
        
        # Test clear
        container.clear()
        assert container.size() == 0
        assert container.empty()
    
    def test_indexing(self):
        """Test indexing operations."""
        data = [1.0, 2.0, 3.0, 4.0, 5.0]
        container = pybind_demo.DataContainer(data)
        
        # Test __getitem__
        assert container[0] == 1.0
        assert container[2] == 3.0
        assert container[4] == 5.0
        
        # Test __setitem__
        container[1] = 10.0
        assert container[1] == 10.0
        assert container.data[1] == 10.0
        
        # Test bounds checking
        with pytest.raises(pybind_demo.PyBindDemoException):
            _ = container[10]
        
        with pytest.raises(pybind_demo.PyBindDemoException):
            container[10] = 1.0
    
    def test_len_and_iteration(self):
        """Test __len__ and iteration."""
        data = [1.0, 2.0, 3.0, 4.0, 5.0]
        container = pybind_demo.DataContainer(data)
        
        # Test __len__
        assert len(container) == 5
        
        # Test iteration
        values = []
        for value in container:
            values.append(value)
        
        assert values == data
        
        # Test list conversion
        assert list(container) == data
    
    def test_statistical_methods(self):
        """Test statistical computation methods."""
        data = [1.0, 2.0, 3.0, 4.0, 5.0]
        container = pybind_demo.DataContainer(data)
        
        # Test mean
        mean = container.mean()
        assert abs(mean - 3.0) < 1e-10
        
        # Test sum
        total = container.sum()
        assert abs(total - 15.0) < 1e-10
        
        # Test min
        minimum = container.min()
        assert minimum == 1.0
        
        # Test max
        maximum = container.max()
        assert maximum == 5.0
    
    def test_statistical_methods_empty(self):
        """Test statistical methods on empty container."""
        container = pybind_demo.DataContainer()
        
        # Should handle empty data gracefully
        assert container.mean() == 0.0
        assert container.sum() == 0.0
        assert container.min() == 0.0
        assert container.max() == 0.0
    
    def test_statistical_methods_single(self):
        """Test statistical methods on single element."""
        container = pybind_demo.DataContainer([42.0])
        
        assert container.mean() == 42.0
        assert container.sum() == 42.0
        assert container.min() == 42.0
        assert container.max() == 42.0


class TestResourceManager:
    """Test ResourceManager class with smart pointers."""
    
    def test_default_constructor(self):
        """Test default constructor."""
        manager = pybind_demo.ResourceManager()
        
        assert manager.name == ""
        assert not manager.has_shared_resource()
        assert manager.shared_resource_use_count() == 0
    
    def test_name_constructor(self):
        """Test constructor with name."""
        manager = pybind_demo.ResourceManager("test_manager")
        
        assert manager.name == "test_manager"
        assert not manager.has_shared_resource()
    
    def test_name_property(self):
        """Test name property access."""
        manager = pybind_demo.ResourceManager()
        
        manager.name = "my_manager"
        assert manager.name == "my_manager"
    
    def test_create_shared_resource(self):
        """Test creating shared resources."""
        manager = pybind_demo.ResourceManager("test")
        
        # Create shared resource
        manager.create_shared_resource(5, 2.5)
        
        assert manager.has_shared_resource()
        assert manager.shared_resource_use_count() >= 1
        
        # Check the data
        data = manager.shared_data
        assert len(data) == 5
        assert all(abs(x - 2.5) < 1e-10 for x in data)
    
    def test_create_shared_resource_default(self):
        """Test creating shared resource with default value."""
        manager = pybind_demo.ResourceManager()
        
        manager.create_shared_resource(3)
        
        assert manager.has_shared_resource()
        data = manager.shared_data
        assert len(data) == 3
        assert all(x == 0.0 for x in data)
    
    def test_shared_data_property(self):
        """Test shared_data property access."""
        manager = pybind_demo.ResourceManager()
        
        # Initially no shared data
        assert manager.shared_data is None
        
        # Create resource
        manager.create_shared_resource(4, 1.0)
        data = manager.shared_data
        
        assert data is not None
        assert len(data) == 4
        
        # Modify through property
        new_data = [10.0, 20.0, 30.0]
        manager.shared_data = new_data
        
        retrieved_data = manager.shared_data
        assert list(retrieved_data) == new_data
    
    def test_shared_resource_reference_counting(self):
        """Test reference counting behavior."""
        manager1 = pybind_demo.ResourceManager("manager1")
        manager2 = pybind_demo.ResourceManager("manager2")
        
        # Create resource in manager1
        manager1.create_shared_resource(3, 5.0)
        initial_count = manager1.shared_resource_use_count()
        assert initial_count >= 1
        
        # Share resource with manager2
        manager2.shared_data = manager1.shared_data
        
        # Both should have the resource
        assert manager1.has_shared_resource()
        assert manager2.has_shared_resource()
        
        # Use count should have increased
        assert manager1.shared_resource_use_count() >= initial_count
        assert manager2.shared_resource_use_count() == manager1.shared_resource_use_count()
        
        # Data should be the same
        assert list(manager1.shared_data) == list(manager2.shared_data)


@pytest.mark.integration
class TestClassIntegration:
    """Integration tests combining multiple class features."""
    
    def test_calculator_with_shapes(self):
        """Test using Calculator with Shape calculations."""
        calc = pybind_demo.Calculator(0.0, "shape_calc")
        
        # Calculate total area of multiple shapes
        shapes = [
            pybind_demo.Rectangle(5.0, 3.0),
            pybind_demo.Circle(2.0),
            pybind_demo.Rectangle(4.0, 4.0),
        ]
        
        total_area = 0.0
        for shape in shapes:
            total_area += shape.area()
        
        calc.value = total_area
        
        # Verify calculation
        expected = 15.0 + (math.pi * 4.0) + 16.0
        assert abs(calc.value - expected) < 1e-10
    
    def test_data_container_with_shape_data(self):
        """Test DataContainer storing shape measurements."""
        container = pybind_demo.DataContainer()
        container.label = "shape_areas"
        
        # Add areas from various shapes
        shapes = [
            pybind_demo.Rectangle(3.0, 4.0),  # area = 12
            pybind_demo.Circle(1.0),           # area = π
            pybind_demo.Rectangle(2.0, 2.0),  # area = 4
        ]
        
        for shape in shapes:
            container.append(shape.area())
        
        # Test statistics
        assert container.size() == 3
        total_area = container.sum()
        expected_total = 12.0 + math.pi + 4.0
        assert abs(total_area - expected_total) < 1e-10
        
        mean_area = container.mean()
        expected_mean = expected_total / 3.0
        assert abs(mean_area - expected_mean) < 1e-10
    
    def test_resource_manager_with_calculator_data(self):
        """Test ResourceManager storing Calculator results."""
        manager = pybind_demo.ResourceManager("calc_results")
        
        # Perform calculations and store results
        calcs = [
            pybind_demo.Calculator(10.0).add(5.0).multiply(2.0),  # 30.0
            pybind_demo.Calculator(20.0).subtract(5.0).divide(3.0),  # 5.0
            pybind_demo.Calculator(7.0).multiply(6.0),  # 42.0
        ]
        
        results = [calc.value for calc in calcs]
        manager.shared_data = results
        
        # Verify storage
        assert manager.has_shared_resource()
        stored_data = list(manager.shared_data)
        assert stored_data == [30.0, 5.0, 42.0]
    
    def test_complex_object_interaction(self):
        """Test complex interactions between multiple object types."""
        # Create a scenario where we process shape data through multiple classes
        
        # 1. Create shapes
        shapes = [
            pybind_demo.Shape.create_rectangle(5.0, 4.0, "rect1"),
            pybind_demo.Shape.create_circle(3.0, "circle1"),
            pybind_demo.Shape.create_rectangle(2.0, 6.0, "rect2"),
        ]
        
        # 2. Calculate perimeters and store in container
        perimeter_container = pybind_demo.DataContainer()
        perimeter_container.label = "perimeters"
        
        for shape in shapes:
            perimeter_container.append(shape.perimeter())
        
        # 3. Use calculator to process the mean perimeter
        calc = pybind_demo.Calculator(perimeter_container.mean(), "perimeter_calc")
        calc.multiply(1.1)  # Add 10% margin
        
        # 4. Store final result in resource manager
        manager = pybind_demo.ResourceManager("final_results")
        manager.create_shared_resource(1, calc.value)
        
        # 5. Verify the entire pipeline
        expected_perimeters = [18.0, 6.0 * math.pi, 16.0]
        expected_mean = sum(expected_perimeters) / 3.0
        expected_final = expected_mean * 1.1
        
        assert abs(calc.value - expected_final) < 1e-10
        assert len(manager.shared_data) == 1
        assert abs(manager.shared_data[0] - expected_final) < 1e-10
