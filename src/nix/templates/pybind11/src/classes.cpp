#include "../include/classes.h"
#include <algorithm>
#include <numeric>
#include <sstream>
#include <iomanip>
#include <cmath>
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

namespace py = pybind11;

namespace pybind_demo {

// Calculator implementation
Calculator::Calculator() : value_(0.0), name_("") {}

Calculator::Calculator(double initial_value) : value_(initial_value), name_("") {}

Calculator::Calculator(double initial_value, const std::string& name) 
    : value_(initial_value), name_(name) {}

Calculator::Calculator(const Calculator& other) 
    : value_(other.value_), name_(other.name_) {}

Calculator& Calculator::operator=(const Calculator& other) {
    if (this != &other) {
        value_ = other.value_;
        name_ = other.name_;
    }
    return *this;
}

Calculator::~Calculator() = default;

double Calculator::get_value() const {
    return value_;
}

void Calculator::set_value(double value) {
    value_ = value;
}

const std::string& Calculator::get_name() const {
    return name_;
}

void Calculator::set_name(const std::string& name) {
    name_ = name;
}

Calculator& Calculator::add(double value) {
    value_ += value;
    return *this;
}

Calculator& Calculator::subtract(double value) {
    value_ -= value;
    return *this;
}

Calculator& Calculator::multiply(double value) {
    value_ *= value;
    return *this;
}

Calculator& Calculator::divide(double value) {
    if (value == 0.0) {
        throw PyBindDemoException("Division by zero");
    }
    value_ /= value;
    return *this;
}

Calculator Calculator::operator+(const Calculator& other) const {
    return Calculator(value_ + other.value_, name_ + " + " + other.name_);
}

Calculator Calculator::operator-(const Calculator& other) const {
    return Calculator(value_ - other.value_, name_ + " - " + other.name_);
}

Calculator Calculator::operator*(double scalar) const {
    return Calculator(value_ * scalar, name_ + " * " + std::to_string(scalar));
}

bool Calculator::operator==(const Calculator& other) const {
    return std::abs(value_ - other.value_) < 1e-9 && name_ == other.name_;
}

bool Calculator::operator!=(const Calculator& other) const {
    return !(*this == other);
}

Calculator Calculator::create_named(const std::string& name, double value) {
    return Calculator(value, name);
}

double Calculator::pi() {
    return 3.141592653589793;
}

std::vector<double> Calculator::apply_to_vector(const std::vector<double>& values) const {
    std::vector<double> result;
    result.reserve(values.size());
    
    for (const auto& val : values) {
        result.push_back(val + value_);
    }
    
    return result;
}

std::string Calculator::to_string() const {
    std::ostringstream oss;
    oss << "Calculator(name='" << name_ << "', value=" << std::fixed << std::setprecision(2) << value_ << ")";
    return oss.str();
}

std::ostream& operator<<(std::ostream& os, const Calculator& calc) {
    os << calc.to_string();
    return os;
}

// Shape static factory methods
std::shared_ptr<Shape> Shape::create_rectangle(double width, double height, const std::string& name) {
    return std::make_shared<Rectangle>(width, height, name);
}

std::shared_ptr<Shape> Shape::create_circle(double radius, const std::string& name) {
    return std::make_shared<Circle>(radius, name);
}

// Rectangle implementation
Rectangle::Rectangle(double width, double height, const std::string& name) 
    : Shape(name), width_(width), height_(height) {
    if (width <= 0 || height <= 0) {
        throw PyBindDemoException("Rectangle dimensions must be positive");
    }
}

void Rectangle::set_width(double width) {
    if (width <= 0) {
        throw PyBindDemoException("Width must be positive");
    }
    width_ = width;
}

void Rectangle::set_height(double height) {
    if (height <= 0) {
        throw PyBindDemoException("Height must be positive");
    }
    height_ = height;
}

double Rectangle::area() const {
    return width_ * height_;
}

double Rectangle::perimeter() const {
    return 2 * (width_ + height_);
}

std::string Rectangle::description() const {
    std::ostringstream oss;
    oss << "Rectangle '" << name_ << "' with width=" << width_ << " and height=" << height_;
    return oss.str();
}

// Circle implementation
Circle::Circle(double radius, const std::string& name) 
    : Shape(name), radius_(radius) {
    if (radius <= 0) {
        throw PyBindDemoException("Circle radius must be positive");
    }
}

void Circle::set_radius(double radius) {
    if (radius <= 0) {
        throw PyBindDemoException("Radius must be positive");
    }
    radius_ = radius;
}

double Circle::area() const {
    return 3.141592653589793 * radius_ * radius_;
}

double Circle::perimeter() const {
    return 2 * 3.141592653589793 * radius_;
}

std::string Circle::description() const {
    std::ostringstream oss;
    oss << "Circle '" << name_ << "' with radius=" << radius_;
    return oss.str();
}

// DataContainer implementation
DataContainer::DataContainer() : read_only_(false) {}

DataContainer::DataContainer(const std::vector<double>& initial_data) 
    : data_(initial_data), read_only_(false) {}

const std::vector<double>& DataContainer::get_data() const {
    return data_;
}

void DataContainer::set_data(const std::vector<double>& data) {
    if (read_only_) {
        throw PyBindDemoException("Cannot modify read-only container");
    }
    data_ = data;
}

const std::string& DataContainer::get_label() const {
    return label_;
}

void DataContainer::set_label(const std::string& label) {
    label_ = label;
}

bool DataContainer::is_read_only() const {
    return read_only_;
}

void DataContainer::set_read_only(bool read_only) {
    read_only_ = read_only;
}

void DataContainer::append(double value) {
    if (read_only_) {
        throw PyBindDemoException("Cannot modify read-only container");
    }
    data_.push_back(value);
}

void DataContainer::extend(const std::vector<double>& values) {
    if (read_only_) {
        throw PyBindDemoException("Cannot modify read-only container");
    }
    data_.insert(data_.end(), values.begin(), values.end());
}

void DataContainer::clear() {
    if (read_only_) {
        throw PyBindDemoException("Cannot modify read-only container");
    }
    data_.clear();
}

size_t DataContainer::size() const {
    return data_.size();
}

bool DataContainer::empty() const {
    return data_.empty();
}

double DataContainer::mean() const {
    if (data_.empty()) {
        return 0.0;  // Return 0 for empty container as expected by tests
    }
    return std::accumulate(data_.begin(), data_.end(), 0.0) / data_.size();
}

double DataContainer::sum() const {
    return std::accumulate(data_.begin(), data_.end(), 0.0);
}

double DataContainer::min() const {
    if (data_.empty()) {
        return 0.0;  // Return 0 for empty container as expected by tests
    }
    return *std::min_element(data_.begin(), data_.end());
}

double DataContainer::max() const {
    if (data_.empty()) {
        return 0.0;  // Return 0 for empty container as expected by tests
    }
    return *std::max_element(data_.begin(), data_.end());
}

double& DataContainer::operator[](size_t index) {
    if (index >= data_.size()) {
        throw PyBindDemoException("Index out of bounds");
    }
    if (read_only_) {
        throw PyBindDemoException("Cannot modify read-only container");
    }
    return data_[index];
}

const double& DataContainer::operator[](size_t index) const {
    if (index >= data_.size()) {
        throw PyBindDemoException("Index out of bounds");
    }
    return data_[index];
}

// ResourceManager implementation
ResourceManager::ResourceManager() : unique_name_(std::make_unique<std::string>("")) {}

ResourceManager::ResourceManager(const std::string& name) 
    : unique_name_(std::make_unique<std::string>(name)) {}

std::vector<double> ResourceManager::get_data() const {
    return data_;
}

void ResourceManager::set_data(const std::vector<double>& data) {
    data_ = data;
}

const std::string& ResourceManager::get_name() const {
    return *unique_name_;
}

void ResourceManager::set_name(const std::string& name) {
    *unique_name_ = name;
}

void ResourceManager::create_shared_resource(size_t size, double default_value) {
    shared_data_ = std::make_shared<std::vector<double>>(size, default_value);
}

bool ResourceManager::has_shared_resource() const {
    return shared_data_ != nullptr;
}

size_t ResourceManager::shared_resource_use_count() const {
    return shared_data_ ? shared_data_.use_count() : 0;
}

py::object ResourceManager::get_shared_data() const {
    if (shared_data_) {
        return py::cast(*shared_data_);
    } else {
        return py::none();
    }
}

void ResourceManager::set_shared_data(const std::vector<double>& data) {
    shared_data_ = std::make_shared<std::vector<double>>(data);
}

} // namespace pybind_demo
