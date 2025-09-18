#include "object_manager.hpp"

namespace cpp_bridge {

// Static member definitions
ObjectManager* ObjectManager::instance_ = nullptr;
std::mutex ObjectManager::instance_mutex_;

} // namespace cpp_bridge