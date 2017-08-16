#ifndef AETHER_OBJECT_H
#define AETHER_OBJECT_H

#include <atomic>
#include <functional>

namespace aether {

struct Object {
  Object() 
    : id{next_id++}
  {}

  int id;
  static std::atomic<int> next_id;

  friend std::size_t hash_value(const Object& object) {
    return std::hash<int>{}(object.id);
  }
};

} // end namespace aether

#endif
