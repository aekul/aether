#ifndef AETHER_CACHED_FUNCTION_H
#define AETHER_CACHED_FUNCTION_H

#include <boost/hana.hpp>
#include <boost/functional/hash.hpp>
#include <unordered_map>

using namespace boost;

namespace aether {

template <typename T, typename R>
struct CachedFunction {
  template <typename... Args>
  R operator()(Args&&... args) const {
    auto key = Derived().hash_value(std::forward<Args>(args)...);
    auto it = cache.find(key);

    if (it == cache.end()) {
      auto value = Derived().impl(std::forward<Args>(args)...);
      cache.insert({key, value});
      return value;
    }

    return it->second;
  } 

  template <typename... Args>
  std::size_t hash_value(Args&&... args) const {
    std::size_t seed{0};
    hana::for_each(hana::make_tuple(std::forward<Args>(args)...), [&](const auto& arg) {
      boost::hash_combine(seed, arg);
    });
    return seed;
  }

  T& Derived() {
    return static_cast<T&>(*this);
  }

  const T& Derived() const {
    return static_cast<const T&>(*this);
  }

  mutable std::unordered_map<std::size_t, R> cache;
};

} // end namespace aether

#endif
