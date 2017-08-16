#ifndef TYPELIST_FRONT_H
#define TYPELIST_FRONT_H

#include "../fwd/typelist/Front.h"

namespace aether {

template <typename T, typename... Ts>
constexpr T front_t::operator()(TypeList<T, Ts...>) const {
  return {}; 
}

template <typename T, typename... Ts>
constexpr T front_t::operator()(TypeSet<T, Ts...>) const {
  return {}; 
}

} // end namespace aether

#endif
