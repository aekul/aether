#ifndef TYPELIST_REMOVE_FRONT_H
#define TYPELIST_REMOVE_FRONT_H

#include "../fwd/typelist/RemoveFront.h"

namespace aether {

template <typename T, typename... Ts>
constexpr TypeList<Ts...> remove_front_t::operator()(TypeList<T, Ts...>) const {
  return {};
}

template <typename T, typename... Ts>
constexpr TypeSet<Ts...> remove_front_t::operator()(TypeSet<T, Ts...>) const {
  return {};
}

} // end namespace aether

#endif
