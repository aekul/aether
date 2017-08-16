#ifndef TYPELIST_UNIQUE_H
#define TYPELIST_UNIQUE_H

#include "aether/fwd/typelist/Unique.h"

namespace aether {

template <typename... Rs>
constexpr auto unique_t::impl(TypeList<>, TypeSet<Rs...>) const {
  return TypeList<Rs...>{};
}

template <typename T, typename... Ts, typename... Rs>
constexpr auto unique_t::impl(TypeList<T, Ts...>, TypeSet<Rs...>) const {
  return impl(TypeList<Ts...>{}, TypeSet<Rs...>{}.Put(T{}));
}

template <typename... Ts>
constexpr auto unique_t::operator()(TypeList<Ts...>) const {
  return impl(TypeList<Ts...>{}, TypeSet<>{});
}

} // end namespace aether

#endif
