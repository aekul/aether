#ifndef TYPELIST_SUM_H
#define TYPELIST_SUM_H

#include "../fwd/typelist/Sum.h"

namespace aether {

template <typename T>
constexpr T sum_t::impl(TypeList<T>) const {
  return {};
}

template <typename A, typename B, typename... Ts>
constexpr auto sum_t::impl(TypeList<A, B, Ts...>) const {
  return A{} + impl(TypeList<B, Ts...>{});
}

template <typename... Ts>
constexpr auto sum_t::operator()(TypeList<Ts...>) const {
  return impl(TypeList<Ts...>{});
}

template <typename... Ts>
constexpr auto sum_t::operator()(Ts...) const {
  return impl(TypeList<Ts...>{});
}

} // end namespace aether

#endif
