#ifndef TYPELIST_REVERSE_H
#define TYPELIST_REVERSE_H

#include "aether/fwd/typelist/Reverse.h"

namespace aether {

constexpr TypeList<> reverse_t::impl(TypeList<>) const {
  return {};
}

template <typename T, typename... Ts>
constexpr auto reverse_t::impl(TypeList<T, Ts...>) const {
  return impl(TypeList<Ts...>{}).Concat(TypeList<T>{});
}

template <typename T0, typename T1, typename... Ts>
constexpr auto reverse_t::impl(TypeList<T0, T1, Ts...>) const {
  return impl(TypeList<Ts...>{}).Concat(TypeList<T1, T0>{});
}

template <typename T0, typename T1, typename T2, typename T3, typename... Ts>
constexpr auto reverse_t::impl(TypeList<T0, T1, T2, T3, Ts...>) const {
  return impl(TypeList<Ts...>{}).Concat(TypeList<T3, T2, T1, T0>{});
}

template <typename T0, typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename T7, typename... Ts>
constexpr auto reverse_t::impl(TypeList<T0, T1, T2, T3, T4, T5, T6, T7, Ts...>) const {
  return impl(TypeList<Ts...>{}).Concat(TypeList<T7, T6, T5, T4, T3, T2, T1, T0>{});
}

template <typename T0, typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename T7, typename T8, typename T9, typename T10, typename T11, typename T12, typename T13, typename T14, typename T15, typename... Ts>
constexpr auto reverse_t::impl(TypeList<T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, Ts...>) const {
  return impl(TypeList<Ts...>{}).Concat(TypeList<T15, T14, T13, T12, T11, T10, T9, T8, T7, T6, T5, T4, T3, T2, T1, T0>{});
}

template <typename... Ts>
constexpr auto reverse_t::operator()(TypeList<Ts...>) const {
  return impl(TypeList<Ts...>{}); 
}

} // end namespace aether

#endif
