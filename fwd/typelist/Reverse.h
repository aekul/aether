#ifndef FWD_TYPELIST_REVERSE_H
#define FWD_TYPELIST_REVERSE_H

#include "aether/fwd/TypeTraits.h"
#include "aether/fwd/typelist/TypeList.h"

namespace aether {

struct reverse_t {
  constexpr TypeList<> impl(TypeList<>) const;

  template <typename T, typename... Ts>
  constexpr auto impl(TypeList<T, Ts...>) const;

  template <typename T0, typename T1, typename... Ts>
  constexpr auto impl(TypeList<T0, T1, Ts...>) const;

  template <typename T0, typename T1, typename T2, typename T3, typename... Ts>
  constexpr auto impl(TypeList<T0, T1, T2, T3, Ts...>) const;

  template <typename T0, typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename T7, typename... Ts>
  constexpr auto impl(TypeList<T0, T1, T2, T3, T4, T5, T6, T7, Ts...>) const;

  template <typename T0, typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename T7, typename T8, typename T9, typename T10, typename T11, typename T12, typename T13, typename T14, typename T15, typename... Ts>
  constexpr auto impl(TypeList<T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, Ts...>) const;

  template <typename... Ts>
  constexpr auto operator()(TypeList<Ts...>) const;
};

constexpr reverse_t reverse{};

} // end namespace aether

#endif
