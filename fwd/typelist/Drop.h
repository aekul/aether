#ifndef FWD_TYPELIST_DROP_H
#define FWD_TYPELIST_DROP_H

#include <utility>

#include "aether/fwd/typelist/TypeList.h"
#include "aether/fwd/TypeTraits.h"

namespace aether {

struct drop_from_front_t {
  template <std::size_t N, typename... Ts, EnableIf<(N > 0)> = 0>
  constexpr auto operator()(_size_t<N>, TypeList<Ts...>) const;

  template <std::size_t N, typename... Ts, EnableIf<(N <= 0)> = 0>
  constexpr TypeList<Ts...> operator()(_size_t<N>, TypeList<Ts...>) const;
};

constexpr drop_from_front_t drop_from_front{};

struct drop_from_back_t {
  template <typename... Ts>
  constexpr TypeList<> impl(_size_t<0>, TypeList<Ts...>) const;

  template <std::size_t N, typename T, typename... Ts, EnableIf<(N >= 1 && N < 2)> = 0>
  constexpr auto impl(_size_t<N>, TypeList<T, Ts...>) const;

  template <std::size_t N, typename T0, typename T1, typename... Ts, EnableIf<(N >= 2 && N < 4)> = 0>
  constexpr auto impl(_size_t<N>, TypeList<T0, T1, Ts...>) const;

  template <std::size_t N, typename T0, typename T1, typename T2, typename T3, typename... Ts, EnableIf<(N >= 4 && N < 8)> = 0>
  constexpr auto impl(_size_t<N>, TypeList<T0, T1, T2, T3, Ts...>) const;

  template <std::size_t N, typename T0, typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename T7, typename... Ts, EnableIf<(N >= 8 && N < 16)> = 0>
  constexpr auto impl(_size_t<N>, TypeList<T0, T1, T2, T3, T4, T5, T6, T7, Ts...>) const;

  template <std::size_t N, typename T0, typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename T7, typename T8, typename T9, typename T10, typename T11, typename T12, typename T13, typename T14, typename T15, typename... Ts, EnableIf<(N >= 16)> = 0>
  constexpr auto impl(_size_t<N>, TypeList<T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, Ts...>) const;

  template <std::size_t N, typename... Ts, EnableIf<(N > 0)> = 0>
  constexpr auto operator()(_size_t<N>, TypeList<Ts...>) const;

  template <std::size_t N, typename... Ts, EnableIf<(N <= 0)> = 0>
  constexpr TypeList<Ts...> operator()(_size_t<N>, TypeList<Ts...>) const;
};

constexpr drop_from_back_t drop_from_back{};

} // end namespace aether

#endif
