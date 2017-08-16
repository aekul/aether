#ifndef FWD_TYPELIST_SUM_H
#define FWD_TYPELIST_SUM_H

#include "TypeList.h"

namespace aether {

struct sum_t {
  template <typename T>
  constexpr T impl(TypeList<T>) const;

  template <typename A, typename B, typename... Ts>
  constexpr auto impl(TypeList<A, B, Ts...>) const;

  template <typename... Ts>
  constexpr auto operator()(TypeList<Ts...>) const;

  template <typename... Ts>
  constexpr auto operator()(Ts...) const;
};

constexpr sum_t sum{};

} // end namespace aether

#endif
