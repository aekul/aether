#ifndef FWD_TYPELIST_FILL_H
#define FWD_TYPELIST_FILL_H

#include "aether/fwd/typelist/TypeList.h"
#include "aether/fwd/TypeTraits.h"

namespace aether {

struct fill_t {
  template <typename... Ts, typename V>
  constexpr auto impl(_size_t<1>, TypeList<Ts...>, V) const;

  template <std::size_t N, typename... Ts, typename V, EnableIf<(N <= 0)> = 0>
  constexpr auto impl(_size_t<N>, TypeList<Ts...>, V) const;

  template <std::size_t N, typename... Ts, typename V, EnableIf<(N > 0)> = 0>
  constexpr auto impl(_size_t<N>, TypeList<Ts...>, V) const;

  template <std::size_t N, typename V>
  constexpr auto operator()(_size_t<N>, V) const;
};

} // end namespace aether

#endif
