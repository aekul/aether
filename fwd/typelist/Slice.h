#ifndef FWD_TYPELIST_SLICE_H
#define FWD_TYPELIST_SLICE_H

#include <utility>

#include "aether/fwd/typelist/TypeList.h"
#include "aether/fwd/TypeTraits.h"

namespace aether {

struct slice_t {
  template <std::size_t Base, std::size_t N, typename... Ts, EnableIf<(N > 0)> = 0>
  constexpr auto operator()(_size_t<Base>, _size_t<N>, TypeList<Ts...>) const;

  template <std::size_t Base, std::size_t N, typename... Ts, EnableIf<(N <= 0)> = 0>
  constexpr EmptyList operator()(_size_t<Base>, _size_t<N>, TypeList<Ts...>) const;
};

constexpr slice_t slice{};

struct take_from_front_t {
  template <std::size_t N, typename... Ts>
  constexpr auto operator()(_size_t<N>, TypeList<Ts...>) const;
};

constexpr take_from_front_t take_from_front{};

} // end namespace aether

#endif
