#ifndef FWD_TYPELIST_REMOVE_AT_H
#define FWD_TYPELIST_REMOVE_AT_H

#include "aether/fwd/TypeTraits.h"
#include "aether/fwd/typelist/TypeList.h"

namespace aether {

struct remove_at_t {
  template <std::size_t Base, std::size_t N, typename... Ts>
  constexpr auto operator()(_size_t<Base>, _size_t<N>, TypeList<Ts...>) const;

  template <std::size_t I, typename... Ts>
  constexpr auto operator()(_size_t<I>, TypeList<Ts...>) const;
};

constexpr remove_at_t remove_at{};

} // end namespace aether

#endif
