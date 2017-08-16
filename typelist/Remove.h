#ifndef TYPELIST_REMOVE_AT_H
#define TYPELIST_REMOVE_AT_H

#include "aether/fwd/typelist/Remove.h"

#include "aether/typelist/Concat.h"
#include "aether/typelist/Slice.h"

namespace aether {

template <std::size_t Base, std::size_t N, typename... Ts>
constexpr auto remove_at_t::operator()(_size_t<Base>, _size_t<N>, TypeList<Ts...>) const {
  static_assert(Base + N <= sizeof...(Ts), "Out of bounds");
  return concat(
    slice(_size_t<0>{}, _size_t<Base>{}, TypeList<Ts...>{})
    , slice(_size_t<Base + N>{}, _size_t<sizeof...(Ts) - Base - N>{}, TypeList<Ts...>{})
  );
};

template <std::size_t I, typename... Ts>
constexpr auto remove_at_t::operator()(_size_t<I>, TypeList<Ts...>) const {
  return this->operator()(_size_t<I>{}, _size_t<1>{}, TypeList<Ts...>{});
};

} // end namespace aether

#endif
