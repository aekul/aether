#ifndef TYPELIST_SLICE_H
#define TYPELIST_SLICE_H

#include "aether/fwd/typelist/Slice.h"

#include "aether/typelist/Drop.h"

namespace aether {

template <std::size_t Base, std::size_t N, typename... Ts, EnableIf<(N > 0)>>
constexpr auto slice_t::operator()(_size_t<Base> b, _size_t<N>, TypeList<Ts...> list) const {
  static_assert(Base + N <= sizeof...(Ts), "Out of bounds");
  return drop_from_back(_size_t<sizeof...(Ts) - Base - N>{}, drop_from_front(b, list));
};

template <std::size_t Base, std::size_t N, typename... Ts, EnableIf<(N <= 0)>>
constexpr EmptyList slice_t::operator()(_size_t<Base>, _size_t<N>, TypeList<Ts...>) const {
  return {};
};

template <std::size_t N, typename... Ts>
constexpr auto take_from_front_t::operator()(_size_t<N>, TypeList<Ts...>) const {
  return slice(_size_t<0>{}, _size_t<N>{}, TypeList<Ts...>{});
};


} // end namespace aether

#endif
