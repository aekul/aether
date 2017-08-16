#ifndef TYPELIST_FILL_H
#define TYPELIST_FILL_H

#include "aether/fwd/typelist/Fill.h"

namespace aether {

template <typename... Ts, typename V>
constexpr auto fill_t::impl(_size_t<1>, TypeList<Ts...>, V) const {
  return TypeList<Ts..., V>{};
};

template <std::size_t N, typename... Ts, typename V, EnableIf<(N <= 0)>>
constexpr auto fill_t::impl(_size_t<N>, TypeList<Ts...>, V) const {
  return TypeList<Ts...>{};
};

template <std::size_t N, typename... Ts, typename V, EnableIf<(N > 0)>>
constexpr auto fill_t::impl(_size_t<N>, TypeList<Ts...>, V) const {
  auto half = impl(_size_t<N / 2>{}, TypeList<Ts...>{}, V{});
  auto last = impl(_size_t<N % 2>{}, TypeList<Ts...>{}, V{});
  return half.Concat(half).Concat(last);
};

template <std::size_t N, typename V>
constexpr auto fill_t::operator()(_size_t<N>, V) const {
  return impl(_size_t<N>{}, List(), V{});
};

constexpr fill_t fill{};

} // end namespace aether

#endif
