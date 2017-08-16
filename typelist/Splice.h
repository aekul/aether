#ifndef TYPELIST_SPLICE_H
#define TYPELIST_SPLICE_H

#include "aether/fwd/typelist/Splice.h"

#include "aether/typelist/At.h"
#include "aether/typelist/Concat.h"
#include "aether/typelist/Slice.h"

namespace aether {

template <std::size_t Base, std::size_t N>
template <typename... Ts, std::size_t... Is>
constexpr auto splice_t<Base, N>::impl(TypeList<Ts...>, std::index_sequence<Is...>) const {
  return List(at<Base + Is>(TypeList<Ts...>{})...);
};

template <std::size_t Base, std::size_t N>
template <typename... Ts, typename R>
constexpr auto splice_t<Base, N>::operator()(TypeList<Ts...>, R) const {
  static_assert(Base + N <= sizeof...(Ts), "Out of bounds");
  return concat(
    slice<0, Base>(TypeList<Ts...>{})
    , List(R{})
    , slice<Base + N, sizeof...(Ts) - Base - N>(TypeList<Ts...>{})
  );
};

} // end namespace aether

#endif
