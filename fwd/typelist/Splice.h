#ifndef FWD_TYPELIST_SPLICE_H
#define FWD_TYPELIST_SPLICE_H

#include <utility>

#include "TypeList.h"

namespace aether {

template <std::size_t Base, std::size_t N>
struct splice_t {
  template <typename... Ts, std::size_t... Is>
  constexpr auto impl(TypeList<Ts...>, std::index_sequence<Is...>) const;

  template <typename... Ts, typename R>
  constexpr auto operator()(TypeList<Ts...>, R) const;
};

template <std::size_t Base, std::size_t N>
constexpr splice_t<Base, N> splice{};

} // end namespace aether

#endif
