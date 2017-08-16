#ifndef TYPELIST_CHAIN_H
#define TYPELIST_CHAIN_H

#include "../fwd/typelist/Chain.h"

namespace aether {

template <typename Arg, typename... StaticArgs>
constexpr auto chain_t::impl(TypeList<>, Arg, StaticArgs...) const {
  return Arg{};
}

template <typename Fn, typename... Fns, typename Arg, typename... StaticArgs>
constexpr auto chain_t::impl(TypeList<Fn, Fns...>, Arg, StaticArgs...) const {
  return impl(TypeList<Fns...>{}, Fn{}(Arg{}, StaticArgs{}...), StaticArgs{}...);
}

template <typename... Fns, typename Arg, typename... StaticArgs>
constexpr auto chain_t::operator()(TypeList<Fns...>, Arg, StaticArgs...) const {
  return impl(TypeList<Fns...>{}, Arg{}, StaticArgs{}...);
}

} // end namespace aether

#endif
