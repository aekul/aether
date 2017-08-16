#ifndef FWD_TYPELIST_CHAIN_H
#define FWD_TYPELIST_CHAIN_H

#include "TypeList.h"

namespace aether {

struct chain_t {
  template <typename Arg, typename... StaticArgs>
  constexpr auto impl(TypeList<>, Arg, StaticArgs...) const;

  template <typename Fn, typename... Fns, typename Arg, typename... StaticArgs>
  constexpr auto impl(TypeList<Fn, Fns...>, Arg, StaticArgs...) const;

  template <typename... Fns, typename Arg, typename... StaticArgs>
  constexpr auto operator()(TypeList<Fns...>, Arg, StaticArgs...) const;
};

constexpr chain_t chain{};

} // end namespace aether

#endif
