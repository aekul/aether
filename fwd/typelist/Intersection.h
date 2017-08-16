#ifndef FWD_TYPELIST_INTERSECTION_H
#define FWD_TYPELIST_INTERSECTION_H

#include "aether/fwd/typelist/TypeList.h"
#include "aether/Int.h"
#include "aether/typelist/Contains.h"

namespace aether {

struct intersection_t {
  template <typename... Bs, typename... Is>
  constexpr auto impl(TypeList<>, TypeList<Bs...>, TypeList<Is...>) const;

  template <typename A, typename... As, typename... Bs, typename... Is, EnableIf<!contains(A{}, TypeList<Bs...>{})> = 0>
  constexpr auto impl(TypeList<A, As...>, TypeList<Bs...>, TypeList<Is...>) const;

  template <typename A, typename... As, typename... Bs, typename... Is, EnableIf<contains(A{}, TypeList<Bs...>{})> = 0>
  constexpr auto impl(TypeList<A, As...>, TypeList<Bs...>, TypeList<Is...>) const;

  template <typename... As, typename... Bs>
  constexpr auto operator()(TypeList<As...>, TypeList<Bs...>) const;
};

} // end namespace aether

#endif
