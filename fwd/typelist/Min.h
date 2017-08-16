#ifndef FWD_TYPELIST_MIN_H
#define FWD_TYPELIST_MIN_H

#include "TypeList.h"
#include "TypeSet.h"

namespace aether {

struct min_t {
  template <typename Min>
  constexpr Min impl(Min, TypeList<>) const;

  template <typename Min, typename A, typename... As, EnableIf<!less_than(A{}, Min{})> = 0>
  constexpr auto impl(Min, TypeList<A, As...>) const;

  template <typename Min, typename A, typename... As, EnableIf<less_than(A{}, Min{})> = 0>
  constexpr auto impl(Min, TypeList<A, As...>) const;

  template <typename A, typename... As>
  constexpr auto operator()(TypeSet<A, As...>) const;

  template <typename A, typename... As>
  constexpr auto operator()(TypeList<A, As...>) const;
};

constexpr min_t min{};

} // end namespace aether

#endif
