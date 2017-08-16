#ifndef FWD_TYPELIST_MAX_H
#define FWD_TYPELIST_MAX_H

#include "TypeList.h"
#include "TypeSet.h"

namespace aether {

struct max_t {
  template <typename Max>
  constexpr Max impl(Max, TypeList<>) const;

  template <typename Max, typename A, typename... As, EnableIf<less_than(A{}, Max{})> = 0>
  constexpr auto impl(Max, TypeList<A, As...>) const;

  template <typename Max, typename A, typename... As, EnableIf<!less_than(A{}, Max{})> = 0>
  constexpr auto impl(Max, TypeList<A, As...>) const;

  template <typename A, typename... As>
  constexpr auto operator()(TypeSet<A, As...>) const;

  template <typename A, typename... As>
  constexpr auto operator()(TypeList<A, As...>) const;
};

constexpr max_t max{};

} // end namespace aether

#endif
