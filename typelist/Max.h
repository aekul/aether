#ifndef TYPELIST_MAX_H
#define TYPELIST_MAX_H

#include "aether/fwd/typelist/Max.h"

#include "aether/typelist/TypeList.h"
#include "aether/fwd/typelist/TypeSet.h"

namespace aether {

template <typename Max>
constexpr Max max_t::impl(Max, TypeList<>) const {
  return {};
}

template <typename Max, typename A, typename... As, EnableIf<less_than(A{}, Max{})>>
constexpr auto max_t::impl(Max, TypeList<A, As...>) const {
  return impl(Max{}, TypeList<As...>{});
}

template <typename Max, typename A, typename... As, EnableIf<!less_than(A{}, Max{})>>
constexpr auto max_t::impl(Max, TypeList<A, As...>) const {
  return impl(A{}, TypeList<As...>{});
}

template <typename A, typename... As>
constexpr auto max_t::operator()(TypeSet<A, As...>) const {
  return impl(A{}, TypeList<As...>{});
}

template <typename A, typename... As>
constexpr auto max_t::operator()(TypeList<A, As...>) const {
  return impl(A{}, TypeList<As...>{});
}

} // end namespace aether

#endif
