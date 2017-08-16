#ifndef TYPELIST_MIN_H
#define TYPELIST_MIN_H

#include "aether/fwd/typelist/Min.h"

namespace aether {

template <typename Min>
constexpr Min min_t::impl(Min, TypeList<>) const {
  return {};
}

template <typename Min, typename A, typename... As, EnableIf<!less_than(A{}, Min{})>>
constexpr auto min_t::impl(Min, TypeList<A, As...>) const {
  return impl(Min{}, TypeList<As...>{});
}

template <typename Min, typename A, typename... As, EnableIf<less_than(A{}, Min{})>>
constexpr auto min_t::impl(Min, TypeList<A, As...>) const {
  return impl(A{}, TypeList<As...>{});
}

template <typename A, typename... As>
constexpr auto min_t::operator()(TypeSet<A, As...>) const {
  return impl(A{}, TypeList<As...>{});
}

template <typename A, typename... As>
constexpr auto min_t::operator()(TypeList<A, As...>) const {
  return impl(A{}, TypeList<As...>{});
}

} // end namespace aether

#endif
