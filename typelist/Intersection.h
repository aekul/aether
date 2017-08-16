#ifndef TYPELIST_INTERSECTION_H
#define TYPELIST_INTERSECTION_H

#include "aether/fwd/typelist/Intersection.h"

#include "aether/typelist/At.h"
#include "aether/typelist/RemoveFirst.h"
#include "aether/typelist/TypeList.h"

namespace aether {

template <typename... Bs, typename... Is>
constexpr auto intersection_t::impl(TypeList<>, TypeList<Bs...>, TypeList<Is...>) const {
  return List(TypeList<>{}, TypeList<Bs...>{}, TypeList<Is...>{});
}

template <typename A, typename... As, typename... Bs, typename... Is, EnableIf<!contains(A{}, TypeList<Bs...>{})>>
constexpr auto intersection_t::impl(TypeList<A, As...>, TypeList<Bs...>, TypeList<Is...>) const {
  return List(TypeList<A>{}.Concat(at<0>(impl(TypeList<As...>{}, TypeList<Bs...>{}, TypeList<Is...>{}))), at<1>(impl(TypeList<As...>{}, TypeList<Bs...>{}, TypeList<Is...>{})), at<2>(impl(TypeList<As...>{}, TypeList<Bs...>{}, TypeList<Is...>{})));
}

template <typename A, typename... As, typename... Bs, typename... Is, EnableIf<contains(A{}, TypeList<Bs...>{})>>
constexpr auto intersection_t::impl(TypeList<A, As...>, TypeList<Bs...>, TypeList<Is...>) const {
  return impl(
    TypeList<As...>{},
    remove_first(A{}, TypeList<Bs...>{}),
    TypeList<Is...>{}.Concat(TypeList<A>{}) 
  );
}

template <typename... As, typename... Bs>
constexpr auto intersection_t::operator()(TypeList<As...>, TypeList<Bs...>) const {
  return impl(TypeList<As...>{}, TypeList<Bs...>{}, TypeList<>{});
}
  
constexpr intersection_t intersection{};



} // end namespace aether

#endif
