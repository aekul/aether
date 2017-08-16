#ifndef TYPELIST_GROUP_BY_KEY_H
#define TYPELIST_GROUP_BY_KEY_H

#include "aether/fwd/typelist/GroupByKey.h"

#include "aether/typelist/Match.h"
#include "aether/typelist/Partition.h"
#include "aether/typelist/Pick.h"

namespace aether {

template <typename... Rs>
constexpr auto group_by_key_t::impl(TypeList<Rs...>, TypeList<>) const {
  return TypeList<Rs...>{};
}

template <typename... Rs, typename K, typename V, typename... Ts>
constexpr auto group_by_key_t::impl(TypeList<Rs...>, TypeList<TypeList<K, V>, Ts...>) const {
  constexpr auto parts = partition(match<TypeList<K, wildcard_t>>{}, List(Ts{}...));
  constexpr auto values = pick<1>(at<0>(parts)).Concat(V{});
  return impl(append(TypeList<Rs...>{}, List(K{}, values)), at<1>(parts));
}

template <typename... Ts>
constexpr auto group_by_key_t::operator()(TypeList<Ts...>) const {
  return impl(List(), TypeList<Ts...>{}); 
}

} // end namespace aether

#endif
