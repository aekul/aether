#ifndef FWD_TYPELIST_GROUP_BY_KEY_H
#define FWD_TYPELIST_GROUP_BY_KEY_H

#include "TypeList.h"

namespace aether {

struct group_by_key_t {
  template <typename... Rs>
  constexpr auto impl(TypeList<Rs...>, TypeList<>) const;

  template <typename... Rs, typename K, typename V, typename... Ts>
  constexpr auto impl(TypeList<Rs...>, TypeList<TypeList<K, V>, Ts...>) const;

  template <typename... Ts>
  constexpr auto operator()(TypeList<Ts...>) const;
};

constexpr group_by_key_t group_by_key{};

} // end namespace aether

#endif
