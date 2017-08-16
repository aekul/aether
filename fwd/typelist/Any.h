#ifndef FWD_TYPELIST_ANY_H
#define FWD_TYPELIST_ANY_H

#include "TypeList.h"
#include "TypeSet.h"

namespace aether {

struct any_t {
  template <typename Pred, typename... Ts>
  constexpr bool operator()(Pred pred, TypeList<Ts...>) const;

  template <typename Pred, typename... Ts>
  constexpr bool operator()(Pred pred, TypeSet<Ts...>) const;
};

constexpr any_t any{};

} // end namespace aether

#endif
