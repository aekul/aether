#ifndef TYPELIST_ANY_H
#define TYPELIST_ANY_H

#include "aether/fwd/typelist/Any.h"

namespace aether {

template <typename Pred, typename... Ts>
constexpr bool any_t::operator()(Pred, TypeList<Ts...>) const {
  return (Pred{}(Ts{}) || ... || false);
}

template <typename Pred, typename... Ts>
constexpr bool any_t::operator()(Pred, TypeSet<Ts...>) const {
  return (Pred{}(Ts{}) || ... || false);
}

} // end namespace aether

#endif
