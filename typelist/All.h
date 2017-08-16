#ifndef TYPELIST_ALL_H
#define TYPELIST_ALL_H

#include "aether/fwd/typelist/All.h"

namespace aether {

template <typename Pred, typename... Ts>
constexpr bool all_t::operator()(Pred, TypeList<Ts...>) const {
  return (Pred{}(Ts{}) && ... && true);
}

} // end namespace aether

#endif
