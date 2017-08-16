#ifndef TYPELIST_TRANSFORM_H
#define TYPELIST_TRANSFORM_H

#include "aether/fwd/typelist/Transform.h"

namespace aether {

template <typename Fn, typename... Ts>
constexpr auto transform_t::operator()(const Fn& fn, TypeList<Ts...>) const {
  return List(fn(Ts{})...);
}

} // end namespace aether

#endif
