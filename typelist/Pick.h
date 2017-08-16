#ifndef TYPELIST_PICK_H
#define TYPELIST_PICK_H

#include "aether/fwd/typelist/Pick.h"

#include "aether/typelist/TypeList.h"
#include "aether/typelist/At.h"

namespace aether {

template <std::size_t I, typename... Ts>
constexpr auto pick(TypeList<Ts...>) {
  return List(at<I>(Ts{})...);
}

} // end namespace aether

#endif
