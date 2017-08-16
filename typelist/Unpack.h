#ifndef UNPACK_H
#define UNPACK_H

#include "aether/fwd/typelist/TypeList.h"

namespace aether {

template <template <typename...> typename T, typename... Ts>
constexpr T<Ts...> unpack(Ts...) {
  return {};
}

template <template <typename...> typename T, typename... Ts>
constexpr T<Ts...> unpack_list(TypeList<Ts...>) {
  return {};
}

} // end namespace aether

#endif
