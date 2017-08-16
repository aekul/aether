#ifndef TYPELIST_PREPEND_H
#define TYPELIST_PREPEND_H

#include "../fwd/typelist/Prepend.h"

namespace aether {

template <typename T, typename... Ts>
constexpr TypeList<T, Ts...> prepend_t::operator()(T, TypeList<Ts...>) const {
  return {};
}

} // end namespace aether

#endif
