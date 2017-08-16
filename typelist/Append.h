#ifndef TYPELIST_APPEND_H
#define TYPELIST_APPEND_H

#include "../fwd/typelist/Append.h"

namespace aether {

template <typename... Ts, typename T>
constexpr TypeList<Ts..., T> append_t::operator()(TypeList<Ts...>, T) const {
  return {};
}

} // end namespace aether

#endif
