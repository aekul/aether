#ifndef FWD_TYPELIST_PREPEND_H
#define FWD_TYPELIST_PREPEND_H

#include "TypeList.h"

namespace aether {

struct prepend_t {
  template <typename T, typename... Ts>
  constexpr TypeList<T, Ts...> operator()(T, TypeList<Ts...>) const;
};

constexpr prepend_t prepend{};

} // end namespace aether

#endif
