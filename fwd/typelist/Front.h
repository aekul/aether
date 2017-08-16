#ifndef FWD_TYPELIST_FRONT_H
#define FWD_TYPELIST_FRONT_H

#include "TypeList.h"
#include "TypeSet.h"

namespace aether {

struct front_t {
  template <typename T, typename... Ts>
  constexpr T operator()(TypeList<T, Ts...>) const;

  template <typename T, typename... Ts>
  constexpr T operator()(TypeSet<T, Ts...>) const;
};

constexpr front_t front{};

} // end namespace aether

#endif
