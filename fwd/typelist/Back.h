#ifndef FWD_TYPELIST_BACK_H
#define FWD_TYPELIST_BACK_H

#include "TypeList.h"
#include "TypeSet.h"

namespace aether {

struct back_t {
  template <typename... Ts>
  constexpr auto operator()(TypeList<Ts...>) const;

  template <typename... Ts>
  constexpr auto operator()(TypeSet<Ts...>) const;
};

constexpr back_t back{};

} // end namespace aether

#endif
