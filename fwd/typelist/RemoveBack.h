#ifndef FWD_TYPELIST_REMOVE_BACK_H
#define FWD_TYPELIST_REMOVE_BACK_H

#include "TypeList.h"
#include "TypeSet.h"

namespace aether {

struct remove_back_t {
  template <typename... Ts>
  constexpr auto operator()(TypeList<Ts...>) const;

  template <typename... Ts>
  constexpr auto operator()(TypeSet<Ts...>) const;
};

constexpr remove_back_t remove_back{};

} // end namespace aether

#endif
