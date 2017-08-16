#ifndef FWD_TYPELIST_REMOVE_FRONT_H
#define FWD_TYPELIST_REMOVE_FRONT_H

#include "TypeList.h"
#include "TypeSet.h"

namespace aether {

struct remove_front_t {
  template <typename T, typename... Ts>
  constexpr TypeList<Ts...> operator()(TypeList<T, Ts...>) const;

  template <typename T, typename... Ts>
  constexpr TypeSet<Ts...> operator()(TypeSet<T, Ts...>) const;
};

constexpr remove_front_t remove_front{};

} // end namespace aether

#endif
