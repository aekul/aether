#ifndef FWD_TYPELIST_REMOVE_FIRST_H
#define FWD_TYPELIST_REMOVE_FIRST_H

#include "TypeList.h"

namespace aether {

struct remove_first_t {
  template <typename T, typename... Ts>
  constexpr auto operator()(T, TypeList<Ts...>) const;
};

constexpr remove_first_t remove_first{};

} // end namespace aether

#endif
