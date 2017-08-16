#ifndef FWD_TYPELIST_TRANSFORM_H
#define FWD_TYPELIST_TRANSFORM_H

#include "TypeList.h"
#include "TypeSet.h"

namespace aether {

struct transform_t {
  template <typename Fn, typename... Ts>
  constexpr auto operator()(const Fn& fn, TypeList<Ts...>) const;
};

constexpr transform_t transform{};

} // end namespace aether

#endif
