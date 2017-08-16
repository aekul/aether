#ifndef FWD_TYPELIST_CONTAINS_H
#define FWD_TYPELIST_CONTAINS_H

#include "aether/fwd/typelist/TypeList.h"
#include "aether/fwd/typelist/TypeSet.h"

namespace aether {

struct contains_t {
  template <typename R, typename... Ts>
  constexpr bool operator()(R, TypeList<Ts...>) const;

  template <typename R, typename... Ts>
  constexpr bool operator()(R, TypeSet<Ts...>) const;

  template <typename R, typename... Ts>
  constexpr bool operator()(Type<R>, TypeSet<Ts...>) const;
};

constexpr contains_t contains{};

} // end namespace aether

#endif
