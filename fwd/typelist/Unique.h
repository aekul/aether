#ifndef FWD_TYPELIST_UNIQUE_H
#define FWD_TYPELIST_UNIQUE_H

#include "TypeList.h"
#include "TypeSet.h"

namespace aether {

struct unique_t {
  template <typename... Rs>
  constexpr auto impl(TypeList<>, TypeSet<Rs...>) const;
  
  template <typename T, typename... Ts, typename... Rs>
  constexpr auto impl(TypeList<T, Ts...>, TypeSet<Rs...>) const;

  template <typename... Ts>
  constexpr auto operator()(TypeList<Ts...>) const;
};

constexpr unique_t unique{};

} // end namespace aether

#endif
