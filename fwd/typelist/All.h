#ifndef FWD_TYPELIST_ALL_H
#define FWD_TYPELIST_ALL_H

#include "TypeList.h"
#include "aether/fwd/TypeTraits.h"

namespace aether {

struct all_t {
  template <typename Pred, typename... Ts>
  constexpr bool operator()(Pred, TypeList<Ts...>) const;
};

constexpr all_t all{};

} // end namespace aether

#endif
