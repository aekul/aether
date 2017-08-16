#ifndef FWD_TYPELIST_COUNT_H
#define FWD_TYPELIST_COUNT_H

#include "aether/fwd/TypeTraits.h"

#include "TypeList.h"

namespace aether {

struct count_t {
  template <typename Pred, typename... Ts>
  constexpr int operator()(Pred pred, TypeList<Ts...>) const;
};

constexpr count_t count{};

} // end namespace aether

#endif
