#ifndef FWD_TYPELIST_FIND_H
#define FWD_TYPELIST_FIND_H

#include "../TypeTraits.h"

#include "TypeList.h"
#include "TypeSet.h"

namespace aether {

struct find_t {
  template <typename Pred>
  constexpr auto impl(Pred, TypeList<>) const;

  template <typename Pred, typename T, typename... Ts, EnableIf<!Pred{}(T{})> = 0>
  constexpr auto impl(Pred pred, TypeList<T, Ts...>) const;

  template <typename Pred, typename T, typename... Ts, EnableIf<Pred{}(T{})> = 0>
  constexpr T impl(Pred pred, TypeList<T, Ts...>) const;

  template <typename Pred, typename... Ts>
  constexpr auto operator()(Pred pred, TypeList<Ts...>) const;

  template <typename Pred, typename... Ts>
  constexpr auto operator()(Pred pred, TypeSet<Ts...>) const;
};

constexpr find_t find{};

} // end namespace aether

#endif
